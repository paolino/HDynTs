{-# language ViewPatterns #-}
{-# language TypeFamilies #-}
{-# language FlexibleInstances #-}
{-# language MultiParamTypeClasses#-}
{-# language DataKinds#-}
{-# language GADTs#-}
{-# language StandaloneDeriving#-}
{-# language DeriveFoldable#-}
{-# language RankNTypes #-}
{-# language ScopedTypeVariables #-}
{-# language TemplateHaskell #-}
{-# language FlexibleContexts #-}
-- {-# #-}
import Data.FingerTree (FingerTree, split, Measured(..), ViewL(..), 
    (<|), viewl, fromList)
import qualified Data.FingerTree as F
import Data.Set (Set,member)
import qualified Data.Set as S
import Data.Monoid
import Control.Arrow
import Data.Maybe
import Data.Tree
import Data.List (sortBy, partition)
import Data.Ord (comparing)
import Data.Foldable
import Control.Lens.TH (makeLenses)
import Control.Lens (over,set,view)
import Control.Monad.State (evalState, put, gets , State)
import Control.Monad (forM)

    
type Depends = Maybe
type Belongs = Set 
type Pos = Sum Int

pick :: Measured v a 
    => (v -> Bool) 
    -> FingerTree v a 
    -> Maybe (a, Maybe a -> FingerTree v a)
pick c f = case F.split c f of
    (b1,viewl -> x :< b2) -> Just (x,\r -> b1 <> (maybe id (<|) r) b2)
    (b1,viewl -> EmptyL) -> Nothing

unsafePick c = fromJust . pick c



newtype Elem a = Elem a deriving Show

fromElem (Elem x) = x

type LMonoid a = (Belongs a, Pos) 

instance Ord a => Measured (LMonoid a) (Elem a) where
    measure (Elem x) = (S.singleton x, Sum 1)

type Seq a = FingerTree (LMonoid a) (Elem a)

data SeqR a = SeqR (Seq a) (Seq a) deriving (Show)

instance Foldable SeqR   where
    foldr f x (SeqR a b) = foldr (\(Elem x) -> f x) x a

seqR [] = SeqR mempty mempty
seqR (x:xs) = SeqR (F.singleton $ Elem x) (F.singleton $ Elem x) <> seqR xs
        

swap (SeqR x y) = SeqR y x
nullSeq (SeqR x _) = F.null x

-- splitting a SeqR
cutSeqR :: Ord a => a -> SeqR a -> (SeqR a, SeqR a)
cutSeqR x (SeqR p n) = let 
    (p',p'') = F.split (member x . fst) p
    (_,k) = measure p''
    (n',n'') = F.split ((<) k. snd) n
    in (SeqR p' n'', SeqR p'' n')

-- the monoid respect the inversions
instance Ord a => Monoid (SeqR a) where
    SeqR x xR `mappend` SeqR y yR = SeqR (x <> y) (yR <> xR)
    mempty = SeqR mempty mempty

-- a path as product of its dependency / Nothing as root
-- a seq of the nodes going up, same sequence reverted

data CheckRoot = IsRoot | NotRoot

data Path (t:: CheckRoot) a where
     Path :: a -> SeqR a -> Path NotRoot a
     Root :: SeqR a -> Path IsRoot a

deriving instance Show a => Show (Path r a)
-- can only rotate a root path
rotate  :: Ord a 
        => a 
        -> Path IsRoot a 
        -> (Path IsRoot a, Maybe (Path NotRoot a))
rotate x (Root p) = let
    (p1,swap -> p2) = cutSeqR x p 
    in (Root p2, Path x <$> case nullSeq p1 of
            True -> Nothing
            False -> Just p1)

type HMonoid a = (Belongs a, Belongs a)



-- given first path goes into second, switch the front
-- ABCDEF -> RSTFBB -> (ABCDEFBB,RST)
cross   :: Ord a 
        => Path NotRoot a 
        -> Path r a 
        -> (Path r a, Path NotRoot a)
cross (Path f1 p1) (Path f2 p2) = let
    (p21, p22) = cutSeqR f1 p2
    in (Path f2 $ p1 <> p2, Path f1 p21)
cross (Path f1 p1) (Root p2) = let
    (p21, p22) = cutSeqR f1 p2
    in (Root $ p1 <> p22, Path f1 p21)


data APath a where 
    APath :: Path r a -> APath a

biAPath = APath *** APath
deriving instance Show a => Show (APath a)

type Algo a = FingerTree (Belongs a, Belongs a) (APath a)

instance Ord a => Measured (HMonoid a) (APath a) where
    measure (APath (Path x  (SeqR b _))) = (fst $ measure b, S.singleton x)
    measure (APath (Root (SeqR b _))) = (fst $ measure b, mempty)


-- bring a to be part of the path containing the root
-- Nothing means a  was not found , free catch
reroot :: Ord a => (Path IsRoot a -> Path r a) -> a ->  Algo a -> Algo a
reroot mt x = uncurry expose . second ($Nothing) . unsafePick (member x. fst) where
    expose (APath r@(Root p)) t = case rotate x r of
        (root, Nothing) -> invariant (APath $ mt root) t
        (root, Just another) -> invariant (APath $ mt root) $  APath another <| t
    expose (APath p@(Path f _)) t = let 
        ((p',q'),qs) = case unsafePick (member f . fst) $ t of
            (APath q, qs) -> (biAPath $ cross p q,qs)
        in expose p' . qs . Just $ q'



invariant :: forall a . Ord a => APath  a -> Algo a -> Algo a
invariant r = case r of 
    APath (Root s) -> invariantCore s Root
    APath (Path f s) -> invariantCore s (Path f)
    where
    invariantCore 
            :: SeqR a 
            -> (SeqR a -> Path r a) 
            -> Algo a 
            -> Algo a
    invariantCore s@(SeqR (viewl -> Elem x :<_) _) f t = 
        case pick (member x . snd) t of
            Nothing -> r <| t
            Just (APath (Path _ p),ps) -> ps . Just . APath . f $ p <> s

link :: Ord a => a -> a -> Algo a -> Algo a 
link x y t = let 
    t' = reroot (\(Root s) -> Path y s) x t
    (p,ps) = unsafePick (member y . fst) t'
    in invariant p $ ps Nothing

cut :: forall a .  Ord a =>  a -> Algo a -> Algo a
cut x t = let 
    (p,ps) = unsafePick (member x . fst) t
    (p1,p2) = case p of
         APath (Root s) -> biAPath $ cutCore  s Root
         APath (Path f s) -> biAPath $ cutCore  s (Path f)
    cutCore 
        :: SeqR a 
        -> (SeqR a -> Path r a) 
        -> (Path IsRoot a, Path r a)
    cutCore s f = let
        (swap -> p1,swap -> p2) = cutSeqR x (swap s)
        in (Root p2, f p1)
    in invariant p2 (ps . Just $ p1)


data P a = P {
    _father :: Maybe a,
    _path :: [a]
    }

makeLenses ''P

pathsCore :: Tree a -> [P a]
pathsCore (Node x []) = [P Nothing [x]]
pathsCore (Node x xs) = (over path (x:) p) : map (set father $ Just x) ps where
    p:ps = sortBy (comparing $ length . view path) $ xs >>= pathsCore

pathsOne :: Ord a => Tree a -> Algo a
pathsOne t = let
    (P Nothing p):ps = pathsCore t
    in fromList $ APath (Root $ seqR p): map (\(P (Just f) p) -> APath (Path f $ seqR p)) ps 
     
fromForest :: Ord a => [Tree a] -> Algo a
fromForest  = mconcat . map pathsOne

toPaths :: forall a. Algo a -> [P a]
toPaths = map f . toList where
    f :: APath a -> P a
    f (APath (Root p)) = P Nothing (toList p)
    f (APath (Path f p)) = P (Just f) (toList p)
    

toTree :: Eq a => [a] -> State [P a] (Tree a)
toTree [x] = return $ Node x []
toTree (x:xs) = do
    let match Nothing = False
        match (Just y) = y == x
    (cs,rs) <- gets $ partition (match . view father)
    put rs
    cs' <- mapM (toTree . view path) cs
    Node x <$> (:cs') <$> toTree xs

toForest :: Eq a => Algo a -> [Tree a] 
toForest t = flip evalState (toPaths t) $ do
    (as, rs) <- gets $ partition (isNothing . view father) 
    put rs
    forM as $ \(P Nothing xs) -> toTree xs
            
            
{-
cutCore
  :: Ord a =>
     a -> SeqR a 
     -> (SeqR a -> Path r a) 
     -> (Path IsRoot a, Path r a)
-}

{-
-}
    

