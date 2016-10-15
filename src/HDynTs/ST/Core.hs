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

module HDynTs.ST.Core where

import Data.FingerTree (FingerTree, Measured(..), ViewL(..),ViewR (..), 
    (<|), viewl, viewr, fromList)
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
import Control.Monad.State (evalState, put, gets , State)
import Control.Monad (forM)
import GHC.Exts (Constraint)
import HDynTs.Lib.Access
import HDynTs.Lib.FingerTree


-- | monoid for inter path dependency
type Depends = Maybe

-- | monoid for vertex presence 
type Belongs = Set 

-- | monoid for position in path 
type Pos = Sum Int

-- | values supporting Measured SequenceMonoid, must be orderable
newtype Elem a = Elem a deriving Show

-- | the monoid for a Sequence, tracking membership and position 
type SequenceMonoid a = (Belongs a, Pos) 

instance Ord a => Measured (SequenceMonoid a) (Elem a) where
    measure (Elem x) = (S.singleton x, Sum 1)

-- | A 'Sequence' is a FingerTree of 'Elem'
type Sequence a = FingerTree (SequenceMonoid a) (Elem a)

-- | A composition of 2 reversed sequences
data Reversable a = Reversable (Sequence a) (Sequence a) deriving (Show)

instance Foldable Reversable   where
    foldr f x (Reversable a _) = foldr (\(Elem x) -> f x) x a

-- | build a 'Reversable'
reversable [] = Reversable mempty mempty
reversable (x:xs) = Reversable (F.singleton $ Elem x) (F.singleton $ Elem x) <> reversable xs
        
-- | swap a reversable
swap (Reversable x y) = Reversable y x

instance Ord a => Split Reversable a where
    type SplitP Reversable a = a
    split x (Reversable p n) = let 
        (p',p'') = F.split (member x . fst) p
        (_,k) = measure p''
        (n',n'') = F.split ((<) k. snd) n
        in (Reversable p' n'', Reversable p'' n')

-- the monoid respect the inversions
instance Ord a => Monoid (Reversable a) where
    Reversable x xR `mappend` Reversable y yR = Reversable (x <> y) (yR <> xR)
    mempty = Reversable mempty mempty

-- a path as product of its dependency / Nothing as root
-- a sequence of the nodes going up, same sequenceuence reverted

-- | differentiate Root paths from the others TL
data CheckRoot = IsRoot | NotRoot

-- | A Path can be a 'Root' and it contains a root, a 'Path' when it 
-- flows into another one 'Depends'
data Path (t :: CheckRoot) a where
     Path :: a -> Reversable a -> Path NotRoot a
     Root :: Reversable a -> Path IsRoot a

--debug
deriving instance Show a => Show (Path r a)

-- | Rotate a 'Path' and can only rotate a root paths.
rotate  :: Ord a 
        => a -- ^ new root
        -> Path IsRoot a  -- ^ a root path
        -> (Path IsRoot a, Maybe (Path NotRoot a)) -- ^ the new root path with the cutted, reversed rest
rotate x (Root p) = let
    (p1,swap -> p2) = split x p 
    in (Root p2, Path x <$> case null p1 of
            True -> Nothing
            False -> Just p1)

-- | Monoid for PathForest. It tracks memberships and dependency
type PathForestMonoid a = (Belongs a, Belongs a)

-- | given first path goes into second, switch the front
-- | ABCDEF -> RSTFBB -> (ABCDEFBB,RST)
-- can we enforce by type the first path depends on the second ?
cross   :: Ord a 
        => Path NotRoot a 
        -> Path r a 
        -> (Path r a, Path NotRoot a)
cross (Path f1 p1) (Path f2 p2) = let
    (p21, p22) = split f1 p2
    in (Path f2 $ p1 <> p2, Path f1 p21)
cross (Path f1 p1) (Root p2) = let
    (p21, p22) = split f1 p2
    in (Root $ p1 <> p22, Path f1 p21)

-- | Box the difference between an 'IsRoot' and an 'NotRoot'
data APath a where 
    APath :: Path r a -> APath a

-- it happens more than once
biAPath :: (Path r a, Path r1 a1) -> (APath a, APath a1)
biAPath = APath *** APath

-- debug
deriving instance Show a => Show (APath a)

-- | PathForest is the FingerTree of the 'Path's
type PathForest a = FingerTree (PathForestMonoid a) (APath a)

instance Ord a => Measured (PathForestMonoid a) (APath a) where
    measure (APath (Path x  (Reversable b _))) 
        = (fst $ measure b, S.singleton x)
    measure (APath (Root (Reversable b _))) 
        = (fst $ measure b, mempty)

-- | testing membership on the 'PathForestMonoid'
memberPathForest :: Ord a => a -> PathForestMonoid a -> Bool
memberPathForest x = member x . fst

-- | bring the vertex to be part of the path containing the root and
-- make it the root
-- Nothing means the vertex was not found , free catch
reroot  :: forall a r . Ord a 
         => (Path IsRoot a -> Path r a) -- ^ give a CheckRoot to a Path
         -> a  -- ^ vertex
         -> PathForest a -- ^ original forest
         -> PathForest a

reroot mt x = uncurry expose . second ($Nothing) . unsafeAdjust (memberPathForest x) where
    expose (APath r@(Root p)) t = case rotate x r of
        (root, Nothing) -> invariant (APath $ mt root) t
        (root, Just another) -> 
            invariant (APath $ mt root) $  APath another <| t
    expose (APath p@(Path f _)) t = let 
        ((p',q'),qs) = case unsafeAdjust (memberPathForest f) $ t of
            (APath q, qs) -> (biAPath $ cross p q,qs)
        in expose p' . qs . Just $ q'


-- | force insertion under invariant on "All paths end in a leave"
invariant :: forall a . Ord a => APath  a -> PathForest a -> PathForest a
invariant r = case r of 
    APath (Root s) -> invariantCore s Root
    APath (Path f s) -> invariantCore s (Path f)
    where
    invariantCore 
            :: Reversable a 
            -> (Reversable a -> Path r a) 
            -> PathForest a 
            -> PathForest a
    invariantCore s@(Reversable (viewl -> Elem x :<_) _) f t = 
        case adjust (member x . snd) t of
            Nothing -> r <| t
            Just (APath (Path _ p),ps) -> ps . Just . APath . f $ p <> s

-- | find the top of a tree given an elem, jumping through deps
top :: Ord a => a -> PathForest a -> Maybe a
top x t = adjust (memberPathForest x) t >>= f where
    f (APath (Root (Reversable (viewr -> _ :> Elem r) _)) ,_) = Just r
    f (APath (Path p _),_) = top p t
   
-- | unsafe link
link :: Ord a => a -> a -> PathForest a -> PathForest a 
link x y t = let 
    t' = reroot (\(Root s) -> Path y s) x t
    (p,ps) = unsafeAdjust (memberPathForest y) t'
    in invariant p $ ps Nothing

-- | unsafe cut
cut :: forall a .  Ord a =>  a -> PathForest a -> PathForest a
cut x t = let 
    (p,ps) = unsafeAdjust (memberPathForest x) t
    (p1,p2) = case p of
         APath (Root s) -> biAPath $ cutCore  s Root
         APath (Path f s) -> biAPath $ cutCore  s (Path f)
    cutCore 
        :: Reversable a 
        -> (Reversable a -> Path r a) 
        -> (Path IsRoot a, Path r a)
    cutCore s f = let
        (swap -> p1,swap -> p2) = split x (swap s)
        in (Root p2, f p1)
    in invariant p2 (ps . Just $ p1)


