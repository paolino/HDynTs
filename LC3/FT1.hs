{-# language MultiParamTypeClasses, TemplateHaskell, ScopedTypeVariables, 
    ViewPatterns, FlexibleInstances,DeriveFunctor, StandaloneDeriving, 
    NoMonomorphismRestriction, FlexibleContexts #-}

module FT1 where

import qualified Data.FingerTree as F
import  Data.FingerTree (FingerTree, split, measure, Measured, viewl,viewr,  
    (<|) , (|>), ViewL ((:<)), ViewR ((:>)))
import qualified Data.Set as S
import Data.IntMap (IntMap)
import Data.Set (Set)
import Data.List
import Control.Arrow ((&&&), (***), second, first )
import Data.Maybe (isNothing, fromJust )
import Control.Lens hiding ((:<),(<|),(|>),(:>), children)
import Control.Lens.TH
import Data.Ord
import Data.Tree hiding (Forest)
import Data.Monoid
import Data.Maybe
import Data.Foldable
import Control.Monad.State

--------------------------------------------------------------------------------
----------------Library --------------------------------------------------------
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------

-- | change inplace first element matching the change (Data.List)
change :: (a -> Maybe a) -> [a] -> Maybe [a]
change t [] = Nothing
change t (x:xs) = case t x of
    Nothing -> (x :) <$>  change t xs
    Just x' -> Just $ x' : xs

-- | more powerful than unfoldr, returns the last generator value (Data.List)
unfoldrE :: (b -> Either b (a,b)) -> b -> ([a],b)
unfoldrE f x = case f x of
    Left y -> ([],y)
    Right (z,y) -> first (z:) $ unfoldrE f y

-- | Node lenses (Data.Tree)
label = lens rootLabel (\(Node x xs) y -> Node y xs)
-- | Node lenses (Data.Tree)
subs = lens subForest (\(Node x _) ys -> Node x ys)

-- | Sort the children of a rosetree by label (Data.Tree)
sortChildren :: Ord a => Tree a -> Tree a
sortChildren  = over subs $ sortBy (comparing rootLabel)  . map sortChildren 

--------------------------------------------------------------------------------
---------------------Sets as a measure for collect, probably broken as monoid---
------------------------check monotonicity -------------------------------------
--------------------------------------------------------------------------------

newtype Collect a = Collect {unCollect :: a} deriving Show

instance Ord a => Measured (Set a) (Collect a) where
    measure (Collect x) = S.singleton x

type FT a = FingerTree (Set a) (Collect a)

--------------------------------------------------------------------------------
---------------------IsoPaths---------------------------------------------------
---paths coupled with their reversed vrsion-------------------------------------
--------------------------------------------------------------------------------

data IsoPath a = IsoPath {
    _orig :: FT a,
    _rev :: FT a 
    } deriving Show
makeLenses ''IsoPath


instance Foldable IsoPath where
    foldr f x = foldr f x . toList

fromList [] = IsoPath F.empty F.empty
fromList (x:xs) = over orig (x <| ) . over rev (|> x) $ fromList xs
instance Ord a => Monoid (IsoPath a) where
    IsoPath o r `mappend` IsoPath o' r' = 
        IsoPath (o `mappend` o') (r' `mappend` r)
    mempty = IsoPath mempty mempty

-- | Splitting effect
data Split a = Take a | Splitted a a
    deriving (Show, Functor)

splitIsoPath :: Ord a => a -> IsoPath a -> Maybe (Split (IsoPath a))
splitIsoPath x p@(IsoPath o r) = let 
    (ao,bo) = split (S.member x) o
    (ar, viewl -> w :< br) = split (S.member x) r
    in  if null bo then Nothing
        else if null ao then Just $ Take p
        else Just $ Splitted (IsoPath ao br) (IsoPath bo $ ar |> w )

-- | a cut below version of the above, failing silently on Take
cutSplitIsoPath x p = let
    r = splitIsoPath x . swapIsoPath $ p
    f (Splitted (swapIsoPath -> x) (swapIsoPath -> y)) = 
       Splitted y x
    f (Take p) = p
    in fmap f r

swapIsoPath :: IsoPath a -> IsoPath a
swapIsoPath (IsoPath o r) = IsoPath r o

tailIsoPath :: Ord a => IsoPath a -> Collect a
tailIsoPath (IsoPath (viewl -> w :< _) _) = w
--------------------------------------------------------------------------------
--------A path is an IsoPath with a potential depedency-------------------------
--------the last element of a path with no dependency is actual root------------
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------

data Path a = Path {
    _father :: Maybe a,
    _path :: IsoPath a
    } deriving Show

makeLenses ''Path

type Forest b a = [b a]

--------------------------------------------------------------------------------
---------Deconstruction---------------------------------------------------------
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
-- | convert a forest of trees to a forest of paths
treesToPaths   :: Ord a 
            => Forest Tree a -- ^ tree to deconstruct
            -> Forest Path a -- ^ deconstruction
treesToPaths = concatMap (paths' Nothing) where
    paths' m (Node x []) = [Path m (IsoPath ft ft)] where 
                     ft = F.singleton $ Collect x
    paths' m (Node x xs) = let 
        Path _ (IsoPath o r) : ps = 
            sortBy (comparing (length . view (path . orig))) $ 
                zip (repeat $ Just x) xs >>= uncurry paths' 
        in 
        Path m (IsoPath (o |> Collect x) (Collect x <| r)) : ps

--------------------------------------------------------------------------------
----------Reconstruction -------------------------------------------------------
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
-- path head consumer, fail on empty path
consume (Path _ (IsoPath (viewr -> o :> Collect h ) (viewl -> _ :< r))) = 
    Just (h,Path (Just h) (IsoPath o r))
consume _ = Nothing

--------------------------------------------------------------------------------
--------------Pure reconstruction ----------------------------------------------
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------

-- collect a subforest of trees , children of a node
children    ::  Ord a 
            => Maybe a -- dependency
            -> Forest Path a  -- forest to consume
            -> (Forest Path a, Forest Tree (Maybe a))
children mf =   second catMaybes . 
                uncurry (mapAccumL maybeNode) .
                partition ((/=) mf . view father) 

-- try to reconstruct a node of a tree
maybeNode   :: Ord a 
            => Forest Path a 
            -> Path a 
            -> (Forest Path a, Maybe (Tree (Maybe a)))

maybeNode f c = case consume c of
    Nothing -> (f,Nothing)
    Just (h,c') -> let
        (f',mc) = maybeNode f c'
        in second (Just . Node (Just h)) $ case mc of 
            Nothing -> (f',[])
            Just n -> second (n:) $ children (Just h) f'

-- | convert a forest of paths back to forest of trees format
pathsToTrees    
                :: Ord a 
                =>  Forest Path a    -- ^ paths to reconstruct
                ->  Forest Tree a   -- ^ reconstruction
pathsToTrees = map (fromJust <$>) . snd . children Nothing 
--------------------------------------------------------------------------------
--------Monadic reconstruction -------------------------------------------------
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------

maybeNodeM  :: (Monad m, Ord a)
            => Path a 
            -> StateT (Forest Path a) m (Maybe (Tree (Maybe a)))
maybeNodeM (consume -> Just (h,c)) = fmap (Just . Node (Just h)) $
        maybeNodeM c >>= \mc -> case mc of 
            Nothing -> return []
            Just n -> (n:) <$> childrenM (Just h)
maybeNodeM _ = return Nothing

childrenM   :: (Monad m, Ord a)
            => Maybe a 
            -> StateT (Forest Path a) m (Forest Tree (Maybe a))
childrenM mf = fmap catMaybes  $ 
        StateT (return . partition ((/=) mf . view father)) 
        >>= mapM maybeNodeM

pathsToTreesM :: (Monad m, Ord a) 
        => (Forest Path a) 
        -> m (Forest Tree a) 
pathsToTreesM = fmap (map (fromJust <$>)) . evalStateT (childrenM Nothing)  


----------------------------------------------------------------
type IsoCut a = a -> IsoPath a -> Maybe (Split (IsoPath a))
-- split a path 
splitPath :: Ord a => IsoCut a -> a -> Path a -> Maybe (Split (Path a))
splitPath c x p@(Path mf dp) = g <$> c x dp where
    g (Splitted p1 p2) = Splitted (Path (Just x) p1) (Path mf p2)
    g (Take p) = Take (Path mf p)

unsplitPath (Splitted l r) = Path (view father r) (view path l <> view path r)
unsplitPath (Take p) = p

joinAndReversePaths :: Ord a => Forest Path a -> Path a
joinAndReversePaths xs = over path (swapIsoPath) $ 
    foldr1 (\(Path _ dp) (Path mf dp') ->  Path mf $ dp <> dp') xs

-- insert a path, trying to merge something depending on its tail
mergeCorrection :: Ord a => Path a -> Forest Path a -> Maybe (Forest Path a)
mergeCorrection p = change $ \p' -> 
    case view father p' == Just (unCollect $ tailIsoPath $ view path p) of
        True -> Just $ over path (mappend $ view path p') p
        _ -> Nothing

mergeOrInsert :: Ord a => Path a -> Forest Path a -> Forest Path a
mergeOrInsert p f = maybe (p:f) id $ mergeCorrection p f


--------------------------------------------------------------------------------
--------Linking-----------------------------------------------------------------
-- linking two vertex or creating an edge---------------------------------------
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------


-- result of a successful split
data Result b a = Result {
    residual :: Maybe (b a),
    taken :: b a
    }


unzipResults :: [Result Path a] -> ([Maybe (Path a)], [Path a])
unzipResults = unzip . map (\(Result r t)  -> (r,t))

-- extract a splitted path from the forest given the splitting vertex
selectPath :: Ord a => IsoCut a -> a -> Forest Path a 
    -> Maybe (Split (Path a), Forest Path a)
selectPath c x f = let
    (rs, unzip -> (f',_)) = partition (isJust . snd) $ 
        zip <*> map (splitPath c x) $ f
    in case rs of
        [(_,Just s)] -> Just $ (s,f')
        _ -> Nothing 

-- core reroot function, cut and take the dependency of the right cutted piece
-- and cut again, collecting results
type ExposeCore a = (Maybe a , Forest Path a)
rerootCore      :: (Ord a)
                => ExposeCore a -- actual results and
                -> Either (ExposeCore a) (Result Path a, ExposeCore a)

rerootCore (Nothing,f) = Left (Nothing,f)
rerootCore (Just x, f) = case selectPath splitIsoPath x f of
            (Just (Splitted r l,f')) -> Right (Result (Just r) l,(view father l,f'))
            (Just (Take p,f')) -> Right (Result Nothing p,(view father p,f'))
            Nothing -> error "vertex not found"

rerootIterate :: (Ord a) => Maybe a -> Forest Path a -> 
    ([Result Path a], Forest Path a)
rerootIterate mf f = second snd $ unfoldrE rerootCore (mf,f)

fromJustE (Just x) = x
fromJustE Nothing = error "invariant broken"

acceptOneNothing (Nothing : xs) = map fromJustE xs
acceptOneNothing (Just x:xs) = x: map fromJustE xs
acceptOneNothing [] = error "cutting the void!"

rerootInternal :: (Ord a) => a -> (Path a -> Path a) 
    -> Forest Path a -> Forest Path a
rerootInternal x c f = let
    (unzipResults -> (acceptOneNothing -> rs,ts),f') = rerootIterate (Just x) f
    in mergeOrInsert (c $ joinAndReversePaths ts) $ rs ++ f' 

--------------------------------------------------------------------------------
-------------- Interface --------------------------------------------------------
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------

-- | bring the element to be the root of its tree
reroot  :: (Ord a) 
        => a 
        -> Forest Path a 
        -> Forest Path a
reroot x = rerootInternal x id

rerootM  = modify . reroot

-- | find the root of a node
root :: Ord a => a -> Forest Path a -> Maybe a
root x = fmap unCollect . r x where
    r x f = case selectPath splitIsoPath x f of
        Nothing -> Nothing 
        Just (unsplitPath -> Path mf (viewr . view orig -> _ :> x'),f') -> 
            maybe (Just x') (flip r f') mf

-- | check 2 vertexes to be on the same tree
connected :: Ord a =>  a -> a -> Forest Path a -> Bool
connected x y = (==) <$> root x <*> root y

-- | create a new link 
link :: (Show a,Ord a) => a -> a -> Forest Path a -> Maybe (Forest Path a)
link y x f = 
    let g (unsplitPath -> s, f') = mergeOrInsert s $ 
                        rerootInternal y (set father $ Just x) f' 
    in g <$> selectPath splitIsoPath x f 



cut :: Ord a => a -> Forest Path a -> Maybe (Forest Path a)
cut x f = g <$>  selectPath cutSplitIsoPath x f where
    g (Splitted p1 p2, f) = set father Nothing p1 : mergeOrInsert p2 f
    g (Take p, f) = set father Nothing p : f



