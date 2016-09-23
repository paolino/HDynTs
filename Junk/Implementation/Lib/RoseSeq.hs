
{-# language ViewPatterns, DeriveFunctor #-}

module RoseSeq (T,mkT, viewT, drawT, SerT (..) , 
        serializeT, rebuildT, Z , mkZ, focus , top, up, tree, insertC)  where

import qualified Data.Map as M
import qualified Data.Sequence as S

import qualified Data.Tree as T
import Data.Maybe (fromJust)
import Data.Sequence (viewr, viewl, ViewR ((:>)), ViewL ((:<)), (|>), (<|))
import Data.Foldable (toList)

import Control.Arrow hiding (right)
import Data.List
import Data.Ord
import Data.Function

-- edge
type E a = (a,a)

mkMap :: Ord a => [E a] -> M.Map a [a]
mkMap =     sortBy (comparing fst) >>>
            groupBy ((==) `on` fst) >>> 
            map (fst . head &&& map snd) >>> 
            M.fromList

type St a = S.Seq (T a)

data T a = T a (St a) deriving (Functor, Show)

viewT (T x ts) = (x,ts)

mkT :: Ord a => M.Map a [a] -> a -> T a
mkT (fmap S.fromList -> es) = mkT' Nothing where
    mkT' mj i 
        | M.null es = T i (S.empty)
        | otherwise = T i . fmap (mkT' $ Just i) 
                . maybe id (S.filter . (/=)) mj $ M.findWithDefault mempty i es

data SerT a = SerT a [E a] deriving (Show,Read)

rebuildT :: Ord a => SerT a  -> T a
rebuildT (SerT t es) = mkT (mkMap es) t

serializeT :: T a -> SerT a
serializeT = uncurry SerT . serializeT' where
    serializeT' (T x xs) = let 
        (vs,es) = unzip $ serializeT' <$> toList xs
        in (x,map ((,) x) vs ++ concat es)

convert :: T a -> T.Tree a
convert (T x xs) = T.Node x $ map convert $ toList xs

drawT :: (a -> String) -> T a -> String
drawT f = T.drawTree . convert . fmap f
--- zipper -------------------------

data Z a  = Z (T a) [C a] deriving (Show)
data C a = C (St a) a (St a) deriving (Show)

mkZ :: a -> Z a
mkZ x = Z (T x mempty) []

focus :: Z a -> a
focus (Z (T x _) _) = x

down (Z (T _ (S.null -> True)) _) = Nothing
down (Z (T n (viewr -> cs :> c)) bcs) = Just $ Z c (C cs n mempty : bcs)

right (Z _ []) = Nothing
right (Z t (C _ _ (S.null -> True) : _)) = Nothing
right (Z t (C ls n (viewl -> r :< rs) : bcs)) = Just $ Z r (C (ls |> t) n rs : bcs)

up (Z _ []) = Nothing
up (Z t (C ls n rs:tcs)) = Just $ Z (T n (ls `mappend`  (t <| rs))) tcs where

insertZ _ (Z t []) = Nothing
insertZ x (Z t (C ls n rs:bcs)) = Just $ Z t (C ls n (T x mempty <| rs) : bcs)

insertC x (Z (T y (S.null -> True)) cs) = Just (Z (T y (S.singleton $ T x mempty)) cs) >>= down
insertC x t = down t >>= insertZ x >>= right

tree (Z t _) = t

run f z@(f -> Nothing) = z
run f z = run f $ fromJust (f z)

top = run up

