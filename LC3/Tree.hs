{-# language ViewPatterns, DeriveFunctor, NoMonomorphismRestriction #-}

module Tree where

import Test.QuickCheck
import Control.Monad.State
import Data.Maybe (fromJust)
import Data.Tree
import Data.List (sortBy, uncons)
import Data.Ord (comparing)


-- | Tree equality equality up to children natural sorting 

newtype TreeEq a = TreeEq (Tree a) 

instance Ord a => Eq (TreeEq a) where
    TreeEq (Node x xs) == TreeEq (Node y ys) = 
        let sort = sortBy (comparing rootLabel)
        in x == y && sort xs == sort ys

sameTree x y = TreeEq x == TreeEq y

---------------------------------------------------------------------------------
-------------Quick Check--------------------------------------------------------------------
---------------------------------------------------------------------------------
---------------------------------------------------------------------------------
---------------------------------------------------------------------------------
---------------------------------------------------------------------------------
gTree :: Int -> Gen (Tree ())
gTree 0 = return $ Node () []
gTree n = do
  m <- choose (0,n-1)
  Node () <$>  vectorOf m (gTree (n-1))

relabel :: Tree a -> State [Int] (Tree Int)
relabel (Node _ cs) = do
    x <- StateT $ return . fromJust . uncons
    cs' <- mapM relabel cs
    return $ Node x cs'

relabelForest :: [Tree a] -> [Tree Int] 
relabelForest f = evalState (mapM relabel f) [1..]

relabeller :: Tree a -> Tree Int
relabeller t = evalState (relabel t) [1..]

arbitraryTree :: Int -> Gen (Tree Int)
arbitraryTree m = fmap relabeller (gTree m)

---------------------------------------------------------------------------------
---------Minimal Zipper----------------------------------------------------------
---------------------------------------------------------------------------------
---------------------------------------------------------------------------------

data Z a  = Z (Tree a) [C a] deriving (Show)
data C a = C a [Tree a] deriving (Show)

mkZ :: a -> Z a
mkZ x = Z (Node x []) []

focus :: Z a -> a
focus (Z (Node x _) _) = x

down (Z (Node n (c:cs)) bcs) = Z c (C n cs : bcs)

up (Z t []) = Nothing
up (Z t (C n rs : tcs)) = Just $ Z (Node n (t : rs)) tcs 

insertC x (Z (Node y ys) cs) = down $ Z (Node y (Node x []: ys)) cs 

tree = (\(Z t _) -> t) . top where
    top z = maybe z top $ up z


{-

newtype M a b = M (Z a -> Maybe (b,Z a)) deriving Functor
instance Applicative (M a) where
    M f <*> M g = M $ \s -> do
            (f',s') <- f s
            (y,s'') <- g s'
            return $ (f' y,s'')
    pure x = M $ \s -> Just (x,s)

instance Monad (M a) where
    M f >>= g = M $ \s -> f s >>= \(x,s') -> let M k = g x in k s'

down' = M $ \t -> (,) () <$> down t
up' = M $ \t -> (,) () <$> up t
insertC' x  = M $ \t -> (,) () <$> insertC x t
focus' = M $ \t -> Just (focus t, t)
-}



