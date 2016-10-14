{-# language ViewPatterns#-}
{-# language DeriveFunctor#-}
{-# language NoMonomorphismRestriction #-}

{-|
Module      : HDynTs.Lib.Tree
Description : Additional functionality for Data.Tree
Copyright   : (c) Paolo Veronelli, 2016
License     : BSD
Maintainer  : paolo.veronelli@gmail.com
Stability   : experimental
-}

module HDynTs.Lib.Tree (
    -- * types
    SortedTree(..),
    -- * quick check
    arbitraryTree,
    arbitraryForest,
    -- * zipper
    Z,
    mkZ,
    focus,
    up,
    insertC,
    tree,
    -- * drawing
    drawTreeU 
    )
    where

import Test.QuickCheck
import Control.Monad.State
import Data.Maybe (fromJust)
import Data.Tree
import Data.List (sortBy, uncons)
import Data.Ord (comparing)


-- | sort the children of a tree based on their label
sortTree :: Ord a => Tree a -> Tree a 
sortTree t@(Node x []) = t
sortTree (Node x xs) = Node x $ sortBy (comparing rootLabel) $ map sortTree xs

-- | Tag for a Tree to gain access to a up-to-children re-sorting Eq instance
newtype SortedTree a = SortedTree (Tree a)

instance Ord a => Eq (SortedTree a) where
    SortedTree x == SortedTree y = sortTree x == sortTree y
--------------------------------------------------------------------------------
-------------Quick Check--------------------------------------------------------
--------------------------------------------------------------------------------

--  generate a random tree mix at most n levels and n aperture
gTree :: Int -> Gen (Tree ())
gTree 0 = return $ Node () []
gTree n = do
  m <- choose (0,n-1)
  Node () <$>  vectorOf m (gTree (n-1))

-- relabel one tree consuming state
relabel :: Tree a -> State [Int] (Tree Int)
relabel (Node _ cs) = do
    x <- StateT $ return . fromJust . uncons
    cs' <- mapM relabel cs
    return $ Node x cs'

relabelForest :: [Tree a] -> [Tree Int] 
relabelForest f = evalState (mapM relabel f) [1..]

-- | create an arbitrary tree with at most m levels and m aperture
arbitraryTree   :: Int  -- ^ m
                -> Gen (Tree Int)
arbitraryTree m = head <$> arbitraryForest 1 m 

-- | create an arbitrary n-tree forest with at most m levels and m aperture
arbitraryForest     :: Int -- ^ n
                    -> Int -- ^ m
                    -> Gen [Tree Int]
arbitraryForest n m = fmap relabelForest $ replicateM n $ gTree m

--------------------------------------------------------------------------------
---------Minimal Zipper---------------------------------------------------------
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------

-- | zipper structure
data Z a  = Z (Tree a) [C a] deriving (Show)
-- | context structure

data C a = C a [Tree a] deriving (Show)

-- | create a zipper on a singleton rose tree
mkZ :: a -> Z a
mkZ x = Z (Node x []) []

-- | get the element at focus
focus :: Z a -> a
focus (Z (Node x _) _) = x

-- | move the zipper down, unsafe
down (Z (Node n (c:cs)) bcs) = Z c (C n cs : bcs)

-- | move the zipper up, safe
up (Z t []) = Nothing
up (Z t (C n rs : tcs)) = Just $ Z (Node n (t : rs)) tcs 

-- | insert a child in front of the others, if present, safe
insertC x (Z (Node y ys) cs) = down $ Z (Node y (Node x []: ys)) cs 

-- | extract the tree from the zipper bringing it to the top
tree = (\(Z t _) -> t) . top where
    top z = maybe z top $ up z


-- | Neat 2-dimensional drawing of a tree, using Unicode
drawTreeU :: Tree String -> String
drawTreeU  = unlines . draw

draw :: Tree String -> [String]
draw (Node x ts0) = x : drawSubTrees ts0
  where
    drawSubTrees [] = []
    drawSubTrees [t] =
        shift "\x2514\x2500" "  " (draw t)
    drawSubTrees (t:ts) =
        shift "\x251c\x2500" "\x2502 " (draw t) ++ drawSubTrees ts

    shift first other = zipWith (++) (first : repeat other)
