{-# language ViewPatterns, DeriveFunctor, NoMonomorphismRestriction #-}
-- | Extensions to the Data.Tree module
module HDynTs.Lib.Tree (
    -- * types
    TreeEq (..),
    -- * quick check
    arbitraryTree,
    arbitraryForest,
    -- * zipper
    Z,
    mkZ,
    focus,
    up,
    insertC,
    tree
    )
    where

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

relabeller :: Tree a -> Tree Int
relabeller t = evalState (relabel t) [1..]

-- | create an arbitrary tree with at most m levels and m aperture
arbitraryTree   :: Int  -- ^ m
                -> Gen (Tree Int)
arbitraryTree m = fmap relabeller (gTree m)

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



