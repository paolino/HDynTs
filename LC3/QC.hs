{-# language FlexibleInstances #-}
import FT1
import Test.QuickCheck
import Data.Tree hiding (Forest)
import Console
import Control.Monad.State
import Control.Monad.Identity
import Data.Foldable
import Data.List

sizedArbTestRoseT :: Int -> Gen (Tree ())
sizedArbTestRoseT 0 = do
  c <- arbitrary
  return $ Node c []
sizedArbTestRoseT n = do
  c <- arbitrary
  subtreeCount <- choose (0,n-1)
  subtrees <- vectorOf subtreeCount (sizedArbTestRoseT (n-1))
  return $ Node c subtrees

relabel :: Tree a -> State [Int] (Tree Int)
relabel (Node _ cs) = do
    x <- StateT $ \(x:xs) -> return (x,xs)
    cs' <- mapM relabel cs
    return $ Node x cs'

relabeller :: Tree a -> Tree Int
relabeller t = evalState (relabel t) [1..]

arbitraryTree :: Int -> Gen (Tree Int)
arbitraryTree m = fmap relabeller (sizedArbTestRoseT m)

testConversionsIsomorphism :: Ord a => Forest Tree a -> Bool
testConversionsIsomorphism t = (pathsToTrees . treesToPaths) t ==  t

someTrees n = (flip evalState [1..] . mapM relabel) <$>
    (sample' $ arbitraryTree n)

testLinkCutIsomorphism :: Tree Int -> Tree a -> Gen Bool
testLinkCutIsomorphism xt yt' = let
        xs = toList xt
        yt = evalState (relabel yt') [last xs + 1 .. ]
        ys = toList yt
        forest = treesToPaths $ [xt,yt]
        in do
            x <- elements xs
            y <- elements ys
            let Just forest' = link x y forest >>= cut x
            return $ TreeEq forest == TreeEq forest'

{-
Node    {rootLabel = 1, subForest = [
            Node {rootLabel = 2, subForest = [
                    Node {rootLabel = 3, subForest = [
                            Node {rootLabel = 4, subForest = []}
                            ]}
                    ]},
                    Node {rootLabel = 5, subForest = [
                        Node {rootLabel = 6, subForest = [
                            Node {rootLabel = 7, subForest = []}
                            ]},
                        Node {rootLabel = 8, subForest = [
                            Node {rootLabel = 9, subForest = []}
                        ]}
                            ]}
              ]}
                            -}
