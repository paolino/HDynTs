module HDynTs.EulerTours.CoreSpec (main, spec) where

import Test.Hspec
import Test.QuickCheck

import HDynTs.Lib.Tree
import HDynTs.EulerTours.Core

import Data.List
import Data.Foldable
import Data.Tree

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "splice" $ do
    it "all elements are spliced" $ property $ 
        test_splice_set 8
  describe "reroot" $ do
    it "reroot head is the elem" $ property $ 
        test_reroot_head 8

  describe "extract" $ do
    it "extract head is the elem" $ property $ 
        test_extract_head 8


eset :: Ord a => [a] -> [a]
eset = map head . group . sort 


test_splice_set :: Int -> () -> Gen Bool
test_splice_set n () = do
    [x,y] <- map fromTree <$> arbitraryForest 2 n
    e <- elements $ toList y
    let s = splice x e y
    return $ eset (toList s) == eset (toList x ++ toList y)


test_reroot_head :: Int -> () -> Gen Bool
test_reroot_head n () = do   
    x <- fromTree <$> arbitraryTree n
    e <- elements $ toList x
    let Node h _ = toTree $ reroot e x 
    return $ h == e

test_extract_head :: Int -> () -> Gen Bool
test_extract_head n () = do   
    t@(Node nh _) <- arbitraryTree n `suchThat` (not . null . subForest)

    let x = fromTree t
    e <- elements (toList x) `suchThat` (/= nh)
    let Node h _ = toTree . fst $ extract e x 
    return $ h == e

