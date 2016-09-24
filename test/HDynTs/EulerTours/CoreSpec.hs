module HDynTs.EulerTours.CoreSpec (main, spec) where

import Test.Hspec
import Test.QuickCheck

import HDynTs.Lib.Tree
import HDynTs.EulerTours.Core

import Data.List
import Data.Foldable

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "splice" $ do
    it "all elements are spliced" $ property $ 
        test_splice 9


eset :: Ord a => [a] -> [a]
eset = map head . group . sort 

test_splice :: Int -> () -> Gen Bool
test_splice n () = do
    [x,y] <- map fromTree <$> arbitraryForest 2 n
    e <- elements $ toList y
    let s = splice x e y
    return $ eset (toList s) == eset (toList x ++ toList y)


