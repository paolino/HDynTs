module HDynTs.EulerTours.CoreSpec (main, spec) where

import Test.Hspec
import Test.QuickCheck

import HDynTs.Lib.Tree
import HDynTs.EulerTours.Core

import Data.Foldable
import Data.Tree
import Data.FingerTree (measure)
import qualified Data.Set as S

main :: IO ()
main = hspec spec

spec :: Spec
spec = do

  describe "conversion" $ do 
    it "valid on from tree" $ property $
        fromTree_valid 8
    it "identity from/to tree" $ property $
        fromTree_toTree 8

  describe "splice" $ do
    it "valid internal operation" $ property $
        splice_valid 8
    it "all elements are spliced" $ property $ 
        splice_set 8

  describe "reroot" $ do
    it "valid reroot" $ property $
        reroot_valid 8
    it "reroot head is the elem" $ property $ 
        reroot_head 8
    it "reroot and back is preserving the graph" $ property $
        reroot_back 8

  describe "extract" $ do
    it "valid internal operation" $ property $ 
        extract_valid 8
    it "extract head is the elem" $ property $ 
        extract_head 8
  describe "splice + extract" $ do 
    it "extract . splice == id" $property $ 
        splice_extract 8 
  
(=&=) :: Ord a => Tree a -> Tree a -> Bool    
x =&= y = sortTree x == sortTree y

(=&&=) :: Ord a => Tour a -> Tour a -> Bool
x =&&= y = toTree x =&= toTree y

pick :: Tour a -> Gen a
pick = elements . toList

fromTree_valid :: Int -> () -> Gen Bool
fromTree_valid n () = valid <$> fromTree <$> arbitraryTree n

fromTree_toTree :: Int -> () -> Gen Bool
fromTree_toTree n () = do
    t <- arbitraryTree n
    return $ t =&= toTree (fromTree t)

splice_valid :: Int -> () -> Gen Bool
splice_valid n () =  do
    [x,y] <- map fromTree <$> arbitraryForest 2 n
    e <- pick y
    return $ valid $ splice x e y

splice_set :: Int -> () -> Gen Bool
splice_set n () = do
    [x,y] <- map fromTree <$> arbitraryForest 2 n
    e <- pick y
    let s = splice x e y
    return $ measure s == measure x `S.union` measure y

reroot_head :: Int -> () -> Gen Bool
reroot_head n () = do   
    x <- fromTree <$> arbitraryTree n
    e <- pick x
    let Node h _ = toTree $ reroot e x 
    return $ h == e

reroot_valid :: Int -> () -> Gen Bool
reroot_valid n () = do   
    x <- fromTree <$> arbitraryTree n
    e <- pick x
    return $ valid  $ reroot e x 

reroot_back :: Int -> () -> Gen Bool
reroot_back n () = do
    x'@(Node h _) <- arbitraryTree n
    let x = fromTree x'
    e <- pick x
    return $ reroot h (reroot e x) =&&= x

extract_head :: Int -> () -> Gen Bool
extract_head n () = do   
    t@(Node nh _) <- arbitraryTree n `suchThat` (not . null . subForest)
    let x = fromTree t
    e <- pick x `suchThat` (/= nh)
    let Node h _ = toTree . fst $ extract e x 
    return $ h == e

extract_valid :: Int -> () -> Gen Bool
extract_valid n () = do   
    t@(Node nh _) <- arbitraryTree n `suchThat` (not . null . subForest)
    let x = fromTree t
    e <- pick x `suchThat` (/= nh)
    let (e1,e2) = extract e x 
    return $ valid e1 && valid e2

splice_extract :: Int -> () -> Gen Bool
splice_extract n () = do
    ts@[tx,_] <- arbitraryForest 2 n
    let [x,y] = map fromTree ts
    e <- pick y
    let     (x',y') = extract (rootLabel tx) $ splice x e y
    return $ x =&&= x' && y =&&= y'

