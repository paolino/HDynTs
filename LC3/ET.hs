
{-# language MultiParamTypeClasses, ScopedTypeVariables, ViewPatterns, GeneralizedNewtypeDeriving, TypeSynonymInstances, FlexibleInstances,TemplateHaskell #-}

module ET where

import qualified Data.FingerTree as F
import Data.Set (Set)
import qualified Data.Set as S
import Data.Monoid
import Control.Arrow
import Control.Lens hiding ((:<),(<|),(|>),(:>), children, elements)
import Data.Foldable
import  Data.FingerTree (FingerTree, split, measure, Measured, viewl,viewr,  
    (<|) , (|>), ViewL ((:<),EmptyL), ViewR ((:>)), fromList )
import Test.QuickCheck
import Data.Tree
import Data.Maybe

import Tree

newtype MP a = MP a deriving (Show,Ord,Eq)
newtype VMP a = VMP (Set a,Sum Int) deriving  (Monoid,Show)

vmpSet (VMP (v,_)) = v
vmpSize (VMP (_,s)) = s

instance Ord a => Measured (VMP a) (MP a) where
    measure (MP x) = VMP (S.singleton x, Sum 1)

type ET a = FingerTree (VMP a) (MP a) 

data ETR a = ETR {
    _orig,_rev :: ET a
    } deriving Show

valid (ETR o r) = o == F.reverse r

instance Ord a => Monoid (ETR a) where
    ETR o r `mappend` ETR o' r' = ETR (o `mappend` o') (r' `mappend` r)
    mempty = ETR mempty mempty

makeLenses ''ETR

mkETR :: Ord a => [a] -> ETR a
mkETR =  foldr (\x -> over orig (x <|) . over rev (|> x)) (ETR mempty mempty) . map MP

splice :: Ord a => ETR a -> a -> ETR a -> ETR a
splice (ETR ot rt) c (ETR o r) = let
    (o1,o2@(viewl -> wc :< _)) = split (S.member c . vmpSet) o
    (r1,r2) = split (flip (>) (vmpSize (measure o2)) . vmpSize) r
    in ETR (o1 <> (wc <| ot) <> o2) (r1 <> (rt |> wc) <> r2)

test_splice n = do
    x <- arbitraryTree n
    y <- arbitraryTree n
    let [fromTree -> x',fromTree -> y'] = relabelForest [x,y]
    MP e <- elements $ toList (view orig x')
    let s@(ETR o r) = splice x' e y'
    return $ valid s

extract :: Ord a => a   -> ETR a -> (ETR a, ETR a)
extract c (ETR o r) = let
    (o1@(viewr -> o1' :> _),o2) = split (S.member c . vmpSet) o
    (r1@(viewr -> r1' :> _),r2) = split (S.member c . vmpSet) r
    l = (vmpSize (measure r2) - vmpSize (measure o1))
    (o21,o22) = split ((> l) . vmpSize) o2
    (r21,r22) = split ((> l) . vmpSize) r2
    in (ETR o21 r21, ETR (o1' <> o22) (r1' <> r22))

reroot :: Ord a => a -> ETR a -> ETR a
reroot x e@(ETR o@(viewl -> MP x' :< _) r) 
    | x == x' = e
    | otherwise = let
        (o1,viewr -> o2 :> _) = split (S.member x . vmpSet) o
        (viewl -> _ :< r1, r2) = split (flip (>) (vmpSize (measure o2)) . vmpSize) r
        in ETR ((o2 <> o1) |> MP x) (MP x <| (r2 <> r1))

xt@(viewr -> xs :> x ) `mcolls` yt@(viewl -> y :< ys) 
    | x == y = (xs |> x) <> ys
    | otherwise = xt <> yt
xt `mcolls` yt  = xt <> yt


test_rereoot ::  Gen Bool
test_rereoot  = do
    xs :: String <- arbitrary `suchThat` (not . null)
    x <- elements xs  
    let ETR o r = reroot x $ mkETR xs
    return $ toList o == reverse (toList r)

fromTree :: Ord a => Tree a -> ETR a
fromTree (Node  x ts) = g . mconcat $ map f ts where
        f t = let ETR o r = fromTree t in ETR (MP x <| o) (r |>  MP x)
        g (ETR o r) = ETR (o |> MP x) (MP x <| r)

-- | from ET using zipper
toTree :: Ord a => Eq a => ETR a -> Tree a
toTree (ETR (viewl -> MP x :< xs) _) = tree $ fromET (mkZ x) xs where
    fromET z (viewl -> EmptyL) = z
    fromET z (viewl -> MP x :< xs) = case focus <$> up z of
        Just ((==) x -> True) -> fromET (fromJust $ up z) xs
        _ -> fromET (insertC x z) xs  
-- | extract an elem from FT

selectFT :: Measured m a => (m -> Bool) -> FingerTree m a -> Maybe (a, FingerTree m a)
selectFT c f = let
    (bs,as) = split c f
    in case viewl as of
        EmptyL -> Nothing
        x :< as' -> Just (x,bs <> as')

--------------------------------------------------------------------------------
---------------------Sets as a measure for collect, probably broken as monoid---
------------------------check monotonicity -------------------------------------
--------------------------------------------------------------------------------

newtype Collect a = Collect {unCollect :: a} deriving (Eq,Ord,Show)

instance Ord a => Measured (Set a) (ETR a) where
    measure (ETR x _) =  vmpSet $ measure x

type High a = FingerTree (Set a) (ETR a)

cut :: Ord a => a -> High a -> Maybe (High a)
cut x h = let
    f (e,h) = let 
        (e',e'') = extract x e 
        in e' <| e'' <| h
    in f <$> selectFT (S.member x) h

link :: Ord a => a -> a -> High a -> Maybe (High a)
link x y h = do
    (ex,h') <- selectFT (S.member x) h
    (ey,h'') <- selectFT (S.member y) h'
    return $ splice (reroot x ex) y ey <| h''


    
