
{-# language MultiParamTypeClasses, ScopedTypeVariables, ViewPatterns, GeneralizedNewtypeDeriving, TypeSynonymInstances, FlexibleInstances,TemplateHaskell #-}
import qualified Data.FingerTree as F
import Data.Set (Set)
import qualified Data.Set as S
import Data.Monoid
import Control.Lens hiding ((:<),(<|),(|>),(:>), children, elements)
import Data.Foldable
import  Data.FingerTree (FingerTree, split, measure, Measured, viewl,viewr,  
    (<|) , (|>), ViewL ((:<),EmptyL), ViewR ((:>)), fromList )
import Test.QuickCheck
import Data.Tree

newtype Collect a = Collect {unCollect :: a} deriving (Eq,Ord,Show)

instance Ord a => Measured (Set a) (Collect a) where
    measure (Collect x) = S.singleton x


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

instance Ord a => Monoid (ETR a) where
    ETR o r `mappend` ETR o' r' = ETR (o `mappend` o') (r' `mappend` r)
    mempty = ETR mempty mempty

makeLenses ''ETR

mkETR :: Ord a => [a] -> ETR a
mkETR =  foldr (\x -> over orig (x <|) . over rev (|> x)) (ETR mempty mempty) . map MP

extract :: Ord a => a   -> ETR a -> (ETR a, ETR a)
extract c (ETR o r) = let
    (o1,o2) = split (S.member c . vmpSet) o
    (r1,r2) = split (S.member c . vmpSet) r
    l = (vmpSize (measure r2) - vmpSize (measure o1))
    (o21,o22) = split ((> l) . vmpSize) o2
    (r21,r22) = split ((> l) . vmpSize) r2
    in (ETR o21 r21, ETR (o1 <> o22) (r1 <> r22))

reroot :: Ord a => a -> ETR a -> ETR a
reroot x (ETR o r) = let
    (o1,o2) = split (S.member x . vmpSet) o
    (r1, r2) = split (flip (>) (vmpSize (measure o2)) . vmpSize) r

    in ETR ((o2 `mcolls` r2) |> MP x) (MP x <| (o1 `mcolls` r1))

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

{-
fromEulerTour :: Eq a => NonEmpty a -> T a
fromEulerTour (x:|xs) = tree . top $ fromEulerTour' (Just $ mkZ x) xs where
    fromEulerTour' (Just z) [] = z
    fromEulerTour' (Just z) (x:xs) = case focus <$> up z of
        Just ((==) x -> True) -> fromEulerTour' (up z) xs
_ -> fromEulerTour' (insertC x z) xs  
-}
--------------------------------------------------------------------------------
---------------------IsoSequences---------------------------------------------------
---paths coupled with their reversed vrsion-------------------------------------
--------------------------------------------------------------------------------
{-

data IsoSequence a = IsoSequence {
    _orig :: Sequence a,
    _rev :: Sequence a 
    } deriving (Show,Eq,Ord)
makeLenses ''IsoSequence


instance Foldable IsoSequence where
    foldr f x = foldr f x . toList

fromList = foldr (\x -> over orig (x <| ) . over rev (|> x)) $ IsoSequence F.empty F.empty

instance Ord a => Monoid (IsoSequence a) where
    IsoSequence o r `mappend` IsoSequence o' r' = 
        IsoSequence (o `mappend` o') (r' `mappend` r)
    mempty = IsoSequence mempty mempty

type IsoCut a = a -> IsoSequence a -> Maybe (Split (IsoSequence a), a)

splitIsoSequence :: Ord a => IsoCut a
splitIsoSequence x p@(IsoSequence o r) = let 
    (ao,bo) = split (S.member x) o
    (ar, viewl -> w :< br) = split (S.member x) r
    in  if null bo then Nothing
        else if null ao then Just $ (Take p,x)
        else Just $ (Splitted (IsoSequence ao br) (IsoSequence bo $ ar |> w ),x)

-- | a cut below version of the above, failing silently on Take
cutSplitIsoSequence :: Ord a => IsoCut a
cutSplitIsoSequence x p = let
    r = splitIsoSequence x . swapIsoSequence $ p
    f (Splitted (swapIsoSequence -> x) (swapIsoSequence -> y),_) = 
       (Splitted y x, unCollect $ tailIsoSequence x) 
    f (Take (swapIsoSequence -> x),_) = (Take x,undefined)
    in fmap f r

swapIsoSequence :: IsoSequence a -> IsoSequence a
swapIsoSequence (IsoSequence o r) = IsoSequence r o


tailIsoSequence :: Ord a => IsoSequence a -> Collect a
tailIsoSequence (IsoSequence (viewl -> w :< _) _) = w
-}
