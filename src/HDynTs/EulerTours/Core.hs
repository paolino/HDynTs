
{-# language MultiParamTypeClasses, ScopedTypeVariables, ViewPatterns, GeneralizedNewtypeDeriving, TypeSynonymInstances, FlexibleInstances,TemplateHaskell, DataKinds, GADTs, FlexibleContexts, UndecidableInstances #-}

module HDynTs.EulerTours.Core (
    -- * types
    Tour,
    tourMonoid ,
    -- * monoid
    TourMonoid,
    tmMember,
    tmPosition,
    tmResetPosition,
    -- * operation 
    splice,
    father,
    extract,
    reroot, 
    -- * conversion
    fromTree,
    toTree
    )
    where

import Data.Set (Set, member,singleton)
import Data.Monoid (Sum (Sum), (<>))
import Data.Foldable (toList)
import Data.FingerTree (FingerTree, split, measure, Measured, viewl,viewr,  
    (<|) , (|>), ViewL ((:<),EmptyL), ViewR ((:>), EmptyR), fromList )
import Data.Tree (Tree(Node))
import Data.Maybe (fromJust)

import HDynTs.Lib.Tree (insertC,focus,up, tree,mkZ)

newtype TourElem a = TourElem a deriving (Show,Ord,Eq)
newtype TourMonoid a = TourMonoid (Set a,Sum Int) deriving  (Monoid,Show)

-- | a predicate to test the presence of an elem in the tour
tmMember :: Ord a => a -> TourMonoid a -> Bool
tmMember x (TourMonoid (v,_)) = x `member` v

-- | position of an element in the tour 
tmPosition :: TourMonoid a -> Int
tmPosition (TourMonoid (_,Sum s)) = s

-- | set the position to one in the monoid
tmResetPosition :: TourMonoid a -> TourMonoid a 
tmResetPosition (TourMonoid (x,_)) = TourMonoid (x,1)

instance Ord a => Measured (TourMonoid a) (TourElem a) where
    measure (TourElem x) = TourMonoid (singleton x, 1)

type STour a = FingerTree (TourMonoid a) (TourElem a) 

-- | Euler tour representation
data Tour a = Tour (STour a) (STour a)

-- | Extract a valid monoid from a tour
tourMonoid :: Ord a => Tour a -> TourMonoid a
tourMonoid (Tour x _) = measure x

instance Ord a => Monoid (Tour a) where
    Tour o r `mappend` Tour o' r' = Tour (o `mappend` o') (r' `mappend` r)
    mempty = Tour mempty mempty

-- | insert a tour into another at specified vertex 
splice  :: Ord a 
        => Tour a   -- ^ tour to insert
        -> a        -- ^ insertion element
        -> Tour a   -- ^ accepting tour
        -> Tour a   -- ^ resulting tour
splice (Tour ot rt) c (Tour o r) = let
    (o1,o2@(viewl -> wc :< _)) = split (tmMember c) o
    (r1,r2) = split (flip (>) (tmPosition (measure o2)) . tmPosition) r
    in Tour (o1 <> (wc <| ot) <> o2) (r1 <> (rt |> wc) <> r2)

-- | find the father of a vertex in a tour
father  :: Ord a 
        => a        -- ^ child
        -> Tour a   -- ^ tour containing the child
        -> Maybe a  -- ^ possibly the father
father x (Tour o _) = case viewr . fst $ split (tmMember x) o of
    _ :> TourElem y -> Just y
    EmptyR -> Nothing
    
-- | extract a subtour from a tour delimited by a vertex
extract     :: Ord a 
            => a        -- ^ delimiting verte
            -> Tour a   -- ^ tour containing the vertex
            -> (Tour a, Tour a) -- ^ subtour and orphaned tour
extract c (Tour o r) = let
    (o1@(viewr -> o1' :> _),o2) = split (tmMember c) o
    (r1@(viewr -> r1' :> _),r2) = split (tmMember c) r
    l = (tmPosition (measure r2) - tmPosition (measure o1))
    (o21,o22) = split ((> l) . tmPosition) o2
    (r21,r22) = split ((> l) . tmPosition) r2
    in (Tour o21 r21, Tour (o1' <> o22) (r1' <> r22))

-- | rotate a tour to represent a rerooting to a vertex
reroot  :: Ord a    
        => a        -- ^ new root
        -> Tour a   -- ^ old routed tour
        -> Tour a   -- ^ new rooted tour
reroot x e@(Tour o@(viewl -> TourElem x' :< _) r) 
    | x == x' = e
    | otherwise = let
        (o1,viewr -> o2 :> _) = split (tmMember x) o
        (viewl -> _ :< r1, r2) = split (flip (>) (tmPosition (measure o2)) . tmPosition) r
        in Tour ((o2 <> o1) |> TourElem x) (TourElem x <| (r2 <> r1))

-- | create a tour representing a given tree 
fromTree    :: Ord a 
            => Tree a   -- ^ given tree
            -> Tour a   -- ^ corresponding tour
fromTree (Node  x ts) = g . mconcat $ map f ts where
        f t = let Tour o r = fromTree t in Tour (TourElem x <| o) (r |>  TourElem x)
        g (Tour o r) = Tour (o |> TourElem x) (TourElem x <| r)

-- | reify a tour into the corrispondent tree
toTree      :: Ord a 
            =>  Tour a  -- ^ abstract tour
            ->  Tree a  -- ^ correstponding tree
toTree (Tour (viewl -> TourElem x :< xs) _) = tree $ fromSTour (mkZ x) xs where
    fromSTour z (viewl -> EmptyL) = z
    fromSTour z (viewl -> TourElem x :< xs) = case focus <$> up z of
        Just ((==) x -> True) -> fromSTour (fromJust $ up z) xs
        _ -> fromSTour (insertC x z) xs  

