{-# language MultiParamTypeClasses#-}
{-# language ScopedTypeVariables#-}
{-# language ViewPatterns#-}
{-# language GeneralizedNewtypeDeriving#-}
{-# language TypeSynonymInstances#-}
{-# language FlexibleInstances#-}
{-# language DataKinds#-}
{-# language GADTs#-}
{-# language FlexibleContexts#-}
{-# language UndecidableInstances #-}

-- | Euler Tour core functionalities. A Tour is a FingerTree of the elements,
-- coupled with its reverse. All modifications are sublinear in the number of
-- elements. The TourMonoid is partly exported to permit composition with 
-- above structures.

module HDynTs.EulerTours.Core (
    -- * types
    Tour (..),
    -- * operation 
    splice,
    extract,
    reroot, 
    -- * query
    path,
    father,
    -- * conversion
    fromTree,
    toTree,
    unsafeFromList,
    fromList,
    toList,
    -- * debug
    valid
    )
    where

import Data.Set (Set, member,singleton)
import qualified Data.Set as S
import Data.Monoid (Sum (Sum), (<>))
import Data.Foldable (toList, Foldable)
import Data.FingerTree (FingerTree, split, measure, Measured, viewl,viewr,  
    (<|) , (|>), ViewL ((:<),EmptyL), ViewR ((:>), EmptyR),  empty )
import Data.Tree (Tree(Node))
import Data.Maybe (fromJust)
import Control.Monad (guard)

import HDynTs.Lib.Tree (insertC,focus,up, tree,mkZ)

newtype TourElem a = TourElem a deriving (Show,Ord,Eq)

--  The monoid for the fingertree representation of the tour
newtype TourMonoid a = TourMonoid (Set a,Sum Int) deriving  (Monoid,Show)

-- a predicate to test the presence of an elem in the tour
tmMember :: Ord a => a -> TourMonoid a -> Bool
tmMember x (TourMonoid (v,_)) = x `member` v

-- position of an element in the tour 
tmPosition :: TourMonoid a -> Int
tmPosition (TourMonoid (_,Sum s)) = s

--  set the position to one in the monoid
tmSet :: TourMonoid a -> Set a
tmSet (TourMonoid (x,_)) = x

instance Ord a => Measured (TourMonoid a) (TourElem a) where
    measure (TourElem x) = TourMonoid (singleton x, 1)

type STour a = FingerTree (TourMonoid a) (TourElem a) 

-- | Euler tour representation
data Tour a = Tour (STour a) (STour a) deriving (Show, Eq)

instance Ord a => Measured (Set a) (Tour a) where
    measure (Tour o _) = tmSet . measure $ o

instance Foldable Tour where
    foldr f x (Tour o _) = foldr f x $ map (\(TourElem x) -> x) $ toList o


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

-- | check validity of internal data
valid :: Ord a => Tour a -> Bool
valid (Tour (viewl -> x :< xs) (viewr -> ys :> y)) 
    | x == y = valid (Tour xs ys)
    | otherwise = False
valid (Tour (viewl -> EmptyL) (viewr -> EmptyR)) = True
valid (Tour _ _) = False

-- | extract a subtour from a tour delimited by a vertex, unsafe
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

-- | rotate a tour to represent a rerooting to a vertex, unsafe
reroot  :: Ord a    
        => a        -- ^ new root
        -> Tour a   -- ^ old routed tour
        -> Tour a   -- ^ new rooted tour
reroot x e@(Tour o@(viewl -> TourElem x' :< _) r) 
    | x == x' = e
    | otherwise = let
        (o1,viewr -> o2 :> _) = split (tmMember x) o
        (viewl -> _ :< r1, r2) = split 
            (flip (>) (tmPosition (measure o2) + 1) . tmPosition) r
        in Tour ((o2 <> o1) |> TourElem x) (TourElem x <| (r2 <> r1))

-- | create a tour representing a given tree, safe 
fromTree    :: Ord a 
            => Tree a   -- ^ given tree
            -> Tour a   -- ^ corresponding tour
fromTree (Node  x ts) = g . mconcat $ map f ts where
        f t = let Tour o r = fromTree t in 
            Tour (TourElem x <| o) (r |>  TourElem x)
        g (Tour o r) = Tour (o |> TourElem x) (TourElem x <| r)

-- | reify a tour into the corrispondent tree, unsafe
toTree      :: Ord a 
            =>  Tour a  -- ^ abstract tour
            ->  Tree a  -- ^ correstponding tree
toTree (Tour (viewl -> TourElem x :< xs) _) = tree $ fromSTour (mkZ x) xs where
    fromSTour z (viewl -> EmptyL) = z
    fromSTour z (viewl -> TourElem x :< xs) = case focus <$> up z of
        Just ((==) x -> True) -> fromSTour (fromJust $ up z) xs
        _ -> fromSTour (insertC x z) xs  

-- check invariants for tours as lists
isEuler :: Ord a => [a] -> Bool
isEuler = isEuler' mempty mempty . (zip <*> tail) where
    isEuler' fs _ [] 
        | S.null fs = True -- children closed at the end
        | otherwise = False
    isEuler' fs gs ((x,x'):xs) 
        | x == x' = False -- must change
        | x' `member` gs = False -- reopening a children
        | (x',x) `member` fs = isEuler' (S.delete (x',x) fs) (S.insert x gs) xs 
            -- closing a children
        | otherwise = isEuler' (S.insert (x,x') fs) gs xs --opening a children
    

-- | safely create a Your from a list, it checks the list is a correct tour
fromList :: Ord a => [a] -> Maybe (Tour a)
fromList xs = guard (isEuler xs) >> return (unsafeFromList xs)

-- | set the tour from a list, no checks on the tour being valid
unsafeFromList :: Ord a => [a] -> Tour a 
unsafeFromList [] = Tour empty empty
unsafeFromList (x:xs) = let
    Tour o r = unsafeFromList xs
    in Tour (TourElem x <| o) (r |> TourElem x)

-- | compute the path between 2 elements of the tour
path :: Ord a => a -> a -> Tour a -> [a]
path x y (reroot x -> Tour o r) =  let
    collect f (viewr -> EmptyR) = f y
    collect f (viewr -> rs:> TourElem h) = collect ((h:) . f) z where
        (z,_) = split (tmMember h) rs
    (o1,_) = split (tmMember y) o
    in collect return o1


