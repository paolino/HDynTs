
{-# language ViewPatterns, DeriveFunctor #-}

module LinkCutNaive where

import Data.List
import Data.Ord
import Control.Arrow
import qualified Data.Tree as T
import Data.Maybe(fromJust)
import Tree

data ATree a = ATree  (T a) Int [(a,R)] deriving Show

reRootATree :: Eq a => a -> ATree a -> ATree a
reRootATree x (ATree t n aux) = let
    Just path = lookup x aux
    (t',f) = reRoot t path
    in ATree t' n $ map (second f) aux

mkATree :: T a -> ATree a
mkATree t = ATree t (count t) $ mkAux [] t

mkAux :: R -> T a -> [(a,R)]
mkAux p (T x []) = [(x,reverse p)]
mkAux p (T x ts) =  (x,reverse p) : (zip [0..] ts >>= \(i,t) -> mkAux (i:p) t)

link' :: Eq a =>  ATree a -> a -> ATree a -> a -> ATree a
link' (ATree tf nf af) (unsafeLookup af -> rf) atc xc = 
        ATree tf' (nf + nc) $ af ++ ac'
    where
    insert [] (T x ts) = (length ts, T x $ ts ++ [tc])
    insert (p:ps) (T x ts) = let 
        (r,c) = pick p ts
        in second (T x . r . Just) $ insert ps c
    (p',tf') = insert rf tf
    ac' = map (second ((rf ++ [p']) ++)) ac
    ATree tc nc ac = reRootATree xc atc

link a1@(ATree _ n1 _) x1 a2@(ATree _ ((< n1) -> True) _) x2 = link' a1 x1 a2 x2
link a1 x1 a2 x2 = link' a2 x2 a1 x1

cut :: Eq a => ATree a -> a -> (ATree a, ATree a)
cut (ATree tf nf af) (unsafeLookup af -> rf) = 
    (ATree tf' (nf - nc) af'', ATree tc nc ac') where
        (ac,af') = partition (isPrefix rf . snd) af
        ac' = map (second $ drop (length rf)) ac
        nc = count tc
        delete [] t = (Nothing,t)
        delete (p:ps) (T x ts) = let
            (r,c) = pick p ts
            in first (Just . T x . r) $ delete ps c
        (Just tf',tc) = delete rf tf
        af'' = map (second $ shrinkPath rf) af'

printTree (ATree t n a) = do
    printTreeS  t
    print n
    mapM_ (\(x,p) -> putStrLn $ unwords [x,show p]) a


