
{-# language ViewPatterns, FlexibleInstances, MultiParamTypeClasses, TypeFamilies #-}

module Implementation.Naive where

import qualified Data.Map as M
import qualified Data.Set as S

import Control.Arrow hiding (right)
import Data.List hiding (delete)
import Data.Ord
import Data.Function
import Control.Monad
import System.IO
import DynTClass

--  graph undirected model, 'a' is vertex
type Gr a = M.Map a (S.Set a)

-- insert an edge
addEdge :: Ord a => Edge a -> Gr a -> Gr a
addEdge (x,y) = f x y . f y x  where
    f x y = M.insertWith S.union x (S.singleton y)

--------------------------------------
-- optimized building a graph from edges, taking both directions 
-- for each edge.
-- equivalent fo *foldl addEdge M.empty*
------------------------------------------
mkGr :: Ord a => [Edge a] -> Gr a
mkGr =     (\xs -> xs ++ map swap xs) >>>
            sortBy (comparing fst) >>>
            groupBy ((==) `on` fst) >>> 
            map (fst . head &&& S.fromList . map snd) >>> 
            M.fromList 
        where swap (x,y) = (y,x)

-- Check both vertex of an edge are part of one graph, bfs
connectedGr :: Ord a => Edge a -> Gr a -> Bool
connectedGr (x,y) m = connected' (S.singleton x) (S.singleton x) where
    connected' s st = case  S.minView s of
        Nothing -> False
        Just (k,s') -> case M.lookup k m of
            Just s'' -> let  
                s''' = s'' `S.difference` st
                in if y `S.member` s''' then True 
                    else connected' (s' `S.union` s''') (st `S.union` s''')
            Nothing -> False

-- link implementation
linkGrs :: Ord a => Edge a -> [Gr a] -> [Gr a]
linkGrs l@(x,y) ms = let
    ([mx],ms') = partition (x `M.member`) ms
    ([my],ms'') = partition (y `M.member`) ms'
    in addEdge l (M.union mx my) : ms''

-- cut implementation
cutGrs :: Ord a => Edge a -> [Gr a] -> [Gr a]
cutGrs (x,y) ms = let 
    ([mxy],ms') = partition (x `M.member`) ms
    mxy' = M.adjust (S.delete x) y mxy 
    mxy'' = M.adjust (S.delete y) x mxy' 
    sx = mxy M.! x
    (mx,my) = M.partitionWithKey (\k _ -> k `S.member` sx || k == x) mxy''
    in mx:my:ms'


-------------------------------------------
----- DynT instance (verbatim)  -----------
-------------------------------------------


newtype Naive a = Naive {unNaive :: Gr a}

withNaives f (map unNaive -> ys) = map Naive $ f ys

instance Ord a => DynT Naive a where
    type Coll Naive = []
    link = withNaives . linkGrs 
    cut = withNaives . cutGrs 
    connected e = any (connectedGr e) . map unNaive
    create = map $ Naive . mkGr


