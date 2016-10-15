{-# language FlexibleInstances #-}
{-# language ScopedTypeVariables #-}
{-# language MultiParamTypeClasses #-}
{-# language GADTs #-}
{-# language ViewPatterns #-}

module HDynTs.ST.Forest where

import Control.Lens (over,set,view)
import Data.Types.Isomorphic (Iso, Injective (to))
import Data.Tree (Tree)
import Data.Foldable (toList)
import Data.FingerTree (fromList, ViewR (..), ViewL (..), viewr)
import Control.Monad.State (put,get)
import HDynTs.Lib.Access (adjust)
import HDynTs.Lib.Paths (P(P))
import HDynTs.ST.Core (PathForest,APath (APath),Path (Path,Root)
    , reversable, link, top, Reversable (..), 
    memberPathForest , reroot, Elem (..), cut)
import qualified HDynTs.ST.Core as C
import HDynTs.Interface (Interpreter (modify,query),Modification (..), 
    Queries (..), Exception (..), Modify , Query, Lang (..) , catchM)

instance Ord a => Injective [Tree a] (PathForest a) where
    to  = mconcat . map pathsOne where
        pathsOne :: Ord a => Tree a -> PathForest a
        pathsOne t = let
            (P Nothing p):ps = to [t]
            in fromList $ APath (Root $ reversable p): 
               map (\(P (Just f) p) -> APath (C.Path f $ reversable p)) ps 

instance Ord a => Injective (PathForest a) [Tree a] where
    to = to . map f . toList where
        f :: APath a -> P a
        f (APath (Root p)) = P Nothing (toList p)
        f (APath (C.Path f p)) = P (Just f) (toList p) 

newtype STForest a = STForest (PathForest a)
instance Ord a => Interpreter STForest a where
    modify (Link x y)   = do
        STForest t <- get  -- get the forest
        catchM $ case top x t of
            Nothing -> Left (VertexNotFound x)
            Just t1 -> case top y t of
                Nothing -> Left (VertexNotFound y)
                Just t2 -> case t1 == t2 of
                    True -> Left (AlreadyConnectedVerteces x y)
                    False -> Right . STForest $ link x y t
    modify (Cut x y)    = do
        STForest t <- get  -- get the forest
        catchM $ case adjust (memberPathForest x) t of
            Nothing -> Left (VertexNotFound x)
            Just _ -> let t' = reroot id x t in
                case adjust (memberPathForest y) t' of 
                    Nothing -> Left (VertexNotFound y)
                    Just (APath (C.Path p (Reversable (viewr -> _ :> Elem r) _)),_) ->
                        case r == y && p == x of
                            True -> Right . STForest $ cut y t'
                            False -> Left (AlreadySeparatedVerteces x y)
                    Just (APath (Root (Reversable 
                        (viewr -> (viewr -> _ :> Elem s) :> _) _)),_) -> 
                            case s == y of
                                True -> Right .STForest $ cut y t'
                                False -> Left (AlreadySeparatedVerteces x y)

--     query (Spanning x)  = spanning x 
--    query (Path x y)    = fpath x y


