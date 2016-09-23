{-# language MultiParamTypeClasses#-}
{-# language ViewPatterns#-}
{-# language FlexibleInstances#-}
{-# language DataKinds#-}
{-# language GADTs#-}

{-# language UndecidableInstances #-}
-- | managing a forest of Tours
module HDynTs.EulerTours.Forest (TourForest) where

import Data.Monoid (Sum (..))
import Control.Arrow (second)
import Data.Foldable (toList)
import Data.FingerTree (FingerTree, Measured (measure), (<|), fromList)
import Data.Tree (Tree)
import Control.Monad.State (gets,put, MonadState)
import Data.Types.Isomorphic (Iso, Injective (to))
import Control.Monad (guard)
import Control.Applicative  ((<|>))


import HDynTs.Lib.FingerTree (select)
import HDynTs.EulerTours.Core (Tour, father, splice,extract, TourMonoid , 
    tourMonoid, reroot,tmPosition, tmMember, fromTree,toTree)
import HDynTs.Interface (GraphInterface (gQuery), 
    GraphQueryT (GQConnected,GQLink, GQDelete), GraphQueryExc (..), 
    GraphQuery (..))


-- | A forest of Tours
newtype TourForest a = TourForest (FingerTree (TourMonoid a) (Tour a))

---------------------------------
----------- graph based interface -----
---------------------------------

link :: Ord a => a -> a -> TourForest a 
    -> Either (GraphQueryExc a GQLink)  (TourForest a)
link x y (TourForest h) = case select (tmMember x) h of
        Nothing -> Left $ VertexNotFound x
        Just (ex,h') -> case tmMember y $ tourMonoid ex of
            True -> Left $ AlreadyConnectedVerteces x y
            False -> case  select (tmMember y) h' of
                Nothing -> Left $ VertexNotFound y
                Just (ey,h'') -> 
                    TourForest <$> return (splice (reroot x ex) y ey <| h'')

delete :: Ord a => a -> a -> TourForest a 
    -> Either (GraphQueryExc a GQDelete) (TourForest a)
delete x y (TourForest h) = case select (tmMember x) h of
        Nothing -> Left $ VertexNotFound x
        Just (ex,h') -> case  select (tmMember y) h  of
            Nothing -> Left $ VertexNotFound y
            Just _ -> let 
                check k h = (h,ex) <$ (father h ex >>= guard . (== k))
                in case check x y <|> check y x of 
                    Nothing -> Left $  AlreadySeparatedVerteces x y
                    Just (uncurry extract -> (e1,e2)) -> 
                        TourForest <$> (Right $ e1 <| e2 <| h')

connected :: Ord a => a -> a -> TourForest a 
    -> Either (GraphQueryExc a GQConnected) Bool
connected x y h = case link x y h of
    Right _ -> Right False
    Left (AlreadyConnectedVerteces _ _ ) -> Right True
    Left (VertexNotFound x) -> Left (VertexNotFound x)


catchM :: MonadState s f => Either a s -> f (Either a ())
catchM (Right g) =  Right <$> put g
catchM (Left e) = Left <$> return e


instance Ord a => Injective (TourForest a) [Tree a] where
    to (TourForest t) = map toTree . toList $ t

instance Ord a => Injective [Tree a] (TourForest a) where
    to  = TourForest . fromList . map fromTree 

instance Ord a => Iso [Tree a] (TourForest a)
instance (Monad m, MonadState (TourForest a) m, Ord a) => GraphInterface m TourForest a where
    gQuery (Link x y) = gets (link x y) >>= catchM
    gQuery (Delete x y) = gets (delete x y) >>= catchM
    gQuery (Connected x y) = gets (connected x y) 
