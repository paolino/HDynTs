{-# language MultiParamTypeClasses#-}
{-# language ViewPatterns#-}
{-# language FlexibleInstances#-}
{-# language DataKinds#-}
{-# language GADTs#-}

{-# language UndecidableInstances #-}

-- | Managing a forest of Tours. A fingertree is holding the forest of Tours.
-- Selecting a Tour is sublinear in the number of verteces. The membership part
-- of the monoid is taken from the underlying fingertree for Tour.
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
import Data.Set (Set,member)


import HDynTs.Lib.FingerTree (select)
import HDynTs.EulerTours.Core (Tour, father, splice,extract ,
        reroot, fromTree,toTree, path)
import HDynTs.Interface (Interface (modify,query),Modification (..), Queries (..), Exception (..), Modify , Query, Lang (..) ) 


-- | A forest of Tours
newtype TourForest a = TourForest (FingerTree (Set a) (Tour a)) deriving Show

-------------------------------------------------------------------------------
-------------------- Graph based interface ------------------------------------
-------------------------------------------------------------------------------

link :: Ord a => a -> a -> TourForest a 
    -> Either (Exception a (Modify LinkT))  (TourForest a)
link x y (TourForest h) = case select (member x) h of
        Nothing -> Left $ VertexNotFound x
        Just (ex,h') -> case member y $ measure ex of
            True -> Left $ AlreadyConnectedVerteces x y
            False -> case  select (member y) h' of
                Nothing -> Left $ VertexNotFound y
                Just (ey,h'') -> Right . TourForest $
                        (splice (reroot x ex) y ey) <| h''

cut :: Ord a => a -> a -> TourForest a 
    -> Either (Exception a (Modify CutT)) (TourForest a)
cut x y (TourForest h) = case select (member y) h of
        Nothing -> Left $ VertexNotFound y
        Just (reroot y -> e,h') -> case father x e >>= guard . (== y) of
                    Nothing -> Left $  OrException (AlreadySeparatedVerteces x y) (VertexNotFound x)
                    Just () -> let (e1,e2) = extract x e in 
                        TourForest <$> (Right $ e1 <| e2 <| h')


fpath :: Ord a => a -> a -> TourForest a -> Either (Exception a (Query PathT)) [a]
fpath x y (TourForest h) = case select (member y) h of
        Nothing -> Left $ VertexNotFound y
        Just (e,_) -> case member x $ measure e of
            False -> Left $ OrException (NotConnectedVerteces x y) (VertexNotFound x)
            True -> Right (path x y e)

spanning :: Ord a => a -> TourForest a -> Either (Exception a (Query SpanningT)) (Tree a)
spanning x (TourForest f) = case select (member x) $ f of
    Nothing -> Left $ VertexNotFound x
    Just (e,_) -> Right $ toTree . reroot x $ e

catchM :: MonadState s f => Either a s -> f (Either a ())
catchM (Right g) =  Right <$> put g
catchM (Left e) = Left <$> return e


instance Ord a => Injective (TourForest a) [Tree a] where
    to (TourForest t) = map toTree . toList $ t
instance Ord a => Injective [Tree a] (TourForest a) where
    to  = TourForest . fromList . map fromTree 
instance Ord a => Iso [Tree a] (TourForest a)

instance Ord a => Interface TourForest a where
    modify (Link x y) = gets (link x y) >>= catchM
    modify (Cut x y) = gets (cut x y) >>= catchM
    query (Spanning x) = spanning x 
    query (Path x y) = fpath x y

