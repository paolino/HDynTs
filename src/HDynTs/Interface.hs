{-# language GADTs#-}
{-# language DataKinds#-}
{-# language MultiParamTypeClasses#-}
{-# language FunctionalDependencies#-}
{-# language FlexibleContexts #-}

-- | Classes for dynamic trees implementation. 
module HDynTs.Interface where

import Control.Monad.State (Monad, MonadState, runState, State, evalState )
import Data.Tree (Tree)

-- | graph query distintion at type level
data GraphQueryT = GQLink | GQDelete | GQConnected

-- | query language
data GraphQuery a r b where
    -- | ask to link two vertices
    Link :: a -> a -> GraphQuery a () GQLink
    -- | ask to remove the link between two verteces
    Delete :: a -> a -> GraphQuery a () GQDelete
    -- | ask if two verteces are connected 
    Connected :: a -> a -> GraphQuery a Bool GQConnected

-- | query language answer exceptions
data GraphQueryExc a b where
    -- | a vertex was not found
    VertexNotFound :: a -> GraphQueryExc a b
    -- | tried  to link two verteces already inside a graph, loop introduction
    AlreadyConnectedVerteces :: a -> a -> GraphQueryExc a GQLink
    -- | trying to unlink two verteces not linked
    AlreadySeparatedVerteces :: a -> a -> GraphQueryExc a GQDelete

-- | graph query interface for implementations
class GraphInterface m t a where
    -- | answer to the queries modifying the structure in the state 
    gQuery  :: (Monad m, MonadState (t a) m)  
        =>  GraphQuery a r b  -- ^ query 
        -> m (Either (GraphQueryExc a b) r) -- ^ result or failing

-- | Inspectable structures can map an element to the tree containing it
-- where they are the root (spanning tree)
class Spanning t a where
    spanning :: a -> t a -> Maybe (Tree a)

-- | pure link 
link :: GraphInterface (State (t a)) t a => a -> a -> t a -> Either (GraphQueryExc a GQLink) (t a)
link x y t = let 
    (v,t') = runState (gQuery (Link x y)) t
    in const t <$> v
    
-- | pure delete 
unlink :: GraphInterface (State (t a)) t a => a -> a -> t a -> Either (GraphQueryExc a GQDelete) (t a)
unlink x y t = let 
    (v,t') = runState (gQuery (Delete x y)) t
    in const t <$> v

-- | pure connected
connected :: GraphInterface (State (t a)) t a => a -> a -> t a -> Either (GraphQueryExc a GQConnected) Bool
connected x y t = evalState (gQuery (Connected x y)) t

   
