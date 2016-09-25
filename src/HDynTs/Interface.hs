{-# language GADTs#-}
{-# language TypeFamilies#-}
{-# language DataKinds#-}
{-# language MultiParamTypeClasses#-}
{-# language FunctionalDependencies#-}
{-# language FlexibleContexts #-}
{-# language FlexibleInstances #-}
{-# language TypeSynonymInstances #-}

-- | Classes for dynamic trees implementation. 
module HDynTs.Interface where

import Control.Monad.State (Monad, MonadState, runState, State, evalState )
import Data.Tree (Tree)
import Data.Maybe



-- | graph modification distintion at type level
data Modify (a :: Modification) 
data Query (a :: Queries)
data Modification = LinkT | DeleteT
data Queries = PathT | SpanningT

-- | query language
data Lang a b r where
    -- | ask to link two vertices
    Link :: a -> a -> Lang a (Modify LinkT) ()
    -- | ask to remove the link between two verteces
    Delete :: a -> a -> Lang a (Modify DeleteT) ()
    -- | ask the path between 2 verteces 
    Path :: a -> a -> Lang a (Query PathT) [a]
    -- | aske the spanning tree from a vertex
    Spanning :: a -> Lang a (Query SpanningT) (Tree a)

data Exception a b where
        -- | tried  to link two verteces already inside a graph, loop introduction
    AlreadyConnectedVerteces :: a -> a -> Exception a (Modify LinkT)
    -- | trying to unlink two verteces not linked
    AlreadySeparatedVerteces :: a -> a -> Exception a (Modify DeleteT)
    -- | a vertex was not found
    VertexNotFound :: a -> Exception a b 
    -- | 
    NotConnectedVerteces :: a -> a -> Exception a (Query PathT)
    -- |
    OrException :: Exception a b  -> Exception a b -> Exception a b


-- | graph query interface for implementations
class Interface t a where
    -- | answer to the queries modifying the structure in the state 
    modify  :: (Monad m, MonadState (t a) m)  
        =>  Lang a (Modify c) ()  -- ^ query 
        -> m (Either (Exception a (Modify c)) ()) -- ^ result or failing
    query :: Lang a (Query c) r -> t a -> Either (Exception a (Query c)) r
-- | Inspectable structures can map an element to the tree containing it
-- where they are the root (spanning tree)
class Spanning t a where
    spanning :: a -> t a -> Maybe (Tree a)


-- | pure link 
link :: Interface t a => a -> a -> t a -> Either (Exception a (Modify LinkT)) (t a)
link x y t = let 
    (v,t') = runState (modify (Link x y)) t
    in const t <$> v
    
-- | pure delete 
unlink :: Interface  t a => a -> a -> t a -> Either (Exception a (Modify DeleteT)) (t a)
unlink x y t = let 
    (v,t') = runState (modify (Delete x y)) t
    in const t <$> v

