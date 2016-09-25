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

-- | modification types
data Modification = LinkT | CutT

-- | query types
data Queries = PathT | SpanningT

-- | query language
data Lang a b r where
    -- | ask to link two vertices
    Link :: a -> a -> Lang a (Modify LinkT) ()
    -- | ask to remove the link between two verteces
    Cut :: a -> a -> Lang a (Modify CutT) ()
    -- | ask the path between 2 verteces 
    Path :: a -> a -> Lang a (Query PathT) [a]
    -- | aske the spanning tree from a vertex
    Spanning :: a -> Lang a (Query SpanningT) (Tree a)

data Exception a b where
    -- | two verteces are part of the same graph, loop avoiding
    AlreadyConnectedVerteces :: a -> a -> Exception a (Modify LinkT)
    -- | trying to unlink two verteces not linked
    AlreadySeparatedVerteces :: a -> a -> Exception a (Modify CutT)
    -- | a vertex was not found
    VertexNotFound :: a -> Exception a b 
    -- | 2 verteces  are not connected
    NotConnectedVerteces :: a -> a -> Exception a (Query PathT)
    -- | more than one exception condition could have happened
    OrException :: Exception a b  -> Exception a b -> Exception a b


-- | query interface for algorithms
class Interface t a where
    -- | answer to the queries modifying the structure in the state 
    modify  :: (Monad m, MonadState (t a) m)  
        =>  Lang a (Modify c) ()  -- ^ query 
        -> m (Either (Exception a (Modify c)) ()) -- ^ result or failing
    query   :: Lang a (Query c) r -> t a -> Either (Exception a (Query c)) r

-- | pure link 
link :: Interface t a => a -> a -> t a -> Either (Exception a (Modify LinkT)) (t a)
link x y t = let 
    (v,t') = runState (modify (Link x y)) t
    in const t <$> v
    
-- | pure delete 
cut :: Interface  t a => a -> a -> t a -> Either (Exception a (Modify CutT)) (t a)
cut x y t = let 
    (v,t') = runState (modify (Cut x y)) t
    in const t <$> v

