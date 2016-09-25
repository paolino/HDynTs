{-# language GADTs#-}
{-# language TypeFamilies#-}
{-# language DataKinds#-}
{-# language MultiParamTypeClasses#-}
{-# language FunctionalDependencies#-}
{-# language FlexibleContexts #-}
{-# language FlexibleInstances #-}
{-# language TypeSynonymInstances #-}

{-|
Module      : HDynTs.Interface
Description : Common interface for dynamic trees algorithms
Copyright   : (c) Paolo Veronelli, 2016
License     : BSD
Maintainer  : paolo.veronelli@gmail.com
Stability   : experimental

= Classes for dynamic trees algorithms. 

== This module defines the /interface/ for algorithms. 

All algorithms can be used via the 'Interpreter' class. 

'Path' query should produce the same answer for all, given the same initial state 
and the same sequence of modifications.

'Spanning' query should produce the same answer up-to children reordering

-}

module HDynTs.Interface (
    -- * Types
    Modify,
    Query,
    -- * Promoted types
    Modification (..),
    Queries (..) ,
    -- * Language
    Lang (..),
    Exception (..),
    Interpreter (..),
    -- * Helpers
    pureModify
    )
    

where

import Control.Monad.State (Monad, MonadState, runState, State, evalState )
import Data.Tree (Tree)


-- | modification tagger
data Modify (a :: Modification) 
-- | query tagger
data Query (a :: Queries)

-- | modification types
data Modification = LinkT | CutT

-- | query types
data Queries = PathT | SpanningT

-- | Generic query language for algorithms
data Lang a b r where
    -- | link two vertices
    Link :: a -> a -> Lang a (Modify LinkT) ()
    -- | remove the link between two verteces
    Cut :: a -> a -> Lang a (Modify CutT) ()
    -- | compute the path between two verteces 
    Path :: a -> a -> Lang a (Query PathT) [a]
    -- | compute the spanning tree from a vertex
    Spanning :: a -> Lang a (Query SpanningT) (Tree a)

-- | Possible exception condition for Lang interpreters
data Exception a b where
    -- | two verteces are part of the same graph, loop avoiding exception
    AlreadyConnectedVerteces :: a -> a -> Exception a (Modify LinkT)
    -- | unlink two not linked verteces
    AlreadySeparatedVerteces :: a -> a -> Exception a (Modify CutT)
    -- | a vertex was not found
    VertexNotFound :: a -> Exception a b 
    -- | two verteces are not connected
    NotConnectedVerteces :: a -> a -> Exception a (Query PathT)
    -- | alternative exception conditions holds
    OrException :: Exception a b  -> Exception a b -> Exception a b

-- | Query interpreter for algorithms
class Interpreter t a where
    -- | answer to the queries modifying the structure in the state 
    modify  :: (Monad m, MonadState (t a) m)  
            => Lang a (Modify c) ()  
            -> m (Either (Exception a (Modify c)) ()) 
    -- | queries are pure functions from state
    query   :: Lang a (Query c) r  
            -> t a          
            -> Either (Exception a (Query c)) r 

-- | Pure modifications. Let you use the 'modify' function outside the 'MonadState' 
--
-- prop> to (pureModify (Link 1 2) (to [Node 1 [], Node [2]])) =&= [Node 2 [Node [1]]]
pureModify  :: Interpreter t a 
            => Lang a (Modify c) () -- ^ modification
            -> t a -- ^ state
            -> Either (Exception a (Modify c)) (t a) -- ^ exception or new state
pureModify f t = let 
    (v,t') = runState (modify f) t
    in const t <$> v



