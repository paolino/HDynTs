{-# language ViewPatterns #-}
{-# language TypeFamilies #-}
{-# language FlexibleInstances #-}
{-# language MultiParamTypeClasses#-}
{-# language DataKinds#-}
{-# language GADTs#-}
{-# language StandaloneDeriving#-}
{-# language DeriveFoldable#-}
{-# language RankNTypes #-}
{-# language ScopedTypeVariables #-}
{-# language TemplateHaskell #-}
{-# language FlexibleContexts #-}
-- {-# #-}

module HDynTs.Lib.Paths where

import Control.Lens (over,set,view)
import Control.Lens.TH (makeLenses)
import Data.Tree (Tree(..))
import Control.Monad.State (gets,put, evalState,State)
import Data.Types.Isomorphic (Iso, Injective (to))
import Data.List (sortBy,partition)
import Data.Ord (comparing)
import Data.Maybe (isNothing)
import Control.Monad (forM)


-- |  An unencoded path
data P a = P {
    _father :: Maybe a,
    _path :: [a]
    }

makeLenses ''P

fromTree :: Tree a -> [P a]
fromTree (Node x []) = [P Nothing [x]]
fromTree (Node x xs) = (over path (x:) p) : map (set father $ Just x) ps where
    p:ps = sortBy (comparing $ length . view path) $ xs >>= fromTree



toTree :: Eq a => [a] -> State [P a] (Tree a)
toTree [x] = return $ Node x []
toTree (x:xs) = do
    let match Nothing = False
        match (Just y) = y == x
    (cs,rs) <- gets $ partition (match . view father)
    put rs
    cs' <- mapM (toTree . view path) cs
    Node x <$> (:cs') <$> toTree xs

instance Ord a => Injective [P a] [Tree a] where
    to  = evalState $ do
    (as, rs) <- gets $ partition (isNothing . view father) 
    put rs
    forM as $ \(P Nothing xs) -> toTree xs

instance Ord a => Injective [Tree a] [P a] where
    to ts = ts >>= fromTree 

instance Ord a => Iso [Tree a] [P a]

