
{-|
Module      : HDynTs.Lib.FingerTree
Description : Additional functionality for 'Data.FingerTree'
Copyright   : (c) Paolo Veronelli, 2016
License     : BSD
Maintainer  : paolo.veronelli@gmail.com
Stability   : experimental
-}

{-# language MultiParamTypeClasses#-}
{-# language TypeFamilies#-}
{-# language ViewPatterns#-}
{-# language FlexibleInstances#-}
module HDynTs.Lib.FingerTree where

import Data.FingerTree hiding (split)
import qualified Data.FingerTree as F
import Data.Monoid ((<>))
import HDynTs.Lib.Access

-- | extract an elem from a fingertree, giving out the element and the 
-- orphaned fingertree. It fails when the element is missing
select   :: Measured m a 
            => (m -> Bool)  -- ^ selecting predicate, first True from the left
            -> FingerTree m a  -- ^ finger tree to be orphaned
            -> Maybe (a,FingerTree m a) -- ^ selected and the rest
select c f = let
    (bs,as) = split c f
    in case viewl as of
        EmptyL -> Nothing
        x :< as' -> Just (x,bs <> as')

-- should move to Lib


instance Measured v a => Split (FingerTree v) a where
    type SplitP (FingerTree v) a = v -> Bool
    split = F.split

-- | classical element wise adjust operation for 'FingerTree'
--  should go in a class ?

instance Measured v a => Adjust (FingerTree v) a where
    type AdjustP (FingerTree v) a = v -> Bool
    adjust c f = case split c f of
        (b1,viewl -> x :< b2) 
            -> Just (x,\r -> b1 <> (maybe id (<|) r) b2)
        (b1,viewl -> EmptyL) -> Nothing
