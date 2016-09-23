-- | Extension to the Data.FingerTree
module HDynTs.Lib.FingerTree where

import Data.FingerTree
import Data.Monoid ((<>))

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

