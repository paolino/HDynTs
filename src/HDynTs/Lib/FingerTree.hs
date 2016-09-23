module HDynTs.Lib.FingerTree where

import Data.FingerTree
import Data.Monoid ((<>))
-- | extract an elem from FT

selectFT :: Measured m a => (m -> Bool) -> FingerTree m a -> Maybe (a, m, FingerTree m a)
selectFT c f = let
    (bs,as) = split c f
    in case viewl as of
        EmptyL -> Nothing
        x :< as' -> Just (x,measure bs, bs <> as')

