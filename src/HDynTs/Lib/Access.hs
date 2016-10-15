{-# language MultiParamTypeClasses#-}
{-# language TypeFamilies#-}
-- {-# language #-}
module HDynTs.Lib.Access where
import Data.Maybe

class Split a b where
    type SplitP a b
    split :: SplitP a b -> a b ->  (a b, a b)


class Adjust a b where
    type AdjustP a b
    adjust :: AdjustP a b -> a b -> Maybe (b, Maybe b -> a b)

-- | correct an element 
unsafeAdjust
  :: Adjust a b => AdjustP a b -> a b -> (b, Maybe b -> a b)
unsafeAdjust c = fromJust . adjust c
