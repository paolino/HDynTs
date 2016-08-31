import Data.List
import Data.Ord

data T a = T a [T a]

type P a = [a]

paths :: T a -> [P a]
paths (T x []) = [[x]]
paths (T x xs) = let p:ps = sortBy (comparing length) $ xs >>= paths in (x:p):ps
