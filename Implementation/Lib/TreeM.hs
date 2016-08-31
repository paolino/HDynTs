
{-# language ViewPatterns, DeriveFunctor #-}

import Data.List
import Data.Ord
import Control.Arrow
import qualified Data.Tree as T
import qualified Data.IntMap as M 


-- type Up a = M.Map a (Int,a)
type L a = M.IntMap a

data T a = T a (L (T a)) deriving (Show, Functor)

convert :: (Maybe Int,T a) -> T.Tree (Maybe Int,a)
convert (n,T x xs) = T.Node (n,x) $ map (convert . first Just) $ M.assocs xs

drawT :: ((Maybe Int,a) -> String) -> T a -> String
drawT f = T.drawTree . fmap f . convert . (,) Nothing


data A a = D | C a | A a

pick :: Int ->  L a -> (A a -> (L a, Int) , a)
pick n xs = let 
    (succ -> m) = fst $ M.findMax xs 
    x = xs M.! n
    f D = (xs,n)
    f (C x) = (M.insert n x xs, n)
    f (A x) = (M.insert m x xs, m)
    in (f , x)
 
type R = [Int]
reRoot = reRoot' id where
    reRoot' :: (T a -> T a) -> T a -> R -> T a
    reRoot' f t [] = f t
    reRoot' f (T x ts) (p:ps) = let 
        (r , c) = pick p ts
        g (T y ts) =  T y $ T x ts' : ts
        in reRoot' g c ps

{-
cut :: T a -> R -> (T a ,T a)
cut (T x ts) [p] = first (T x . ($ Nothing)) $ pick p ts
cut (T x ts) (p:ps) = let 
    (r, c) = pick p ts
    in first (T x . r . Just) . cut c $ ps

link :: T a -> R -> T a -> R -> T a
link t r (T x ts) [] = T x $ reRoot  t r : ts
link t r (T x ts) (p:ps) = let
    (s,c) = pick p ts
    in T x . s . Just $ link t r c ps 

type P a = [a]
paths :: T a -> [P a]
paths (T x []) = [[x]]
paths (T x xs) = (x:p):ps where
    p:ps = sortBy (comparing length) $ xs >>= paths 
-}

    
