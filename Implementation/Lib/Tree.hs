
{-# language ViewPatterns, DeriveFunctor #-}

import Data.List
import Data.Ord
import Control.Arrow
import qualified Data.Tree as T
-- import qualified Data.Map as M 


-- type Up a = M.Map a (Int,a)

data T a = T a [T a] deriving (Show, Functor)

convert :: T a -> T.Tree a
convert (T x xs) = T.Node x $ map convert xs

drawT :: (a -> String) -> T a -> String
drawT f = T.drawTree . convert . fmap f

type P a = [a]

type R = [Int]

pick :: Int -> [a] -> (Maybe a -> [a] , a)
pick n xs = let 
    (bs,x:rs) = splitAt n xs
    in ((\f -> bs ++ f rs) . maybe id (:), x)
 
reRoot = reRoot' id where
    reRoot' :: (T a -> T a) -> T a -> R -> T a
    reRoot' f t [] = f t
    reRoot' f (T x ts) (p:ps) = let 
        (($ Nothing) -> ts', c) = pick p ts
        g (T y ts) =  T y $ T x ts' : ts
        in reRoot' g c ps

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

paths :: T a -> [P a]
paths (T x []) = [[x]]
paths (T x xs) = (x:p):ps where
    p:ps = sortBy (comparing length) $ xs >>= paths 


    
