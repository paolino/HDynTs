
{-# language ViewPatterns, DeriveFunctor #-}

import Data.List
import Data.Ord
import Control.Arrow
import qualified Data.Tree as T
-- import qualified Data.Map as M 


-- type Up a = M.Map a (Int,a)

data T a = T a [T a] deriving (Show, Functor)

type P a = [a]

type R = [Int]

pick :: Int -> [a] -> (Maybe a -> [a] , a)
pick n xs = let 
    (bs,x:rs) = splitAt n xs
    in ((\f -> bs ++ f rs) . maybe id (:), x)
 
type B = ((R,[R]),R)

prefixSubst :: [(Int,R)] -> (R,[R]) -> R -> R
prefixSubst = undefined

reRoot :: T a -> R -> (T a, R -> R)
reRoot (T x ts) (p:ps) =
    let     (T x . ($ Nothing) -> t',y) = pick p ts
            (t'',(cs,acc))  = reRootInside t' y ps [p]
            mbs =  map (id &&&  reverse . (: acc) . g) $ delete p [0 .. length ts - 1]
            g n | n < p = n
                | otherwise = n - 1
            correct [] = reverse acc
            correct xs = prefixSubst mbs cs xs
    in (t'', correct)


reRootInside :: T a -> T a -> R -> R -> (T a, B)
reRootInside t' (T x ts) [] rs = (T x $ ts ++ [t'], ((reverse rs,[[]]), [length ts]))
reRootInside t' (T x ts) (p:ps) rs = let 
    (T x . ($ Just t') -> t'', c) = pick p ts
    h ((rps, cs),acc) = ((rps , reverse acc : cs) , p:acc) 
    in second h $ reRootInside t'' c ps (p:rs)

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

paths :: T a -> [P a]
paths (T x []) = [[x]]
paths (T x xs) = (x:p):ps where
    p:ps = sortBy (comparing length) $ xs >>= paths 

-}
    
convert :: T a -> T.Tree a
convert (T x xs) = T.Node x $ map convert xs

drawT :: (a -> String) -> T a -> String
drawT f = T.drawTree . convert . fmap f

