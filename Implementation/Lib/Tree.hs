
{-# language ViewPatterns, DeriveFunctor #-}

module Tree where

import Data.List
import Data.Ord
import Control.Arrow
import qualified Data.Tree as T
import Control.Applicative
import Data.Maybe(fromJust)

data T a = T a [T a] deriving (Show, Functor)

type P a = [a]

type R = [Int]

pick :: Int -> [a] -> (Maybe a -> [a] , a)
pick n xs = let 
    (bs,x:rs) = splitAt n xs
    in ((\f -> bs ++ f rs) . maybe id (:), x)
 
prefixSubst :: (Eq a) => [(a,[a])] -> ([a],[[a]]) -> [a] -> [a] 
prefixSubst xs (rs,zs) yt@(y:ys) = maybe f id $ lookup y ( map (second (++ ys)) xs ) 
  where
      f  = snd . last . takeWhile fst $ zipWith3 h rs yt ds
      h r y d = (r==y,d)
      ds = zipWith (++) zs . tail . tails $ yt

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

type B = ((R,[R]),R)

reRootInside :: T a -> T a -> R -> R -> (T a, B)
reRootInside t' (T x ts) [] rs = (T x $ ts ++ [t'], ((reverse rs,[[]]), [length ts]))
reRootInside t' (T x ts) (p:ps) rs = let 
    (T x . ($ Just t') -> t'', c) = pick p ts
    h ((rps, cs),acc) = ((rps , reverse acc : cs) , p:acc) 
    in second h $ reRootInside t'' c ps (p:rs)

count :: T a -> Int
count (T _ []) = 1
count (T _ ts) = 1 + sum (map count ts)

unsafeLookup xs x = fromJust $ lookup x xs

shrinkPath :: R -> R -> R
shrinkPath _ [] = []
shrinkPath [t] (x:xs) 
    | x > t = (x - 1):xs
    | otherwise = x:xs
shrinkPath (t:ts) (x:xs)
    | t == x = x : shrinkPath ts xs
    | otherwise = x:xs

isPrefix :: R -> R -> Bool
isPrefix [] _  = True
isPrefix _ []  = False
isPrefix (x:xs) (y:ys)  | x == y = isPrefix xs ys
                        | otherwise = False

printTreeS = putStrLn . drawT id 

convert :: T a -> T.Tree a
convert (T x xs) = T.Node x $ map convert xs

drawT :: (a -> String) -> T a -> String
drawT f = T.drawTree . convert . fmap f

