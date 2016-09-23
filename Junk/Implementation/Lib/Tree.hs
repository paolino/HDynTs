{-# language ViewPatterns, DeriveFunctor #-}

module Tree where

import Data.List (delete)
import Control.Arrow ((&&&), second)
import qualified Data.Tree as T
import Lib

data T a = T a [T a] deriving (Show, Functor)

count :: T a -> Int
count (T _ []) = 1
count (T _ ts) = 1 + sum (map count ts)

type P a = [a]

type R = [Int]

shrinkPath :: R -> R -> R
shrinkPath _ [] = []
shrinkPath [t] (x:xs) 
    | x > t = (x - 1):xs
    | otherwise = x:xs
shrinkPath (t:ts) (x:xs)
    | t == x = x : shrinkPath ts xs
    | otherwise = x:xs
 
reRoot :: T a -> R -> (T a, R -> R)
reRoot (T x ts) (p:ps) = (t'', correct)   where 
    (T x . ($ Nothing) -> t',y) = pick p ts
    (t'',(cs,acc))  = reRootCore t' y ps [p]
    mbs =  map (id &&&  reverse . (: acc) . g) $ delete p [0 .. length ts - 1]
    g n | n < p = n
        | otherwise = n - 1
    correct [] = reverse acc
    correct xs = prefixSubst mbs cs xs

reRootCore :: T a -> T a -> R -> R -> (T a, ((R,[R]),R))
reRootCore t' (T x ts) [] rs = (T x $ ts ++ [t'], 
        ((reverse rs,[[]]), [length ts]))
reRootCore t' (T x ts) (p:ps) rs = let 
    (T x . ($ Just t') -> t'', c) = pick p ts
    h ((rps, cs),acc) = ((rps , reverse acc : cs) , p:acc) 
    in second h $ reRootCore t'' c ps (p:rs)

printTreeS = putStrLn . drawT id 

convert :: T a -> T.Tree a
convert (T x xs) = T.Node x $ map convert xs

drawT :: (a -> String) -> T a -> String
drawT f = T.drawTree . convert . fmap f

