{-# language MultiParamTypeClasses, FlexibleInstances #-}


import Data.Bits
import Data.Word
import Numeric
import Data.Char
import Data.FingerTree
import System.Random (randomRIO)
import Control.Monad
import qualified Data.FingerTree as F
import  Data.FingerTree (FingerTree, split, measure, Measured)
import qualified Data.Set as S
import Data.Set (Set)



newtype Collect a = Collect a deriving Show

instance Ord a => Measured (Set a) (Collect a) where
    measure (Collect x) = S.singleton x

------------------------------

output x = putStrLn $ showIntAtBase 2 intToDigit x ""


randomBits n m = replicateM m $ foldr (flip setBit) zeroBits <$>  (replicateM n (randomRIO (0,63)))

hash n m = Hash <$> randomBits n m
hashIt n m x = Hashed x <$> hash n m 

hashThem n m xs = mapM (hashIt n m) xs

newtype Hash = Hash [Word64] deriving (Eq,Show)

instance Monoid Hash where
    mempty = Hash $ repeat 0
    mappend (Hash h1) (Hash h2) = Hash $ zipWith (.|.) h1 h2

data Hashed a = Hashed {
    value :: a,
    hashV :: Hash
    } deriving Show


instance Measured Hash (Hashed a) where
    measure (Hashed _ h) = h

test (Hash h1) (Hash h2) = zipWith (.&.) h1 h2 == h1
membership (Hashed _ h) h' = test h h'

main = do
    let ft = fromList $ map Collect [0..1000]
    mapM_ print $ map (\x -> length . fst .split (S.member x) $ ft) [0..1000]
    
