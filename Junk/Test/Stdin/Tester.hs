{-# language ScopedTypeVariables, FlexibleContexts #-}
module Test.Stdin.Tester where

import Control.Monad
import System.IO
import Interface

testStdin :: forall dynT . GraphInterface dynT Char => dynT Char -> IO ()
testStdin p = do
    n <- readLn
    ds :: Coll dynT (dynT Char) <- fmap create . replicateM n $ do
        m <- readLn
        replicateM m $ (\[x,y] -> (head x, head y)) <$> words <$> getLine
    let f rs ds = do
            t <- isEOF
            if not t then do
                c:[x]:[y]:r <- words <$> getLine
                let l = (x,y)
                case c of
                    "connected" -> f ((connected l ds == read (head r)):rs) ds
                    "link" -> f rs $ link l ds
                    "cut" -> f rs $ cut l ds
            else return rs
    f [] ds >>= mapM_ print . reverse

   
