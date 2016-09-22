{-# language MultiParamTypeClasses, TemplateHaskell, ScopedTypeVariables, 
    ViewPatterns, FlexibleInstances,DeriveFunctor, StandaloneDeriving, 
    NoMonomorphismRestriction, FlexibleContexts #-}

import Data.List
import Data.Tree hiding (Forest)
import Control.Monad.State
import System.Console.Haskeline
import Control.Monad.Cont
import Data.Foldable

import ET
import Tree (arbitraryTree, relabelForest)
import Test.QuickCheck (sample')
import Data.FingerTree (fromList)




data Lang a = R | L a a | C a | RR a | S deriving Read
doc = "R | L x y | C x | S where Read a => x :: a\n"

news = do
    x:y:_ <- liftIO $ fmap (map fromTree . relabelForest) .sample' 
        $ arbitraryTree 4
    return $ fromList [x,y]

play :: IO ()
play = do
    f <- news
    runInputT defaultSettings . flip evalStateT f $ runContT r return where
    r = callCC $ \stop -> forever $ do
            mr <- lift . lift $ getInputLine "> "
            lift . lift $ outputStrLn ""
            case mr of
                Nothing -> stop ()
                Just r -> lift $ case reads r of 
                    [(R,_)] -> do
                        news >>= put 
                    [(L x y,_)] -> do
                            f <- get 
                            case link x y f of
                                Nothing -> lift $ outputStrLn 
                                    "error"
                                Just f -> put f
                            
                    [(C x,_)] -> do
                            f <- get 
                            case cut x f of
                                Nothing -> lift $ outputStrLn 
                                    "error"
                                Just f -> put f
                    _ -> lift . outputStrLn $ doc
            get >>= liftIO . 
                        mapM_ (putStrLn . drawTree . fmap show . toTree) 
                        . toList 
            lift . lift $ outputStrLn " ---~~~~~~~~~~~~~----------"
        

    

