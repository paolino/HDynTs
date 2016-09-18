{-# language MultiParamTypeClasses, TemplateHaskell, ScopedTypeVariables, 
    ViewPatterns, FlexibleInstances,DeriveFunctor, StandaloneDeriving, 
    NoMonomorphismRestriction, FlexibleContexts #-}
import Data.List
import Data.Tree hiding (Forest)
import Control.Monad.State
import System.Console.Haskeline
import Control.Monad.Cont
import Data.Foldable

import FT1



safeLink x y f@(connected x y -> False) =  link x y f
safeLink _ _ _ = Nothing


drawPath :: Show a => Path a -> String
drawPath (Path Nothing (IsoPath o _)) = show (map unCollect . toList $ o) 
    ++ " -> " ++ "ROOT" 
drawPath (Path (Just d) (IsoPath o _)) = show (map unCollect . toList $ o) 
    ++ " -> " ++ show d 

pt :: (Ord a, Show a) => Forest Path a ->  IO ()
pt f = do
    mapM_ (putStrLn. drawTree . fmap show . sortChildren) . 
        pathsToTrees $ f
    mapM_ (putStrLn . drawPath) $ f


data Lang a = L a a | C a | RR a | S deriving Read
doc = "L x y | C x | RR x | S where Read a => x :: a"

play :: forall a. (Ord a, Read a, Show a) =>  Forest Path a -> IO ()
play f = runInputT defaultSettings . flip evalStateT f $ runContT r return where
    r :: ContT () (StateT (Forest Path a) (InputT IO)) ()
    r = callCC $ \stop -> forever $ do
            mr <- lift . lift $ getInputLine "> "
            case mr of
                Nothing -> stop ()
                Just r -> lift $ case reads r of 
                    [(L x y,_)] -> do
                            f <- get 
                            case safeLink x y f of
                                Nothing -> lift $ outputStrLn 
                                    "nodes connected or vertex not found"
                                Just f -> put f
                            
                    [(C x,_)] -> do
                            f <- get 
                            case cut x f of
                                Nothing -> lift $ outputStrLn 
                                    "cutting a root or vertex not found"
                                Just f -> put f
                    [(RR x,_)] -> modify $ reroot x
                    [(S,_)] -> get >>= liftIO . pt
                    _ -> lift . outputStrLn $ doc
        
ex1 = treesToPaths [Node 1 [Node 2 [], Node 3 []], 
    Node 4 [Node 5[Node 7 []], Node 6[]]]


