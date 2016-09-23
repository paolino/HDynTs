
{-# language MultiParamTypeClasses, TemplateHaskell, ScopedTypeVariables, 
    ViewPatterns, FlexibleInstances,DeriveFunctor, StandaloneDeriving, 
    GADTs,
    NoMonomorphismRestriction, FlexibleContexts #-}

import Data.List
import Data.Tree hiding (Forest)
import Control.Monad.State
import System.Console.Haskeline
import Control.Monad.Cont
import Data.Foldable
import Test.QuickCheck (sample')
import Data.Types.Isomorphic (to, Injective, Iso)
import Data.Proxy

import HDynTs.Lib.Tree (arbitraryTree, relabelForest)
import HDynTs.Interface

import HDynTs.EulerTours.Forest

data Lang a =  L a a | D a a | C a a | H deriving Read
doc = "L x y : link x and y verteces\nD x y : unlink x and y verteces\nC x y : check x and y are connected\nH :this help\nCTRL-D: exit\nall the rest will create a new forest\n"

news :: (Injective [Tree Int] (t Int) , MonadIO m) => m (t Int)
news = do
    x:y:_ <- liftIO $ fmap relabelForest .sample' $ arbitraryTree 4
    return $ to [x,y]

errore x = "ERROR: " ++ x ++ "\n"
parseErrors :: Show a => GraphQueryExc a b -> String
parseErrors (AlreadySeparatedVerteces x y) = 
    "verteces " ++ show x ++ " " ++ show y ++ " are not linked"
parseErrors (AlreadyConnectedVerteces x y) = 
    "verteces " ++ show x ++ " " ++ show y ++ " are in the same graph" 
parseErrors (VertexNotFound x) = 
    "vertex " ++ show x ++ " was not found in the forest"

report x = "RESULT: " ++ x ++ "\n" 
parseConnected x y t = "verteces " ++ show x ++ " " ++ show y ++ " are " ++ (if t then "" else "not ") ++ "connected"

catchErrorM :: (MonadIO m ,Show a) => (String -> m ()) -> (r -> m ()) 
    -> Either (GraphQueryExc a b) r -> m ()
catchErrorM f g = either (f . errore . parseErrors) g 

run  :: forall t . (
        GraphInterface (StateT (t Int) (InputT IO)) t Int, 
        Iso [Tree Int] (t Int)
        ) 
        => Proxy (t Int) 
        -> IO ()
run _ = (news :: IO (t Int)) >>= runInputT defaultSettings . 
        evalStateT (runContT loop return)

loop :: forall t . (
        GraphInterface (StateT (t Int) (InputT IO)) t Int, 
        Iso [Tree Int] (t Int)
        ) 
        => ContT () (StateT (t Int) (InputT IO)) ()
help stop = do 
    lift . lift . outputStrLn $ doc
    m <- lift . lift $ getInputLine ""
    case m of
        Nothing -> stop ()
        _ -> return ()
loop = do
    callCC $ \stop -> do
        help stop
        forever $ do
            get >>= liftIO . mapM_ (putStrLn . drawTree . fmap show) . 
                (to :: t Int -> [Tree Int])
            mr <- lift . lift $ getInputLine "> "
            lift . lift $ outputStrLn ""
            case mr of
                Nothing -> stop ()
                Just r -> case reads r of 
                    [(L x y,_)] -> lift $ gQuery (Link x y) >>= 
                        catchErrorM  (lift . outputStrLn) return
                    [(D x y,_)] -> lift $ gQuery (Delete x y) >>= 
                        catchErrorM  (lift . outputStrLn) return
                    [(C x y,_)] -> lift $ gQuery (Connected x y) >>= 
                        catchErrorM  (lift . outputStrLn) 
                        (lift . outputStrLn . report . parseConnected x y )
                    [(H,_)] -> help stop 
                    _ -> lift $ news >>= put 
            lift . lift $ outputStrLn " ----------- ~~~~~~~~~~~~~ ----------"
        
main = run (Proxy :: Proxy (TourForest Int))

