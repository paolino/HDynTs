
{-# language MultiParamTypeClasses, TemplateHaskell, ScopedTypeVariables, 
    ViewPatterns, FlexibleInstances,DeriveFunctor, StandaloneDeriving, 
    GADTs,
    NoMonomorphismRestriction, FlexibleContexts #-}

import Data.List
import Data.Tree hiding (Forest)
import Control.Monad.State hiding (modify)
import System.Console.Haskeline hiding (Exception)
import Control.Monad.Cont
import Data.Foldable
import Test.QuickCheck (sample')
import Data.Types.Isomorphic (to, Injective, Iso)
import Data.Proxy

import HDynTs.Lib.Tree (arbitraryForest)
import HDynTs.Interface

import HDynTs.EulerTours.Forest
import Data.Char

data Lang a =  L a a | D a a | P a a | N | S | I a deriving Read
doc = "l x y: link x and y verteces\nd x y: unlink x and y verteces\np x y : compute path between x and y\nn: new random forest\ni x: spanning tree of x\ns: ouput the forest\nCTRL-d: exit\n"

news :: (Injective [Tree Int] (t Int) , MonadIO m) => m (t Int)
news = to . head <$> (liftIO . sample' $ arbitraryForest 2 4)

errore x = "ERROR: " ++ x ++ "\n"
parseErrors :: Show a => Exception a b -> String
parseErrors (AlreadySeparatedVerteces x y) = 
    "verteces " ++ show x ++ " " ++ show y ++ " are not linked"
parseErrors (AlreadyConnectedVerteces x y) = 
    "verteces " ++ show x ++ " " ++ show y ++ " are in the same graph" 
parseErrors (VertexNotFound x) = 
    "vertex " ++ show x ++ " was not found in the forest"
parseErrors (OrException e1 e2) = parseErrors e1 ++ " or " ++ parseErrors e2
parseErrors (NotConnectedVerteces x y) = 
    "verteces " ++ show x ++ " " ++ show y ++ " are in different graphs" 

report x = "RESULT: " ++ x ++ "\n" 

catchErrorM :: (MonadIO m ,Show a) => (String -> m ()) -> (r -> m ()) 
    -> Either (Exception a b) r -> m ()
catchErrorM f g = either (f . errore . parseErrors) g 

run  :: forall t . (Show (t Int),
        Interface t Int, 
        Iso [Tree Int] (t Int)
        ) 
        => Proxy (t Int) 
        -> IO ()
run _ = (news :: IO (t Int)) >>= runInputT defaultSettings . 
        evalStateT (runContT loop return)

help = lift . lift . outputStrLn $ doc

out :: forall t. (Show (t Int), Injective (t Int) [Tree Int]) => String 
    -> StateT (t Int) (InputT IO) ()
out x =     do
                let g t = do
                        putStrLn . drawTree . fmap show $ t
                get >>= \xs -> do
                    liftIO . mapM_ g .(to :: t Int -> [Tree Int]) $ xs
                lift . outputStrLn $ x

loop :: forall t . (Show (t Int),
        Interface t Int, 
        Iso [Tree Int] (t Int)
        ) 
        => ContT () (StateT (t Int) (InputT IO)) ()

loop = do
    callCC $ \stop -> do
        let gl = do 
                mr <- lift . lift $ getInputLine "> "
                lift . lift $ outputStrLn ""
                return mr
            
        let u   Nothing = stop ()
            u   (Just (map toUpper -> r)) = do
                case reads r of 
                    [(L x y,_)] -> lift $ modify (Link x y) >>= 
                        catchErrorM  out return
                    [(D x y,_)] -> lift $ modify (Delete x y) >>= 
                        catchErrorM  out  return
                    [(P x y,_)] -> lift $ get >>= return . query (Path x y) >>= 
                        catchErrorM  out
                        (lift . outputStrLn . report . show)
                    [(S,_)] -> lift $ out ""
                    [(N,_)] -> lift (news >>= put) >> lift (out "")
                    [(I x,_)] -> lift $ get >>= return . query (Spanning x) >>= 
                         catchErrorM out 
                            (liftIO . putStrLn . drawTree . fmap show)  
                            
                    _ -> help
                lift . lift $ outputStrLn 
                    " ----------- ~~~~~~~~~~~~~ ----------"
                gl >>= u
        help >> gl >>= u


        
main = run (Proxy :: Proxy (TourForest Int))
