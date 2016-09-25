
{-# language MultiParamTypeClasses, TemplateHaskell, ScopedTypeVariables, 
    ViewPatterns, FlexibleInstances,DeriveFunctor, StandaloneDeriving, 
    GADTs,
    NoMonomorphismRestriction, FlexibleContexts, ConstraintKinds #-}

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

errore x = "## CONDITION: " ++ x ++ "\n"
report x = "## RESULT: " ++ x ++ "\n" 

-- error treatment
catchErrorM :: (MonadIO m ,Show a) 
    => (String -> m ()) -- error report callback
    -> (r -> m ()) -- result callback
    -> Either (Exception a b) r -- result
    -> m ()
catchErrorM f g = either (f . errore . parseErrors) g 

-- t constraints
type Env t = (Interface t Int, Iso [Tree Int] (t Int)) 

-- fresh populated structure
news :: (Env t, MonadIO m) => m (t Int)
news = to . head <$> (liftIO . sample' $ arbitraryForest 2 4)

help = lift . lift . outputStrLn $ doc
sep =  "    ....."

out :: forall t. Env t => String -> StateT (t Int) (InputT IO) ()
out x = do
    let g t = do
            putStrLn . drawTree' . fmap show $ t
    get >>= liftIO . mapM_ g . (to :: t Int -> [Tree Int]) 
    lift . outputStrLn $ x

loop :: forall t . Env t => ContT () (StateT (t Int) (InputT IO)) ()
loop = callCC $ \stop -> do
    let gl = do 
            mr <- lift . lift $ getInputLine "> "
            lift . lift $ outputStrLn ""
            return mr
        q p h =  gets (query p) >>= catchErrorM  out h
        m p = modify p >>= catchErrorM out return
        u   Nothing = stop ()
        u   (Just (map toUpper -> r)) = do
            case reads r of 
                [(L x y,_)] -> lift $ m (Link x y) 
                [(D x y,_)] -> lift $ m (Cut x y) 
                [(P x y,_)] -> lift $ q (Path x y) $ 
                    lift . outputStrLn . report . show
                [(I x,_)] -> lift $ q (Spanning x) $
                     liftIO . putStrLn . drawTree' . fmap show
                [(S,_)] -> lift $ out ""
                [(N,_)] -> lift (news >>= put) >> lift (out "")
                _ -> help
            gl >>= u
    help >> gl >>= u


run  :: forall t . Env t => Proxy (t Int) -> IO ()
run _ = (news :: IO (t Int)) >>= 
        runInputT defaultSettings . 
        evalStateT (runContT loop return)

        
main = run (Proxy :: Proxy (TourForest Int))


-- | Neat 2-dimensional drawing of a tree.
drawTree' :: Tree String -> String
drawTree'  = unlines . draw

draw :: Tree String -> [String]
draw (Node x ts0) = x : drawSubTrees ts0
  where
    drawSubTrees [] = []
    drawSubTrees [t] =
        shift "\x2514\x2500" "  " (draw t)
    drawSubTrees (t:ts) =
        shift "\x251c\x2500" "\x2502 " (draw t) ++ drawSubTrees ts

    shift first other = zipWith (++) (first : repeat other)
