import Control.Monad.Cont
import Control.Monad.Trans

-- f :: (MonadIO m) => (Integer -> m Integer) -> m Integer

f g g' x = foldM (\s _ -> if s == 10 then g s else if s == 20 then g' s  else return (s + 1)) x $ repeat ()
main = do
    y <- readLn
    flip runContT print $ do 
        x <- callCC $ \g' -> do
            x <- callCC $ \g ->  f g g' y
            liftIO $ print "ciao"
            return x
        liftIO $ print "hello"
        return x
        
        

