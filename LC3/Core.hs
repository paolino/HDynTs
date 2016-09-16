

{-# language FlexibleInstances,MultiParamTypeClasses, 
    ExistentialQuantification, ScopedTypeVariables, 
    GeneralizedNewtypeDeriving , TypeFamilies, TemplateHaskell,
    ViewPatterns , FlexibleContexts, StandaloneDeriving,ConstraintKinds #-}

import qualified Data.Map as M
import Control.Arrow 
import Data.List hiding (insert)
import Data.Foldable
import Data.Maybe
import Control.Lens hiding (Index)
import Control.Lens.TH
import Control.Monad.State
import HL


mkRMap :: (Enum (IndexF a), Ord (IndexF a)) => IndexF a -> Db a 
mkRMap z = make M.empty where
    query m k = m M.! k
    update m k v = make $ M.insert k v m
    new m v = let
        k = maybe z (fst . fst) $ M.maxViewWithKey m
        in (make $ M.insert (succ k) v m, succ k)
    make x = Db (query x) (update x) (new x)

data Db a = Db {
    query :: IndexF a -> a,
    update :: IndexF a -> a -> Db a,
    new :: a -> (Db a , IndexF a)
    }

data family IndexF a 

---------------------------- Monadic Single interface -------------

newS :: (Resolve b (Db a), Monad m) => a -> StateT b m (IndexF a)
newS x = do
    (db,k) <- flip new x <$> peek <$> get
    modify $ poke db
    return k

updateS :: (Resolve b (Db a), Monad m) => IndexF a -> a -> StateT b m ()
updateS k x = modify . modifyR $ \db -> update db k x 

queryS :: (Resolve b (Db a), Monad m) =>  IndexF a -> StateT b m a
queryS k = gets $ flip query k . peek
--------------------------------------------------------------------------
data Direction = Original | Inverted deriving Show

invert Original = Inverted
invert Inverted = Original

newtype Depth = Depth {fromDepth :: Int} deriving Show


---- elem database -----


type IE a = IndexF (ITV a)
data instance IndexF (ITV a) = IndexE a


--------------------------------
---- inverse linking -- 
----------------------- 

type ITV a = IndexF (TV a)

data TV a = TV (Maybe (ITV a)) (T a)

data instance IndexF (TV a) = IndexT Int deriving (Ord,Eq)

instance Enum (ITV a) where
    toEnum n = IndexT $ toEnum n
    fromEnum (IndexT n) = fromEnum n

data T a = T [(Depth, ITV a)] | L [a]

type HasDb s b = Resolve s (Db b)

bottomUpCore :: (HasDb s (TV a) , Monad m) => ITV a -> StateT s m (T a)
bottomUpCore k = do
    TV mf t <- queryS k
    case mf of
        Nothing -> return t
        Just k' -> bottomUpCore k'


-- bottomUp :: (HasDb ITV a, HasDb TV a, Monad m) => a -> StateT 
bottomUp x = queryS x >>= bottomUpCore

{-

--------- forest level -----------

data P a = P (ITV a) Direction
-}


