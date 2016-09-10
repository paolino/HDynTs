
{-# language FlexibleInstances,MultiParamTypeClasses, 
    ExistentialQuantification, ScopedTypeVariables, 
    GeneralizedNewtypeDeriving , TypeFamilies, TemplateHaskell,
    ViewPatterns#-}

import qualified Data.Map as M
import Control.Arrow 
import Data.List hiding (insert)
import Data.Foldable
import Data.Maybe
import Control.Lens hiding (Index)
import Control.Lens.TH



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

newtype Index a = Index {index :: Int} deriving (Enum, Eq, Ord, Num, Show)

type family IndexF a 

type instance IndexF (Path a) = Index (Path a)
type instance IndexF (Range a) = Index (Range a)
type instance IndexF (Index (Range a)) = a

newtype Depth = Depth {fromDepth :: Int} deriving Show

data Direction = Original | Inverted deriving Show

invert Original = Inverted
invert Inverted = Original

data Path a = Path {
    _dependency :: Maybe (Index (Path a), Depth),
    _ranges :: [Index (Range a)],
    _direction :: Direction
    }
    deriving Show

data Range a = Range {
    _elements :: [a],
    _path :: Index (Path a)
    } 

-- makeLenses' Range

data Forest a =  Forest 
    (Db (Path a))
    (Db (Range a))
    (Db (Index (Range a)))

cut :: Eq a => a -> Forest a -> Forest a
cut x (Forest dbp dbr dbv) = let
    ir = query dbv x -- range holding
    r@(Range _ ip)  = query dbr ir -- path holding
    Path dep rs Original = query dbp ip --- BIG JOB HERE on Data.Sequence 
        -- to resolve Original
    (stay,_:go) = break (==ir) rs   
    (r',fr'') = cutCore x r
    r''@(Range els _) = fr'' ip'''''
    dbr' = update dbr ir r'
    (dbr'',ir'') = new dbr' r''
    dbv' = foldl' (uncurry . update) dbv $ zip els $ repeat ir''
    in Forest dbp dbr'' dbv'

cutCore :: Eq a =>  a -> Range a -> (Range a, Index (Path a) -> Range a)
cutCore x (Range els p) = let
    (els',cs) = break (== x) els
    in (Range els' p, Range cs)


{-


cut :: a -> Forest a -> Forest a
link :: a -> a -> Forest a -> Forest a
connected :: a -> a -> Bool




class LinkCut t a where
    cutLC :: a -> t a -> (t a, t a)
    mirrorLC :: t a -> t a
    linkLC :: t a -> t a -> t a
    createLC :: a -> t a

instance Eq a => LinkCut [] a where
    cutLC x = break (==x)
    mirrorLC = reverse
    linkLC = (++)
    createLC = return

expose :: (Foldable t, LinkCut t a) => Int -> Int -> Forest t a -> Forest t a
expose nx ny f@(Forest rc ri) = let
    expose' (C Nothing _) f = f
    expose' (C (Just d) xs) (Forest rc ri) = let
        iz  = query ri d  -- get the path above
        C dz zs = query rc iz 
        (go,stay) = cutLC d zs -- split that path
        rc' = update rc iz $ C (Just d) stay -- update that path as its tail
        newC = C dz $ linkLC go zs --new y column
        rc'' = update rc' ny newC  -- update the exposing column
        -- update the moved elements position (O(n))
        ri' = foldl' (uncurry . update) ri $ zip (toList go) $ repeat nx
        in expose' newC $ Forest rc'' ri'
    in expose' (query rc ny) f

rotate :: (Foldable t, LinkCut t a) => a -> Int -> Forest t a -> Forest t a 
rotate y ny (Forest rc ri) = let
    C Nothing ys = query rc ny
    (stay,go) = cutLC y ys
    (rc', kc) = new rc $ C Nothing $ mirrorLC go
    rc'' = update rc' ny $ C (Just y) stay
    -- update the moved elements position (O(n))
    ri' = foldl' (uncurry . update) ri $ zip (toList go) $ repeat kc
    in Forest rc'' ri'

link :: (Foldable t, LinkCut t a) => a -> a -> Forest t a -> Forest t a 
link x y f@(Forest rc ri) = let  
    es@(nx,ny) = (query ri x, query ri y)
    Forest rc' ri' = rotate y ny . expose nx ny $ f
    C Nothing ys = query rc' ny
    in Forest (update rc' ny (C (Just x) ys)) ri'

cut :: forall a t. (Foldable t, LinkCut t a) 
        => a -> Forest t a -> Forest t a
cut  x  (Forest rc ri) = let
    nx = query ri x
    C d xs = query rc nx
    (stay,go) = cutLC x xs
    in  if null stay then Forest (update rc nx (C Nothing xs)) ri 
        else let    rc' = update rc nx $ C d stay 
                    (rc'',kc) = new rc' $ C Nothing go
                    ri' = foldl' (uncurry . update) ri $ zip (toList go) $ 
                            repeat kc
             in Forest rc'' ri'
insert :: forall a t. (Foldable t, LinkCut t a) 
        => a -> Forest t a -> Forest t a
insert x (Forest rc ri) = let
    (rc',kc) = new rc (C Nothing $ createLC x)
    ri' = update ri x kc
    in Forest rc' ri'

top x f@(Forest rc ri) = let
    nx = query ri x
    in case query rc nx  of
        C Nothing _ -> nx
        C (Just d) _ -> top d f

-}
