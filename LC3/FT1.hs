{-# language MultiParamTypeClasses, TemplateHaskell, ViewPatterns, FlexibleInstances #-}
import qualified Data.FingerTree as F
import  Data.FingerTree (FingerTree, split, measure, Measured, viewl, (<|) , (|>), ViewL ((:<)))
import qualified Data.Set as S
import Data.IntMap (IntMap)
import Data.Set (Set)
import Data.List
import Control.Arrow ((&&&), (***), second)
import Data.Maybe (isNothing, fromJust )
import Control.Lens hiding ((:<),(<|),(|>))
import Control.Lens.TH
import Data.Ord
import Data.Tree hiding (Forest)
import Data.Monoid
import Data.Foldable

newtype Collect a = Collect a deriving Show

instance Ord a => Measured (Set a) (Collect a) where
    measure (Collect x) = S.singleton x

type FT a = FingerTree (Set a) (Collect a)

data DoublePath a = DoublePath {
    _orig :: FT a,
    _rev :: FT a 
    } deriving Show

instance Monoid (DoublePath a) where
    DoublePath o r <> DoublePath o' r' = DoublePath (o <> o') (r' <> r)

data Split b a = NotFound | Take (b a) | Splitted (b a) (b a) deriving Show

data Cut = LC | RC 

splitDoublePath :: Ord a => Cut -> a -> DoublePath a -> Split DoublePath a
splitDoublePath  LC x p@(DoublePath o r) = let 
    (ao,bo) = split (S.member x) o
    (ar, viewl -> w :< br) = split (S.member x) r
    in  if null bo then NotFound
        else if null ao then Take p
        else Splitted (DoublePath ao br) (DoublePath bo $ ar |> w )
 
swapDoublePath (DoublePath o r) = DoublePath r o
-----------------------------------------------------------------------


data Path a = Path {
    _father :: Maybe a,
    _path :: DoublePath a
    } deriving Show

makeLenses ''Path

paths :: Ord a => Tree a -> [Path a]
paths = paths' Nothing where
    paths' m (Node x []) = [Path m ft ft] where ft = F.singleton $ Collect x
    paths' m (Node x xs) = let Path _ o r : ps = sortBy (comparing (length . view orig)) $ zip (undefined : repeat (Just x)) xs >>= uncurry paths' in 
        Path m (Collect x <| o) (r |>  Collect x) : ps


splitPath :: Ord a => Cut -> a -> Path a -> Split Path a
splitPath c  x p@(Path mf dp) = let 
    res = splitDoublePath c x dp
    case res of
        Splitted p1 p2 -> Splitted (Path (Just x) p1) (Path mf p2)
        Take p -> Take (Path mf p)
        NotFound -> NotFound

joinAndReversePaths :: [Path a] -> Path a
joinAndReversePaths xs = Path mf $ swapDoublePath $ foldr1 (\(Path _ dp) (Path mf dp') ->  Path mf $ dp <> dp') xs

tailDoublePath (DoublePath o (r :> w)) = w
---------------------------------------------------------------------


type Forest  a = [Path a]

data Result a = Result {
    residual :: Path a,
    taken :: Path a
    }

unzipResults :: [Result a] -> ([Path a], [Path a])
unzipResults = unzip . map (\(Result r t)  -> (r,t))

mergeCorrection :: Path a -> Forest a -> (Path a,Forest a)
mergeCorrection = undefined

takePath :: Cut -> a -> Forest a -> (Either (Path a) (Result a), Forest a)
takePath c x f = let
    failure NotFound = True
    failure _ = False
    ([t], f') = partition (not . failure . splitPath c x) f
    in case t of
        Take p -> (Left p, f') -- error "minimum number of path invariant broken"
        Splitted r p = (Right $ Result r p, f')

exposeIterate  :: Maybe a -> ([Result a],Forest a) -> ([Result a] , Forest a)
exposeIterate Nothing y = y
exposeIterate (Just x) (rs,f) = let
    case takePath LC x f of
        (Right (r@(Result _ (Path mf _))),f') -> exposeIterate mf (r:rs,f')       
        _ -> error "invariant broken" 

expose :: a -> Forest a -> (Path a,Forest a)
expose x f = let
    (unzipResults -> (rs,ts),f') = exposeIterate (Just x) ([],f)
    root = joinAndReversePaths ts -- :: Path a
    in second (rs ++) $ mergeCorrection root f' 
    
link :: a -> a -> Forest a -> Forest a
link x y f = let 
    (Path Nothing p, f') = expose y f
    in case takePath LC x f' of
        (Right _,_) -> Path (Just x) p : f'
        (Left (Path mf p'),f'') -> Path mf (p <> p') : f''

cut :: a -> Forest a -> Maybe (Forest a)
cut x f = case takePath RC x f of
        (Left _, _) -> Nothing
        (Right (Result (Path _ dp) p),f') -> let 
            (p',f'') = mergeCorrection p f'
            in Path Nothing dp : p' : f''
    
