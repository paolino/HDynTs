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
import Data.Tree
import Data.Monoid
import Data.Foldable

deleteAt :: Int -> [a] -> [a]
deleteAt n xs = let (bs,_:rs) = splitAt n xs in bs ++ rs



newtype Collect a = Collect a deriving Show

instance Ord a => Measured (Set a) (Collect a) where
    measure (Collect x) = S.singleton x

type FT a = FingerTree (Set a) (Collect a)

type Index = Int

data Path a = Path {
    _father :: Maybe a,
    _orig :: FT a,
    _rev :: FT a 
    } deriving Show
makeLenses ''Path


paths :: Ord a => Tree a -> [Path a]
paths = paths' Nothing where
    paths' m (Node x []) = [Path m ft ft] where ft = F.singleton $ Collect x
    paths' m (Node x xs) = let Path _ o r : ps = sortBy (comparing (length . view orig)) $ zip (undefined : repeat (Just x)) xs >>= uncurry paths' in 
        Path m (Collect x <| o) (r |>  Collect x) : ps

data Split a = NotFound | Take (Path a) | Splitted (Path a) (Path a) deriving Show

failure NotFound = True
failure _ = False

splitPath :: Ord a => a -> Path a -> Split a
splitPath  x p@(Path mf o r) = let 
    (ao,bo) = split (S.member x) o
    (ar, viewl -> w :< br) = split (S.member x) r
    in  if null bo then NotFound
        else if null ao then Take p
        else Splitted (Path (Just x) ao br) (Path mf bo $ ar |> w )

joinAndReversePaths xs = Path mf r o where
    Path mf o r = foldr1 (\(Path _ o r) (Path mf o' r') ->  Path mf (o <> o') (r' <> r)) xs

newtype Elem a = Elem {fromElem  :: a}
instance Measured  (Sum Int) (Elem a) where
    measure _ = Sum 1

type Paths a = FingerTree (Sum Int) (Elem (Path a))

exposeStep :: Paths a -> a -> (Paths a, Path a)
exposeStep ps x = let
    ([Elem t], rs) = partition (not . failure . splitPath x . fromElem) $ toList ps
    in case t of
        Take p -> (ps,p)
        Splitted r p -> (ps |> Elem r ,p)

    
    


