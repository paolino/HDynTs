{-# language MultiParamTypeClasses, TemplateHaskell #-}
import qualified Data.FingerTree as F
import  Data.FingerTree (FingerTree, split, measure, Measured)
import qualified Data.Set as S
import qualified Data.IntMap as M
import Data.IntMap (IntMap)
import Data.Set (Set)
import Data.List
import Control.Arrow ((&&&), (***), second)
import Data.Maybe (isNothing, fromJust )
import Control.Lens
import Control.Lens.TH

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
    _rev :: FT a, 
    _versus :: Bool
    }

composePathsA :: Path a -> Path a -> Maybe (Paths a ,Paths a)
composePathsA  (Path mf1@(Just x) o1 r1 True) (Path mf2 o2 r2 True) = let 
    (ao,bo) = split (S.member x) o2
    (ar,br) = split (S.member x) r2
    in case af of 
        [] -> No:w
        thing
        _ -> Just (Path mf2 (o1 <> af) (bf' <> r1) True, Path mf1 bf (b :< af'))
composePathsA (Path mf1@(Just x) o1 r1 True) (Path mf2 o2 r2 False) = let 
    (bf ,b <: af) = split (S.member x) o2
    (bf',af') = split (S.member x) r2
    in case af of 
        [] -> Nothing
        _ -> Just (Path mf2 (o1 <> af') (bf :> b <> r1) True, Path mf1 af (b :< af'))
    


splitB True f t = split f t
splitB False f t = let 
    (bf :> w ,af) = split f t
    in (bf, w :< af)


makeLenses ''Path

type Forest a = IntMap (Path a)

select :: Forest a -> Index -> (Path a, Path a -> Forest a)
select fs i = (fs M.! i, \x -> M.Insert i x fs)

splitB True f t = split f t
splitB False f t = let 
    (bf :> w ,af) = split f t
    in (bf, w :< af)


-- search for presence
cut     :: Ord a 
        => a 
        -> Bool
        -> Forest a 
        -> (Forest a, (FT a, FT a))
cut x fs = let 
    (i, (p, (bf,af))) = fromJust . find (not . null . snd . snd . snd) . map (second (id &&& split (S.member x) . view path))) . M.assocs
    (bf' :> t,af') = split (S.member x) $ view reversePath p
    in (M.insert i (Path (Just x) bf (t :< af') $ view versus p) fs, (af,bf')) 


    
{-
search :: Ord a => Collect a -> Forest a -> (Index, (Path a, (FT a,Ft a)))
search x fs = let 
    z@(i,(p,cuts)) = fromJust $ find (\(_,(_ , (_,r))) -> not . null $ r) $ zip [0..] $ searchC x fs
    in 



cutStep :: Ord a => Collect a -> Forest a -> (Forest a, Path a)
cutStep x fs = let
    Just (n,(Path father _, (stay,leave))) = find (not . null . snd . snd . snd) $ zip [0..] $ cutter x fs
    in (Path (Just x) stay : deleteAt n fs, Path father leave)

driver a@(_, p@(Path Nothing _)) = a
driver (fs,Path (Just x) _) = cutStep x fs 

oneMore :: (a -> Bool) -> [a] -> [a]
oneMore c (x:xs) | c x = [x]
                 | otherwise = x:oneMore c xs

expose :: Ord a => Collect a -> Forest a -> Forest a
expose x = (\(fs,x) -> x : fs) . (last *** Path Nothing . F.reverse . mconcat . map (view path)) . unzip . oneMore (isNothing .  view father . snd) . iterate driver . cutStep x 
-}



