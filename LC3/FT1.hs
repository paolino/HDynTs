{-# language MultiParamTypeClasses, TemplateHaskell, ScopedTypeVariables, 
    ViewPatterns, FlexibleInstances,DeriveFunctor, StandaloneDeriving #-}
import qualified Data.FingerTree as F
import  Data.FingerTree (FingerTree, split, measure, Measured, viewl,viewr,  
    (<|) , (|>), ViewL ((:<)), ViewR ((:>)))
import qualified Data.Set as S
import Data.IntMap (IntMap)
import Data.Set (Set)
import Data.List
import Control.Arrow ((&&&), (***), second)
import Data.Maybe (isNothing, fromJust )
import Control.Lens hiding ((:<),(<|),(|>),(:>), children)
import Control.Lens.TH
import Data.Ord
import Data.Tree hiding (Forest)
import Data.Monoid
import Data.Maybe
import Data.Foldable

--------------------------------------------------------------------------------
----------------Library --------------------------------------------------------
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------

-- change inplace first element matching the change
change :: (a -> Maybe a) -> [a] -> Maybe [a]
change t [] = Nothing
change t (x:xs) = case t x of
    Nothing -> (x :) <$>  change t xs
    Just x' -> Just $ x' : xs

--------------------------------------------------------------------------------
---------------------Sets as a measure for fingertrees--------------------------
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------

newtype Collect a = Collect {unCollect :: a} deriving Show

instance Ord a => Measured (Set a) (Collect a) where
    measure (Collect x) = S.singleton x

type FT a = FingerTree (Set a) (Collect a)

--------------------------------------------------------------------------------
---------------------IsoPaths---------------------------------------------------
---paths coupled with their reversed vrsion-------------------------------------
--------------------------------------------------------------------------------

data IsoPath a = IsoPath {
    _orig :: FT a,
    _rev :: FT a 
    } deriving Show
makeLenses ''IsoPath

instance Ord a => Monoid (IsoPath a) where
    IsoPath o r `mappend` IsoPath o' r' = 
        IsoPath (o `mappend` o') (r' `mappend` r)
    mempty = IsoPath mempty mempty

------results of splitting an IsoPath ------------------------------------------
data Split a = NotFound | Take  a | Splitted a a
    deriving (Show, Functor)

splitIsoPath :: Ord a => a -> IsoPath a -> Split (IsoPath a)
splitIsoPath x p@(IsoPath o r) = let 
    (ao,bo) = split (S.member x) o
    (ar, viewl -> w :< br) = split (S.member x) r
    in  if null bo then NotFound
        else if null ao then Take p
        else Splitted (IsoPath ao br) (IsoPath bo $ ar |> w )

swapIsoPath :: IsoPath a -> IsoPath a
swapIsoPath (IsoPath o r) = IsoPath r o

tailIsoPath :: Ord a => IsoPath a -> Collect a
tailIsoPath (IsoPath (viewl -> w :< _) _) = w
-----------------------------------------------------------------------

data Path a = Path {
    _father :: Maybe a,
    _path :: IsoPath a
    } deriving Show

makeLenses ''Path

type Forest b a = [b a]

-- | convert a st of trees to a forest of paths
toForest    :: Ord a 
            => Forest Tree a -- ^ tree to deconstruct
            -> Forest Path a -- ^ deconstruction
toForest = concatMap (paths' Nothing) where
    paths' m (Node x []) = [Path m (IsoPath ft ft)] where 
                     ft = F.singleton $ Collect x
    paths' m (Node x xs) = let 
        Path _ (IsoPath o r) : ps = 
            sortBy (comparing (length . view (path . orig))) $ 
                zip (repeat $ Just x) xs >>= uncurry paths' 
        in 
        Path m (IsoPath (o |> Collect x) (Collect x <| r)) : ps

-- path head consumer, fail on empty path
consume (Path _ (IsoPath (viewr -> o :> Collect h ) (viewl -> _ :< r))) = 
    Just (h,Path (Just h) (IsoPath o r))
consume _ = Nothing

-- collect a subforest of trees , children of a node
children    ::  Ord a 
            => Maybe a -- dependency
            -> Forest Path a  -- forest to consume
            -> (Forest Path a, Forest Tree (Maybe a))
children mf =   second catMaybes . 
                uncurry (mapAccumL maybeNode) .
                partition ((/=) mf . view father) 

-- try to reconstruct a node of a tree
maybeNode   :: Ord a 
            => Forest Path a 
            -> Path a 
            -> (Forest Path a, Maybe (Tree (Maybe a)))

maybeNode f c = case consume c of
    Nothing -> (f,Nothing)
    Just (h,c') -> let
        (f',mc) = maybeNode f c'
        in second (Just . Node (Just h)) $ case mc of 
            Nothing -> (f',[])
            Just n -> second (n:) $ children (Just h) f'
        
-- | convert a forest of paths back to forest of trees format
fromForest  :: Ord a 
            => Forest Path a    -- ^ paths to reconstruct
            ->  Forest Tree a   -- ^ reconstruction
fromForest = map (fromJust <$>) . snd . children Nothing 

        
splitPath :: Ord a => a -> Path a -> Split (Path a)
splitPath x p@(Path mf dp) = case splitIsoPath x dp of
        Splitted p1 p2 -> 
            Splitted (Path (Just x) p1) (Path mf p2)
        Take p -> Take (Path mf p)
        NotFound -> NotFound

joinAndReversePaths :: Ord a => Forest Path a -> Path a
joinAndReversePaths xs = over path (swapIsoPath) $ 
    foldr1 (\(Path _ dp) (Path mf dp') ->  Path mf $ dp <> dp') xs

---------------------------------------------------------------------

{-

data Result a = Result {
    residual :: Path a,
    taken :: Path a
    }

unzipResults :: [Result a] -> ([Path a], [Path a])
unzipResults = unzip . map (\(Result r t)  -> (r,t))


-- insert a path, trying to merge something depending on its tail
mergeCorrection :: Ord a => Path a -> Forest a -> Maybe (Forest a)
mergeCorrection p = change $ \p' -> 
    case view father p' == Just (unCollect $ tailIsoPath $ view path p) of
        True -> Just $ over path (mappend $ view path p') p
        _ -> Nothing

takePath :: Ord a => a -> Forest a -> (Either (Path a) (Result a), Forest a)
takePath x f = let
    failure NotFound = True
    failure _ = False
    ([(_,t)], unzip -> (f',_)) = partition (not . failure . snd) $ 
        zip <*> map (splitPath x) $ f
    in case t of
        Take p -> (Left p, f') -- error "minimum number of path invariant broken"
        Splitted r p -> (Right $ Result r p, f')

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
-- O (n^2)
fromForest :: Ord a => Forest a -> [Tree a]
fromForest f = 
    let     ([], Node Nothing rs) = fromForest' Nothing f
            fromForest' mf f = let
                (cs,f') = collectChildren mf f
                (f'',ns) = mapAccumL (\f c -> fromForest' (Just c) f) f' cs
                in (f'', Node mf ns)
            collectChildren ma f = let
                (unzip . catMaybes . map consume -> (cs,ps),f') = 
                    partition ((==) ma . view father) f
                in (cs,ps ++ f')
    in map (fromJust <$>) rs

   -} 
