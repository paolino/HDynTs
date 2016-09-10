
{-
data FTree a = forall t h . (Eq t, Eq h) => FTree  (M t (T a, Int)) (M a (t,h)) (M t [h]) (M h R) deriving Show

type M k x = [(k,x)]

link :: Eq a => FTree a -> a -> a -> Excxeoifwo Maybe (FTree a)
link (FTree st sv stv sp) v1 v2 = let
    Just (kt1,h1) = lookup sv v1
    Just (kt2,h2) = lookup sv v2
    if kt1 == kt2 then Nothing else 
       Just undefined
  
  let
        Just (t1,n1) = lookup st kt1
        Just (t2,n2) = lookup st kt2
        Just p1 = lookup sp h1
        Just p2 = lookup sp h2
        (t2',cps) = reRoot p2 t2
        (t1',cts) = insertT t1 p1 t2'
        Just ps = lookup stv kt2 
        sp' = foldr (adjust (cts . cps) ) ps $ sp
        in Just bajsdbhkasf


link x y

-}
    
