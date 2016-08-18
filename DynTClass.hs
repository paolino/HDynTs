{-# language TypeFamilies , MultiParamTypeClasses #-}
module DynTClass where

type Edge a = (a,a)

class DynT d a where
    type Coll d :: * -> *
    link :: Edge a -> Coll d (d a) -> Coll d (d a)
    cut :: Edge a -> Coll d (d a) -> Coll d (d a)
    connected :: Edge a -> Coll d (d a) -> Bool
    create :: [[Edge a]] -> Coll d (d a)

