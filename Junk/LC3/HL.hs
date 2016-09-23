{-# language TypeOperators, MultiParamTypeClasses, FlexibleInstances#-}
module HL where

data a ::: b = a ::: b deriving Show

infixr 5 :::

data Z = Z

class Resolve a c where
    peek :: a -> c
    poke :: c -> a -> a

instance Resolve (a ::: b) a where
    peek (x ::: y) = x
    poke x (_ ::: y) = x ::: y

instance {-# OVERLAPS #-} Resolve b c => Resolve (a ::: b) c where
    peek (x ::: y) = peek y
    poke x (z ::: y) = z ::: poke x y

modifyR :: Resolve b a => (a -> a) -> b -> b
modifyR f x = poke (f . peek $ x) x
