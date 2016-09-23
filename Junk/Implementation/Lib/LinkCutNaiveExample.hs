{-# language ViewPatterns, DeriveFunctor #-}

import Tree
import TreeExample
import LinkCutNaive


tl12 = link (mkATree t0) "z" (mkATree t1) "v"
(s1,s2) = cut tl12 "z"

