{-# language FlexibleInstances #-}

module QC where
--  import FT1
{-
testConversionsIsomorphism :: Ord a => Forest Tree a -> Bool
testConversionsIsomorphism t = (pathsToTrees . treesToPaths) t ==  t

someTrees n = (flip evalState [1..] . mapM relabel) <$>
    (sample' $ arbitraryTree n)

testLinkCutIsomorphism :: Tree Int -> Tree a -> Gen Bool
testLinkCutIsomorphism xt yt' = let
        xs = toList xt
        yt = evalState (relabel yt') [last xs + 1 .. ]
        ys = toList yt
        forest = treesToPaths $ [xt,yt]
        in do
            x <- elements xs
            y <- elements ys
            let Just forest' = link x y forest >>= cut x
            return $ TreeEq forest == TreeEq forest'
-}
{-
Node    {rootLabel = 1, subForest = [
            Node {rootLabel = 2, subForest = [
                    Node {rootLabel = 3, subForest = [
                            Node {rootLabel = 4, subForest = []}
                            ]}
                    ]},
                    Node {rootLabel = 5, subForest = [
                        Node {rootLabel = 6, subForest = [
                            Node {rootLabel = 7, subForest = []}
                            ]},
                        Node {rootLabel = 8, subForest = [
                            Node {rootLabel = 9, subForest = []}
                        ]}
                            ]}
              ]}
                            -}
