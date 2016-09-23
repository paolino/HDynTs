
import Test.QuickCheck
test_splice n = do
    x <- arbitraryTree n
    y <- arbitraryTree n
    let [fromTree -> x',fromTree -> y'] = relabelForest [x,y]
    MP e <- elements $ toList (view orig x')
    let s@(ETR o r) = splice x' e y'
    return $ valid s
test_rereoot ::  Gen Bool
test_rereoot  = do
    xs :: String <- arbitrary `suchThat` (not . null)
    x <- elements xs  
    let ETR o r = reroot x $ mkETR xs
    return $ toList o == reverse (toList r)
