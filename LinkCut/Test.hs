dbS = mkRMap (IString 0) :: Db String

t1 = let 
    (db',k) = new dbS "paolino"
    in query db' k == "paolino"
t2 = let 
    (db1,k) = new dbS "paolino"
    db2 = update db1 "juan"
    in query db2 k == "juan"


newtype DbSingle m a b = DbSingle (StateT (Db a) m b) deriving 
    (Monad, MonadState)

testMonadic :: DbSingle m String Bool
testMonadic = do
    k <- new' "paolino"
    r <- query' k
    update' r "juan"
    return $ r == "juan"

query (new dbS "paolino") (IString 0) == "paolino"
query (new (new dbS "paolino") "juan") (IString 1) == "juan"
query (new (new dbS "paolino") "juan") (IString 1) == "juan"
