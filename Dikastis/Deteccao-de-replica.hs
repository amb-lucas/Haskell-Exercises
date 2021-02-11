isReplica :: String -> Int -> Char -> Bool
isReplica "" 0 _      = True
isReplica "" _ _      = False
isReplica _ 0 _       = False
isReplica (a:as) n c  = (a == c) && (isReplica as (n-1) c)

main = do
    a <- getLine
    b <- getLine
    c <- getChar
    let result = isReplica a (read b) c
    print result
