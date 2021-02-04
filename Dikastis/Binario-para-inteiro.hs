btoi :: String -> Int
btoi str = btoi' str 0 

btoi' :: String -> Int -> Int
btoi' "" n   = n
btoi' ('1':as) n = (btoi' as (2*n+1))
btoi' ('0':as) n = (btoi' as (2*n))

main = do {
  s <- getLine;
  print (btoi s)
}
