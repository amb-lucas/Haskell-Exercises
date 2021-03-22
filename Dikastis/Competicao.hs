abss :: Int -> Int
abss n
  | n >= 0    = n
  | otherwise = -n

maj :: Int -> Int -> Int
maj a b = div (a+b+(abss (a-b))) 2

main = do {
  a <- readLn;
  b <- readLn;
  c <- readLn;
  t <- readLn;
  
  print (t * (maj (maj a b) c))
}
