calc :: Int -> Int
calc n
  | n <= 10   = 7
  | n <= 30   = n - 3
  | n <= 100  = (2*n) - 33 
  | otherwise = (5*n) - 333

main = do
  n <- readLn;
  print $ calc n
