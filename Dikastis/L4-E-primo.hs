sq :: Int -> Int
sq 0 = 0
sq 1 = 1
sq n = sq' n 0 n

sq' :: Int -> Int -> Int -> Int
sq' num l r
  | r < l       = r
  | m*m == num  = m
  | m*m > num   = sq' num l (m-1)
  | otherwise   = sq' num (m+1) r
  where m = ((l+r) `div` 2)

isPrime :: Int -> Bool
isPrime 2 = True
isPrime n = isPrime' n nums
  where nums = 2:[3,5..(sq n)]
  
isPrime' :: Int -> [Int] -> Bool
isPrime' _ [] = True
isPrime' n (a: as)
  | n `mod` a == 0  = False
  | otherwise       = isPrime' n as

main = do {
  num <- readLn;
  
  if isPrime num
  then putStr "E primo"
  else putStr "Nao e primo"
}
