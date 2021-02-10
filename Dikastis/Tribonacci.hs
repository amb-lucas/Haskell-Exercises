tribonacci :: Integer -> Integer
tribonacci 1 = 1
tribonacci 2 = 1
tribonacci 3 = 2
-- Simple option without Dynamic Programming
-- tribonacci n = (tribonacci (n-1)) + (tribonacci (n-2)) + (tribonacci (n-3))
tribonacci n = tribonacci' 1 1 2 3 n

tribonacci' a b c i j
  | i == j = c
  | otherwise = tribonacci' b c (a+b+c) (i+1) j

main :: IO()
main = interact $ show . tribonacci . read
