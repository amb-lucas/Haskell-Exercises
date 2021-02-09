solve :: Int -> Int
solve x
  | x <= 20 = 20 + x*x*x
  | x <= 40 = 8000 + (x-10)*(x-10)
  | x <= 60 = 9000 + 5*x
  | x <= 80 = 9300 + 2*x
  | otherwise = 9500 + x

main = do {
  p <- readLn;
  
  putStr ("Potencia de : " ++ (show (solve p)) ++ " W")
}
