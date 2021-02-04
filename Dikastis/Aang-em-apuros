victory :: Bool -> String
victory truth
  | truth == True = "Aang venceu o combate!"
  | otherwise     = "Aang perdeu o combate e agora esta preso na fortaleza da nacao do fogo."

evaluateSpeed :: String -> Int
evaluateSpeed "Rapido" = 1
evaluateSpeed x = -1

solve :: Int -> Int -> String -> String -> Double -> Double -> String 
solve a1 a2 b1 b2 c1 c2
  | a1 > a2   = victory True
  | a1 < a2   = victory False
  | otherwise = solve' b1 b2 c1 c2
  
solve' :: String -> String -> Double -> Double -> String
solve' b1 b2 c1 c2
  | b1 == "Rapido" && b2 == "Lento" = victory True
  | b1 == "Lento" && b2 == "Rapido" = victory False
  | otherwise                       = solve'' c1 c2
  
solve'' :: Double -> Double -> String
solve'' c1 c2
  | c1 >= c2  = victory True
  | otherwise = victory False

main = do {
  power1 <- readLn;
  power2 <- readLn;
  
  speed1 <- getLine;
  speed2 <- getLine;
  
  precis1 <- readLn;
  precis2 <- readLn;
  
  putStr (solve power1 power2 speed1 speed2 precis1 precis2)
}
