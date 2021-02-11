truncateFormat :: Double -> Int -> String
truncateFormat num 0 = show (truncate' num 0)
truncateFormat num n = truncateFormat' (show (truncate' num n)) n False

truncateFormat' :: String -> Int -> Bool -> String
truncateFormat' _ 0 True          = ""
truncateFormat' "" n True         = "0" ++ (truncateFormat' "" (n-1) True)
truncateFormat' (a:as) n True     = a:(truncateFormat' as (n-1) True)
truncateFormat' "" n False        = "." ++ (truncateFormat' "" n True)
truncateFormat' ('.':as) n False  = "." ++ (truncateFormat' as n True)
truncateFormat' (a:as) n False    = a:(truncateFormat' as n False)

truncate' :: Double -> Int -> Double
truncate' x n = truncate'' (x + (5/t)) n
  where t = 10^(n+1)

truncate'' :: Double -> Int -> Double
truncate'' x n = (fromIntegral (floor (x * t))) / t
  where t = 10^n
    
dist :: Double -> Double -> Double -> Double -> Double
dist x1 y1 x2 y2 = sqrt (dx*dx + dy*dy)
  where dx = x1 - x2
        dy = y1 - y2

solve :: Double -> Double -> String
solve x y = hogsmeadeLine ++ kakarikoLine ++ solitudeLine
  where hogsmeadeLine = "Distancia para Hogsmeade: " ++
                        (truncateFormat (dist x y 34 220) 2) ++ "\n"
        kakarikoLine = "Distancia para Kakariko: " ++
                        (truncateFormat (dist x y 0 0) 2) ++ "\n"
        solitudeLine = "Distancia para Solitude: " ++
                        (truncateFormat (dist x y 140 456) 2)

main = do {
  x <- readLn;
  y <- readLn;
  
  putStr $ solve x y
}
