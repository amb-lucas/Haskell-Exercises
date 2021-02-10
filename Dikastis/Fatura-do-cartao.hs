splitListAt :: Int -> [String] -> [String]
splitListAt 0 v = v
splitListAt n (v:vs) = splitListAt (n-1) vs

splitStringOn :: Char -> String -> [String]
splitStringOn c str = splitStringOn' c str [] ""

splitStringOn' :: Char -> String -> [String] -> String -> [String]
splitStringOn' _ "" curAns "" = curAns
splitStringOn' _ "" curAns curBuilt = (curAns ++ [curBuilt])
splitStringOn' c (on:str) curAns curBuilt
  | c == on && curBuilt /= "" = splitStringOn' c str (curAns ++ [curBuilt]) ""
  | c == on                   = splitStringOn' c str curAns ""
  | otherwise                 = splitStringOn' c str curAns (curBuilt ++ (on:""))

logMes :: String -> String -> Double
logMes a b = logMes' a (splitStringOn ';' b)

logMes' :: String -> [String] -> Double
logMes' _ [] = 0
logMes' m dataList
  | m == month  = value + restSolved
  | otherwise   = restSolved
    where month = words (dataList !! 0) !! 1
          value = read (dataList !! 2)
          rest = splitListAt 3 dataList
          restSolved = logMes' m rest

main = do
    a <- getLine
    b <- getLine
    let result = logMes a b
    print result
