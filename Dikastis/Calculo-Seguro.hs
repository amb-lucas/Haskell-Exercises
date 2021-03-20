import Prelude hiding (Maybe (..))

data Maybe a = Just a |
               Nothing
               deriving(Show)

type Operation = (Int, String, Int)

isNumber :: Char -> Bool
isNumber n = (n >= '0') && (n <= '9')

convertToNum :: Char -> Int
convertToNum '0' = 0
convertToNum '1' = 1
convertToNum '2' = 2
convertToNum '3' = 3
convertToNum '4' = 4
convertToNum '5' = 5
convertToNum '6' = 6
convertToNum '7' = 7
convertToNum '8' = 8
convertToNum '9' = 9

parseNum :: Int -> String -> Int
parseNum cur []     = cur
parseNum cur (a:as) = parseNum (10*cur + convertToNum(a)) as

parseString :: Int -> String -> Operation
parseString cur l
  | (isNumber a)  = (parseString (10*cur + convertToNum(a)) as)
  | otherwise     = (cur, op, b)
  where rest = (splitAt 3 l)
        (a:as) = l
        op = (fst rest)
        b = (parseNum 0 (snd rest))

safeCalc :: String -> IO()
safeCalc op
  | o == "div" && b == 0  = putStr "Nothing"
  | o == "sum"            = print (Just (a+b))
  | o == "sub"            = print (Just (a-b))
  | o == "mul"            = print (Just (a*b))
  | otherwise             = print (Just (div a b))
  where parsedOp = (parseString 0 op)
        (a, o, b) = parsedOp

main = do
       a <- getLine
       safeCalc a
