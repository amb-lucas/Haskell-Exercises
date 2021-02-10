somaSqrt :: [Double] -> Double
somaSqrt nums = somaSqrt' [num | num <- nums, num > 0]

somaSqrt' :: [Double] -> Double
somaSqrt' [] = 0
somaSqrt' (a:as) = (sqrt a) + somaSqrt' as

main = interact $ show . somaSqrt . read
