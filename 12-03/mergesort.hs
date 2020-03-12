
list :: [Int]
list = [6,4,1,2,5,6,2,234,3,523,432,5432,5]

merge :: [Int] -> [Int] -> [Int]
merge [] arr = arr
merge arr [] = arr
merge (a:as) (b:bs)
  | a <= b    = a:(merge (as) (b:bs))
  | otherwise = b:(merge (a:as) (bs))


halfLen :: [Int] -> Int
halfLen arr = (length arr) `div` 2

firstHalf :: [Int] -> [Int]
firstHalf [] = []
firstHalf arr = fst (splitAt (halfLen arr) arr)

secondHalf :: [Int] -> [Int]
secondHalf [] = []
secondHalf arr = snd (splitAt (halfLen arr) arr)

mergeSort :: [Int] -> [Int]
mergeSort [] = []
mergeSort [a] = [a]
mergeSort x = merge (mergeSort (firstHalf x)) (mergeSort (secondHalf x))
