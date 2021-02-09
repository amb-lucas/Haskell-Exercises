ehZero :: Int -> Bool
ehZero 0 = True
ehZero x = False

main :: IO()
main = interact $ show . ehZero . read
