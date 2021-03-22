binomial :: Int -> Int -> Int
binomial n 0 = 1
binomial 0 k = 0
binomial n k = (binomial (n-1) k) + (binomial (n-1) (k-1))

parseInput str = let [n, k] = map read (words str)
                 in (n, k)

main :: IO()
main = interact $ show . uncurry binomial . parseInput
