mdc :: Int -> Int -> Int
mdc 0 x = x
mdc x 0 = x
mdc a b = mdc (b) (a `rem` b)

main = do
   a <- readLn
   b <- readLn
   print (mdc a b)
