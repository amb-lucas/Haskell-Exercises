
quickSort [] = []
quickSort (a:as) = (quickSort [b|b <- as, b <= a]) ++ a:(quickSort [b|b <- as, b > a])

