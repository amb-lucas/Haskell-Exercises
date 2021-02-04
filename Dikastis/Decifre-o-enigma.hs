decEnigma :: String -> [(Char, Char)] -> String
decEnigma "" trad     = ""
decEnigma (a:as) trad = (decEnigma' a trad) : (decEnigma as trad)

decEnigma' :: Char -> [(Char, Char)] -> Char
decEnigma' c trad = head [b | (a, b) <- trad, a == c]

main = do
    a <- getLine
    b <- getLine
    let result = decEnigma a (read b)
    print result
