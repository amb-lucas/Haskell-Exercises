halve :: [String] -> ([String], [String])
halve [] = ([], [])
halve [a] = ([a], [])
halve (a:(b:bs)) = (a:(fst $ halve bs), b:(snd $ halve bs))

main = do {
  x <- getLine;
  print $ halve (words x)
}
