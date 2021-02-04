type Name = String
type Weight = String
type Height = String
type BPM = String
type Character = (Name, Weight, Height, BPM)

knownCharacters :: [Character]
knownCharacters =
  [("Aang", "leve", "baixa", "alto"),
  ("Sokka", "medio", "media", "alto"),
  ("Katara", "leve", "media", "medio"),
  ("Suki", "leve", "alta", "medio"),
  ("Zuko", "medio", "alta", "alto"),
  ("Ty Lee", "medio", "media", "medio"),
  ("Homem Combustao", "pesado", "alta", "baixo"),
  ("Pedregulho", "pesado", "baixa", "medio"),
  ("Tio Iroh", "pesado", "baixa", "baixo")]

filterWeight :: [Character] -> String -> [Character]
filterWeight options weight = [(a, b, c, d) | (a, b, c, d) <- options, b == weight]

filterHeight :: [Character] -> String -> [Character]
filterHeight options height = [(a, b, c, d) | (a, b, c, d) <- options, c == height]

filterBPM :: [Character] -> String -> [Character]
filterBPM options bpm = [(a, b, c, d) | (a, b, c, d) <- options, d == bpm]

filterChar :: String -> String -> String -> [Character]
filterChar a b c = (filterBPM (filterHeight (filterWeight knownCharacters a) b) c)

solve :: String -> String -> String -> String
solve a b c
  | length (filterChar a b c) == 1  = solve' (head (filterChar a b c))
  | otherwise                       = "Quem eh voce, novato?"
  
solve' :: Character -> String
solve' (a, b, c, d) = "Ja sei, voce eh " ++ a ++ "!"

main = do {
  w <- getLine;
  h <- getLine;
  b <- getLine;
  
  putStr (solve w h b)
}
