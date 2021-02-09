avatarToys = ["pirocoptero",
               "tartaruga de barro",
                "macaco de madeira",
                "tambor de mao de madeira"]

solve :: String -> [String] -> [String] -> String
solve name [] _ = (name ++ " eh o avatar.")
solve name _ []  = (name ++ " nao eh o avatar.")
solve name remaining (filt:as) = (solve name [a | a <- remaining, a /= filt] as)

main = do {
  avatar <- getLine;
  toy1 <- getLine;
  toy2 <- getLine;
  toy3 <- getLine;
  toy4 <- getLine;
  
  putStr (solve avatar avatarToys [toy1, toy2, toy3, toy4])
}
