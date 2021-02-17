charmanderWin :: Int -> Bool
charmanderWin cp
  | cp < 1000 || cp > 9999              = False
  | (10*e + f)*(10*e + f) == cp         = True
  | otherwise                           = False
  where a = div cp 1000
        b = mod (div cp 100) 10
        c = mod (div cp 10) 10
        d = mod cp 10
        e = div ((a+c)*10+b+d) 10
        f = mod ((a+c)*10+b+d) 10

main = do {
  cp <- readLn;
  if (charmanderWin cp)
    then putStr "Charmander vitorioso"
    else putStr "Charmander derrotado"
}
