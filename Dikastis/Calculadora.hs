type Comando = String
type Valor = Int

solve :: [(Comando, Valor)] -> Int
solve ops = solve' ops 0

solve' :: [(Comando, Valor)] -> Int -> Int
solve' [] cur                       = cur
solve' (("Soma", val):as) cur       = solve' as (cur + val)
solve' (("Subtrai", val):as) cur    = solve' as (cur - val)
solve' (("Multiplica", val):as) cur = solve' as (cur * val)
solve' (("Divide", 0):as) cur       = solve' as (-666)
solve' (("Divide", val):as) cur     = solve' as (cur `div` val)

main = do {
  ops <- readLn;
  print $ solve ops
}
