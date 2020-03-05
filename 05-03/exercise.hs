
-- Comentário de uma linha
{-
  Comentário
  de
  múltiplas
  linhas
-}


-- Função
vendas n = n*2 + 1

-- Função declarando os tipos de argumentos e retorno
selectVendas :: Int -> Int -> Int
selectVendas n s
  | n < 0           = 0
  | (vendas n) == s = 1 + (selectVendas (n - 1) s)
  | otherwise       = selectVendas (n - 1) s
  
  
-- Usando listas
totalVendas = [1, 2, 3, 3, 3, 4, 4, 5, 5, 6]

-- head: primeiro elemento
-- tail: resto da lista
selectTotalVendas lista s
  | lista == []       = 0
  | (head lista) == s = 1 + (selectTotalVendas (tail lista) s)
  | otherwise         = selectTotalVendas (tail lista) s
  
