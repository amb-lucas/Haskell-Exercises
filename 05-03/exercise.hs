
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
  
  
  
-- EXERCÍCIOS

-- Fatorial: Calcula o fatorial do parâmetro
fatorial :: Int -> Int
fatorial n
  | n == 0    = 1
  | otherwise = n * fatorial(n-1)

-- all4Equal: Checa se todos são iguais
all4Equal :: Int -> Int -> Int -> Int -> Bool
all4Equal a b c d
  | a == b && b == c && c == d = True
  | otherwise                  = False

-- equalCount: Retorna quantos parâmetros são iguais
equalCount :: Int -> Int -> Int -> Int
equalCount a b c
  | a == b && b == c           = 3
  | a == b || b == c || a == c = 2
  | otherwise                  = 0

-- coprimes: Retorna se os números são primos entre si
mdc :: Int -> Int -> Int
mdc a b
  | b == 0    = a
  | otherwise = mdc b (a `mod` b)

coprimes :: Int -> Int -> Bool
coprimes a b
  | mdc a b == 1 = True
  | otherwise    = False
