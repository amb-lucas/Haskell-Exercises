-- CASAMENTO DE PADRÕES
-- Outra forma de declarar funções com caso a caso

-- Declarar funções como casos específicos
fat :: Int -> Int
fat 0 = 1
fat n = n * fat(n-1)
-- Quando se declara uma incógnita como parâmetro, ele assume que é genérico

-- Também se pode ignorar alguns dos parâmetros, declarando-os como incógnitas
mdc :: Int -> Int -> Int
mdc n 0 = n
mdc 0 n = n
mdc a b = mdc b (mod a b)

-- maiorLista: Retorna o maior elemento da lista
maiorLista :: [Int] -> Int
maiorLista [] = minBound :: Int
maiorLista (x:xs) = max x (maiorLista xs)


-- EXERCÍCIOS

-- dobrarLista: Dobrar elementos de uma lista
dobrarLista :: [Int] -> [Int]
dobrarLista [] = []
dobrarLista (x:xs) = (2*x):(dobrarLista xs)

-- duplicarLista: Replicar os elementos de uma lista
duplicarLista :: [Int] -> [Int]
duplicarLista [] = []
duplicarLista (x:xs) = x:x:duplicarLista(xs)

-- membership: Checar se o elemento está numa lista
member :: [Int] -> Int -> Bool
member [] a = False
member (x:xs) a
  | x == a    = True
  | otherwise = member xs a

-- digits: Filtrar apenas os números de uma string
isDigit :: Char -> Bool
isDigit d
  | '0' <= d && d <= '9' = True
  | otherwise            = False

digits :: String -> String
digits "" = ""
digits (x:xs)
  | isDigit x = x:(digits xs)
  | otherwise = digits xs

-- sumPairs: Somar os elementos de duas listas
sumPairs :: [Int] -> [Int] -> [Int]
sumPairs [] x = x
sumPairs x [] = x
sumPairs (a:as) (b:bs) = (a+b):sumPairs as bs

