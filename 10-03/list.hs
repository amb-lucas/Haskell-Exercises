-- LISTAS

-- Usando listas
totalVendas = [1, 2, 3, 3, 3, 4, 4, 5, 5, 6]

-- head: primeiro elemento
-- tail: resto da lista
selectTotalVendas lista s
  | lista == []       = 0
  | (head lista) == s = 1 + (selectTotalVendas (tail lista) s)
  | otherwise         = selectTotalVendas (tail lista) s

-- Operador ' : '
-- Construtor de listas
-- Primeiro parâmetro: Elemento do mesmo tipo da lista
-- Segundo parâmetro: Lista

cabeca = 5
listaComCabeca = cabeca : []
-- [5]

novaCabeca = 6
listaComNovaCabeca = novaCabeca : listaComCabeca
-- [6]

operadorMultiplasVezes = 1:2:3:4:5:[]
-- [1,2,3,4,5]


-- Declarando listas com range ' .. '

-- [2..7] = [2,3,4,5,6,7]
-- ['a'..'d'] = ['a','b','c','d']
-- [2.8..5.0] = [2.8,3.8,4.8]
-- [7,5..0] = [7,5,3,1]
-- [10..1] = []   // Primeiro elemento já passa do limite superior 


-- Concatenação da listas: ' ++ '
-- [1, 2, 3] ++ [4, 5, 6] = [1,2,3,4,5,6]

mdc n 0 = n
mdc a b = mdc b (a `mod` b)
