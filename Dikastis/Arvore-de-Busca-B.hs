data Tree t = Node t (Tree t) (Tree t) | Empty
  deriving  (Eq, Show, Read)

buildSearchTree :: [Int] -> (Tree Int)
buildSearchTree nums  = buildSearchTree' nums Empty

buildSearchTree' [] base      = base
buildSearchTree' (a:as) base  = buildSearchTree' as (insertNum base a)

insertNum Empty a = (Node a (Empty) (Empty))
insertNum (Node n l r) a
  | a < n     = (Node n (insertNum l a) r)
  | otherwise = (Node n l (insertNum r a))

searchTreeSort :: [Int] -> [Int]
searchTreeSort nums = searchTreeSort' a
  where a = buildSearchTree nums

searchTreeSort' :: (Tree Int) -> [Int]
searchTreeSort' Empty = []
searchTreeSort' (Node n l r) = (searchTreeSort' l) ++ (n:(searchTreeSort' r))

main = interact $ show . searchTreeSort . (read :: String -> [Int])
