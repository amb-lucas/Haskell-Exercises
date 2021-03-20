data Tree t = Nilt |
              Node t (Tree t) (Tree t)
              deriving (Read, Show)

insertList :: Ord t => Tree t -> [t] -> Tree t
insertList tree []      = tree
insertList tree (a:as)  = insertList (insertValue tree a) as

insertValue :: Ord t => Tree t -> t -> Tree t
insertValue Nilt v = (Node v Nilt Nilt)
insertValue (Node u t1 t2) v 
  | u > v     = (Node u (insertValue t1 v) t2)
  | otherwise = (Node u t1 (insertValue t2 v))

main = do
       a <- getLine
       b <- getLine
       let result = insertList (read a::Tree Int) (read b)
       print result
