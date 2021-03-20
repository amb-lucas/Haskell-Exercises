data Tree t = Nilt |
              Node t (Tree t) (Tree t)
              deriving (Read)

isBST :: Ord t => Tree t -> Bool
isBST t
  | listed == []  = True
  | otherwise     = isBST'' lh lt
  where listed = isBST' t
        lh = head listed
        lt = tail listed
  
isBST' :: Ord t => Tree t -> [t]
isBST' Nilt = []
isBST' (Node a (t1) (t2)) = (isBST' t1) ++ a:(isBST' t2)

isBST'' :: Ord t => t -> [t] -> Bool
isBST'' a []  = True
isBST'' a b   = (a < (head b)) && (isBST'' (head b) (tail b)) 

main = do
       s <- getLine
       let result = isBST (read s::Tree Int)
       print result
