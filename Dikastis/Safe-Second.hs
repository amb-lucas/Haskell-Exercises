import Prelude hiding (Maybe (..))

data Maybe a = Just a |
               Nothing
               deriving(Show)

safeSecond :: [a] -> Maybe a
safeSecond a
  | (length a) >= 2 = Just (a !! 1)
  | otherwise       = Nothing

main = do
       a <- getLine
       let result = safeSecond (read a::[Int])
       print result
