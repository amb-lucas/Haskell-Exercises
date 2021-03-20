data Command = Forward Int | Backward Int | TurnLeft | TurnRight 
               deriving (Eq, Show, Read)

turnRight :: (Int, Int) -> (Int, Int)
turnRight (1, 0) = (0, -1)
turnRight (0, -1) = (-1, 0)
turnRight (-1, 0) = (0, 1)
turnRight (0, 1) = (1, 0)

turnLeft :: (Int, Int) -> (Int, Int)
turnLeft (1, 0) = (0, 1)
turnLeft (0, 1) = (-1, 0)
turnLeft (-1, 0) = (0, -1)
turnLeft (0, -1) = (1, 0)

destination :: (Int,Int) -> [Command] -> (Int,Int) 
destination pos commands = destination' pos (0, 1) commands

destination' :: (Int, Int) -> (Int, Int) -> [Command] -> (Int, Int)
destination' a _ [] = a
destination' (x, y) (dx, dy) (curCommand:restCommands)
  | curCommand == TurnLeft  = destination' (x,y) (turnLeft (dx, dy)) restCommands
  | curCommand == TurnRight = destination' (x,y) (turnRight (dx, dy)) restCommands
  | otherwise               = destination' (processMove (x, y) (dx, dy) curCommand) (dx, dy) restCommands

processMove :: (Int, Int) -> (Int, Int) -> Command -> (Int, Int)
processMove (x, y) (dx, dy) (Forward n)   = (x + n*dx, y + n*dy)
processMove (x, y) (dx, dy) (Backward n)  = (x - n*dx, y - n*dy) 

main = do
       a <- getLine
       b <- getLine
       let result = destination (read a) (read b)
       print result
