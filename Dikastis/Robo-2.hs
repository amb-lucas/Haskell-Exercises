data Command = Forward Int | Backward Int | TurnLeft | TurnRight 
               deriving (Eq, Show, Read)

data Direction = North | South | West | East
                 deriving (Read, Show)

turnRight :: Direction -> Direction
turnRight North = East
turnRight East = South
turnRight South = West
turnRight West = North

turnLeft :: Direction -> Direction
turnLeft North = West
turnLeft West = South
turnLeft South = East
turnLeft East = North

destination :: Direction -> [Command] -> Direction 
destination dir [] = dir
destination dir (TurnRight:as) = destination (turnRight dir) as
destination dir (TurnLeft:as) = destination (turnLeft dir) as
destination dir (a:as) = destination dir as

main = do
       a <- getLine
       b <- getLine
       let result = destination (read a) (read b)
       print result
