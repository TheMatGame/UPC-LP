import Data.List (sort)

type Pos = (Int, Int)       -- la casella inferior esquerra és (1,1)

dins :: Pos -> Bool
dins (x,y) 
    | x > 8 || y > 8 || x < 1 || y < 1 = False
    | otherwise = True

moviments :: Pos -> [Pos]
moviments (x,y) = filter dins [ (x+i,y+j) | i <- [-2, -1, 1, 2], j <- [-2, -1, 1, 2], abs i /= abs j]

potAnar3 :: Pos -> Pos -> Bool
potAnar3 x y = elem y (concatMap moviments $ concatMap moviments $ moviments x)

potAnar3' :: Pos -> Pos -> Bool
potAnar3' x y = elem y (concatMap moviments $ concatMap moviments $ moviments x)