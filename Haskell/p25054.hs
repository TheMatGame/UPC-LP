myLength :: [Int] -> Int
myLength [] = 0
myLength (x:xs) = 1 + myLength xs

myMaximum :: [Int] -> Int
myMaximum [x] = x
myMaximum (x:xs) 
    | x > m = x
    | otherwise = m
    where m = myMaximum xs

-- arreglar, devuelve la entrada -> no tengo que dividir entre dos, voy perdiendo valor
average :: [Int] -> Int
average [x] = x
average (x:xs) = div (x + average xs) 2

buildPalindrome :: [Int] -> [Int]
buildPalindrome x = (reverse x) ++ x


member :: [Int] -> Int -> Bool
member [] x = False
member (y:ys) x
    | y == x = True
    | otherwise = member ys x

remove :: [Int] -> [Int] -> [Int]
remove [] [] = []
remove [] y = []
remove x [] = x
remove (x:xs) y = if member y x then remove xs y else x:(remove xs y) 

flatten :: [[Int]] -> [Int]
flatten [] = []
flatten (x:xs) = x ++ flatten xs
-- puedo usar el concat pero creo que no es la idea del ejercicio

oddsNevens :: [Int] -> ([Int],[Int])
oddsNevens [] = ([],[])
oddsNevens (x:xs)
    | (mod x 2) == 0 = (x:(fst y), snd y)
    | otherwise = (fst y, x:(snd y))
    where y = oddsNevens xs


-- prime :: [Int]
-- prime 0 = False
-- prime 1 = False
-- prime 2 = True
-- prime n
    -- | (length [ x | x <- [2 .. n-1], mod n x == 0]) > 0 = False
    -- | otherwise = True

-- primeDivisors :: Int -> [Int]
-- primeDivisors 0 = []
-- primeDivisors 1 = []
-- primeDivisors 2 = [2]
-- primeDivisors n
    -- | [ x | x <- [2 .. n-1], mod n x == 0]