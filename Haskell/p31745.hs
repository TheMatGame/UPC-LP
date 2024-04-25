flatten :: [[Int]] -> [Int]
flatten x = foldl (++) [] x 

myLength :: String -> Int
myLength [] = 0
myLength x = last $ zipWith (f) x (iterate (+1) 1)
    where f a b = b

myReverse :: [Int] -> [Int]
myReverse x = foldr (f) [] x
    where f a b = b ++ [a]

countIn :: [[Int]] -> Int -> [Int]
countIn x y = map (f y) x
    where f a b = length $ filter (== a) b

firstWord :: String -> String
firstWord x = takeWhile (/= ' ') $ dropWhile (== ' ') x