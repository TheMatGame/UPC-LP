insert :: [Int] -> Int -> [Int]
insert [] y = [y]
insert (x:xs) y
    | x > y = (y:(x:xs))
    | otherwise = x : insert xs y

isort :: [Int] -> [Int]
isort [] = []
isort (x:xs) = insert (isort xs) x

remove :: [Int] -> Int -> [Int]
remove [] y = []
remove (x:xs) y
    | x == y = xs
    | otherwise = x : remove xs y

ssort :: [Int] -> [Int]
ssort [] = []
ssort [x] = [x]
ssort (x:xs)
    | m > x = x : (ssort xs)
    | otherwise = m : (ssort (x : remove xs m))
    where m = minimum xs

merge :: [Int] -> [Int] -> [Int]
merge [] y = y
merge x [] = x
merge (x:xs) (y:ys)
    | x < y = x : (merge xs (y:ys))
    | otherwise = y : (merge (x:xs) ys)

msort :: [Int] -> [Int]
msort [] = []
msort [x] = [x]
msort x = merge ( msort ( take ( div ( length x) 2) x))  ( msort ( drop ( div ( length x) 2) x))


maxim :: [Int] -> Int -> [Int]
maxim [] y = []
maxim (x:xs) y
    | y <= x = x : maxim xs y
    | otherwise = maxim xs y 

minim :: [Int] -> Int -> [Int]
minim [] y = []
minim (x:xs) y
    | y > x = x : minim xs y
    | otherwise = minim xs y 

qsort :: [Int] -> [Int]
qsort [] = []
qsort [x] = [x]
qsort (x:xs) = ( qsort ( minim xs x)) ++ x : (qsort ( maxim xs x))


genMaxim :: Ord a => [a] -> a -> [a]
genMaxim [] y = []
genMaxim (x:xs) y
    | y <= x = x : genMaxim xs y
    | otherwise = genMaxim xs y 

genMinim :: Ord a => [a] -> a -> [a]
genMinim [] y = []
genMinim (x:xs) y
    | y > x = x : genMinim xs y
    | otherwise = genMinim xs y 

genQsort :: Ord a => [a] -> [a] 
genQsort [] = []
genQsort [x] = [x]
genQsort (x:xs) = ( genQsort ( genMinim xs x)) ++ x : (genQsort ( genMaxim xs x))