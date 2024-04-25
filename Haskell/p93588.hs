myMap :: (a -> b) -> [a] -> [b] 
myMap f x = [ (f y) | y <- x]

myFilter :: (a -> Bool) -> [a] -> [a]
myFilter f x = [ y | y <- x, (f y)]

myZipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
myZipWith f x y = [ (f a b) | (a,b) <- zip x y ]

thingify :: [Int] -> [Int] -> [(Int, Int)]
thingify x y = [ (a,b) | a <- x, b <- y, a `mod` b == 0 ]

factors :: Int -> [Int]
factors x = [ y | y <- [1..x], x `mod` y == 0 ]