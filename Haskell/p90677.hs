myFoldl :: (a -> b -> a) -> a -> [b] -> a
myFoldl _ x [] = x
myFoldl f x (y:ys) = myFoldl f (f x y) ys 

myFoldr :: (a -> b -> b) -> b -> [a] -> b
myFoldr _ x [] = x
myFoldr f x (y:ys) = f y (myFoldr f x ys)

myIterate :: (a -> a) -> a -> [a]
myIterate f x = x : myIterate f (f x)

myUntil :: (a -> Bool) -> (a -> a) -> a -> a
myUntil fx fy x 
    | fx x = x
    | otherwise = myUntil fx fy (fy x)

myMap :: (a -> b) -> [a] -> [b]
myMap _ [] = []
myMap f (x:xs) = (f x) : myMap f xs

myFilter :: (a -> Bool) -> [a] -> [a]
myFilter _ [] = []
myFilter f (x:xs) 
    | f x = x : myFilter f xs
    | otherwise = myFilter f xs

myAll :: (a -> Bool) -> [a] -> Bool
myAll _ [] = True
myAll f (x:xs) 
    | f x = myAll f xs
    | otherwise = False

myAny :: (a -> Bool) -> [a] -> Bool
myAny _ [] = False
myAny f (x:xs) 
    | f x = True
    | otherwise = myAny f xs

myZip :: [a] -> [b] -> [(a, b)]
myZip _ [] = []
myZip [] _ = []
myZip (x:xs) (y:ys) = (x,y) : myZip xs ys

myZipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
myZipWith _ _ [] = []
myZipWith _ [] _ = []
myZipWith f (x:xs) (y:ys) = (f x y) : myZipWith f xs ys