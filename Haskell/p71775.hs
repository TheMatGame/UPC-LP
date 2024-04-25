countIf :: (Int -> Bool) -> [Int] -> Int
countIf f x = length (filter f x)

pam :: [Int] -> [Int -> Int] -> [[Int]]
pam [] _ = []
pam _ [] = []
pam x (y:ys) = map y x : pam x ys

-- pam2 :: [Int] -> [Int -> Int] -> [[Int]]
-- pam2 [] _ = []
-- pam2 _ [] = []
-- pam2 (x:xs) y = map y ( map (const x) (x:xs)) : pam2 xs y

filterFoldl :: (Int -> Bool) -> (Int -> Int -> Int) -> Int -> [Int] -> Int
filterFoldl fx fy x y = foldl fy x (filter fx y)

insert :: (Int -> Int -> Bool) -> [Int] -> Int -> [Int]
insert f x y = takeWhile (f y) x ++ y : dropWhile (f y) x