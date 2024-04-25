import Data.List

degree :: Eq a => [(a, a)] -> a -> Int
degree [] x = 0
degree ((a,b):c) x 
    | a == x || b == x = 1 + degree c x
    | otherwise = degree c x

degree' :: Eq a => [(a, a)] -> a -> Int
degree' l x = length $ filter (edges x) l
    where 
        edges x (a,b) = x == a || x == b
-- degree' l x = foldl (+) 0 (map (contains x) l)

-- contains :: Eq a => a -> (a, a) -> Int
-- contains x (a,b) 
--     | a == x || b == x = 1
--     | otherwise = 0

neighbors :: Ord a => [(a, a)] -> a -> [a]
neighbors l x = sort $ map (nei x) (filter (edges x) l)
    where 
        edges x (a,b) = x == a || x == b

-- neighbors :: Ord a => [(a, a)] -> a -> [a]
-- neighbors edges vertex = sort [y | (a, b) <- edges, a == vertex || b == vertex, let y = if a == vertex then b else a]


nei :: Eq a => a -> (a,a) -> a
nei x (a,b) 
    | a == x = b
    | b == x = a
    | otherwise = x