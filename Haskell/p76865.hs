data Tree a = Empty | Node a (Tree a) (Tree a)
         deriving (Show)

instance Foldable Tree where
    foldr _ z Empty = z
    foldr f z (Node a l r) = f a (foldr f (foldr f z l) r) 

avg :: Tree Int -> Double
avg Empty = 0.0
avg x = fromIntegral (sum x `div` length x)

cat :: Tree String -> String
cat Empty = ""
cat (Node x l r) = x ++ " " ++ cat l ++ cat r