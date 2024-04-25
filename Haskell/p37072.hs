data Tree a = Node a (Tree a) (Tree a) | Empty deriving (Show)

size :: Tree a -> Int
size Empty = 0
size (Node _ y z) = 1 + size y + size z

height :: Tree a -> Int
height Empty = 0
height (Node _ y z) = 1 + max (height y) (height z)

equal :: Eq a => Tree a -> Tree a -> Bool
equal Empty Empty = True
equal _ Empty = False
equal Empty _ = False
equal (Node x1 y1 z1) (Node x2 y2 z2)
    | x1 == x2 = True
    | otherwise = False 

isomorphic :: Eq a => Tree a -> Tree a -> Bool
isomorphic Empty Empty = True
isomorphic _ Empty = False
isomorphic Empty _ = False
isomorphic (Node x1 y1 z1) (Node x2 y2 z2) =
    x1 == x2 && ((isomorphic y1 y2 && isomorphic z1 z2) || (isomorphic y1 z2 && isomorphic z1 y2))

preOrder :: Tree a -> [a]
preOrder Empty = []
preOrder (Node x1 y1 z1) = x1 : (preOrder y1 ++ preOrder z1)

postOrder :: Tree a -> [a]
postOrder Empty = []
postOrder (Node x l r) = postOrder l ++ postOrder r ++ [x]

inOrder :: Tree a -> [a]
inOrder Empty = []
inOrder (Node x l r) = inOrder l ++ [x] ++ inOrder r

-- breadthFirst :: Tree a -> [a]
-- breadthFirst Empty = []
-- breadthFirst (Node x l r) = x : 

-- build :: Eq a => [a] -> [a] -> Tree a

overlap :: (a -> a -> a) -> Tree a -> Tree a -> Tree a
overlap _ t1 Empty = t1
overlap _ Empty t2 = t2
overlap f (Node x l1 r1) (Node y l2 r2) = Node (f x y) (overlap f l1 l2) (overlap f r1 r2)