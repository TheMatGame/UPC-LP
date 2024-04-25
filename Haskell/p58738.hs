data STree a = Nil | Node Int a (STree a) (STree a) deriving Show

isOk :: STree a -> Bool
isOk Nil = True
isOk (Node t _ l r) = isOk l && isOk r && (t == (talla l + talla r + 1))

talla :: STree a -> Int
talla Nil = 0
talla (Node t _ _ _) = t

nthElement :: STree a -> Int -> Maybe a
nthElement (Node t x l r) n 
    | n < 1 || n > t = Nothing
    | n <= t' = nthElement l n
    | n == t'+1 = Just x
    | otherwise = nthElement r (n-t'-1)
    where 
        t' = talla l

mapToElements :: (a -> b) -> STree a -> [Int] -> [Maybe b]
mapToElements f Nil _ = [Nothing]
mapToElements f t [x] = [fmap f (nthElement t x)]
mapToElements f t (x:xs) = [fmap f (nthElement t x)] ++ mapToElements f t xs

instance Functor STree where
    fmap f Nil = Nil
    fmap f (Node t x l r) = Node t (f x) (fmap f l) (fmap f r)