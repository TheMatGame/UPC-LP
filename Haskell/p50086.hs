data Queue a = Queue [a] [a]
     deriving (Show)

create :: Queue a
create = Queue [] []

push :: a -> Queue a -> Queue a
push a (Queue x y) = Queue x (a:y)

pop :: Queue a -> Queue a
pop (Queue [] []) = Queue [] []
pop (Queue [] y) = Queue (reverse (init y)) []
pop (Queue (x:xs) y) = Queue xs y

top :: Queue a -> a
top (Queue [] y) = last y
top (Queue (x:xs) _) = x

empty :: Queue a -> Bool
empty (Queue [] []) = True
empty _ = False

instance Eq a => Eq (Queue a)
    where (Queue a b) == (Queue x y) = (a ++ reverse b) == (x ++ reverse y)

instance Functor (Queue) where
    fmap f (Queue x y) = Queue (fmap f x) (fmap f y)

translation :: Num b => b -> Queue b -> Queue b
translation f (Queue x y) = fmap (+f) (Queue x y)

instance Applicative (Queue) where          -- NI IDEA
    (Queue [x] []) <*> q            = (Queue [] [x]) <*> q
    (Queue [] [x]) <*> q            = (Queue [x] []) <*> q
    pure x                          = (Queue [x] [])

merge :: Queue a -> Queue a -> Queue a
merge (Queue x y) (Queue a b) = Queue (x ++ reverse y) (a ++ reverse b)

instance Monad (Queue) where
    return x = Queue [x] []
    (Queue x y) >>= f = foldl merge create (map f (x ++ reverse y))

kfilter :: (p -> Bool) -> Queue p -> Queue p
kfilter f p = do
    v <- p
    if f v then return v else create