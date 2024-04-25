data Queue a = Queue [a] [a]
     deriving (Show)

create :: Queue a
create = Queue [] []
-- create Queue [] [] = []
-- create Queue x [] = x
-- create Queue x y = x ++ reverse y

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