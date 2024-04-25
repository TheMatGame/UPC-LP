ones :: [Integer]
ones = iterate (*1) 1
-- ones = repeat 1

nats :: [Integer]
nats = iterate (+1) 0

nextNum :: Integer -> Integer
nextNum 0 = 1
nextNum a 
    | a > 0 = -a
    | a < 0 = (-a) + 1

ints :: [Integer]
ints = iterate nextNum 0


triangle :: Int -> [Integer]
triangle a = (foldl (+) 0 (take a nats)) : triangle (a+1)

triangulars :: [Integer]
triangulars = triangle 1


facts :: Int -> [Integer]
facts a = (foldl (*) 1 (tail (take a nats))) : facts (a+1)

factorials :: [Integer]
factorials = facts 1

fib :: Integer -> Integer -> [Integer]
fib x y = x : fib y (x+y)

fibs :: [Integer]
fibs = fib 0 1

-- primes :: [Integer]

-- hammings :: [Integer]

-- lookNsay :: [Integer]

-- tartaglia :: [[Integer]]