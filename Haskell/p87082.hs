main :: IO()
main = do
    line <- getLine
    if line /= "*" then do
        putStrLn $ interpereta line
        main
    else 
        return()

interpereta :: String -> String
interpereta line = name ++ ": " ++ calcula mass height
    where 
        (name, mass, height) = parse line

calcula :: Float -> Float -> String
calcula m h = resultat (m / (h*h))

resultat :: Float -> String
resultat x 
    | x < 18 = "underweight"
    | x < 25 = "normal weight"
    | x < 30 = "overweight"
    | x < 40 = "obese"
    | otherwise = "severely obese"


parse :: String -> (String, Float, Float)
parse line = (name, mass, height)
    where
        [name, _mass, _height] = words line
        mass = read _mass
        height = read _height
