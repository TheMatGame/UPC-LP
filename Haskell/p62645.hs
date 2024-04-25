main :: IO()
main = do
    entrada <- getContents
    putStrLn $ show $ tractar entrada

tractar :: String -> Int
tractar s = foldl (+) 0 x
    where 
        _x = words s
        x = map (read) _x