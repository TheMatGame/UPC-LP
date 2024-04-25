main :: IO()
main = do
    nom <- getLine
    putStrLn $ comproba nom

comproba :: String -> String
comproba s 
    | head s == 'A' || head s == 'a' = "Hello!"
    | otherwise = "Bye!"