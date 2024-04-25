main :: IO()
main = do
    entrada <- getLine
    putStrLn $ resol entrada

resol :: String -> String
resol s = 
    let (a,b,c,d) = parse s



parse :: String -> (Int, String, String, String)
parse s = (a,b,c,d)
    where 
        [_a,b,c,d] = words s
        a = read _a