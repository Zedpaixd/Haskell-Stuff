search :: String -> [(String,Float)] -> Float
search toFind [] = 0.0
search toFind ((key,value):xs)
    | toFind == key = value
    | toFind /= key = search toFind xs
    
    
main :: IO ()
main = do


    print (search "x" [("x",1.2),("y",32.16),("z",-12.3)])

    return()
