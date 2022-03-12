val :: String -> [(String,Float)] -> Float
val toFind [] = 0.0
val toFind ((key,value):xs)
    | toFind == key = value
    | toFind /= key = val toFind xs
    
    
main :: IO ()
main = do


    print (val "x" [("x",1.2),("y",32.16),("z",-12.3)])

    return()
