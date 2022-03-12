data Prefix = Const Float | Var String | Sum Prefix Prefix | Prod Prefix Prefix
    deriving (Show, Eq)


val :: String -> [(String,Float)] -> Float
val toFind [] = 0.0
val toFind ((key,value):xs)
    | toFind == key = value
    | toFind /= key = val toFind xs


evaluate :: Prefix -> [(String,Float)] -> Float
evaluate (Const constantValue) _       = constantValue
evaluate (Var mappedValue) env         = val mappedValue env
evaluate (Sum leftSide rightSide) env  = evaluate leftSide env + evaluate rightSide env
evaluate (Prod leftSide rightSide) env = evaluate leftSide env * evaluate rightSide env



main :: IO ()
main = do

    print(evaluate (Sum (Prod (Var "x") (Const 3)) (Var "y")) [("x",7),("y",2)])

    return()
