flx :: (a -> Bool) -> [a] -> [a]
fxAcc :: (a -> Bool) -> [a] -> [a] -> [a]
flx p xs = fxAcc p xs [] where
fxAcc _ [] acc = reverse acc
fxAcc p (x:xs) acc
      | p x = fxAcc p xs (x:acc)
      | otherwise = fxAcc p xs acc

randomFunc :: Integer -> Bool 
randomFunc a = a - 2 >= 0

main :: IO ()
main = do

      print(flx randomFunc [2,4,5,2,3,1])
      return()
