{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant bracket" #-}

improve3::Double->Double->Double
improve3 0 toSquareRoot = 1.0
improve3 theGuess toSquareRoot = (2*theGuess + toSquareRoot/(theGuess*theGuess))/3


good::Double->Double->Double->Bool 
good theGuess toSquareRoot eps
    | eps < 0 = False 
    | abs(theGuess^3 - toSquareRoot) <= eps = True
    | otherwise = False

newtonIter::Double->Double->Double->Double
newtonIter theGuess toSquareRoot eps
    | (good theGuess toSquareRoot eps) = theGuess
    | otherwise = (newtonIter (improve3 (theGuess+1.0) toSquareRoot) toSquareRoot eps)


sqrt3Newton::Double->Double->Double
sqrt3Newton toSquareRoot eps =
   let improve3 theGuess toSquareRoot 
             | theGuess == 0 = 1.0
             | otherwise = (2*theGuess + toSquareRoot/(theGuess*theGuess))/3
       good theGuess toSquareRoot eps 
             | eps < 0 = False
             | abs(theGuess^3 - toSquareRoot) <= eps = True
             | otherwise = False
       newtonIter theGuess toSquareRoot eps
             | good theGuess toSquareRoot eps  = theGuess
             | otherwise = newtonIter ( improve3 theGuess toSquareRoot) toSquareRoot eps
    in newtonIter 1 toSquareRoot eps



main :: IO ()
main = do

    print(sqrt3Newton 27 0.00000000000000001)

    return()

