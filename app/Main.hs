module Main where

import Dice
import Point
import DiceMap
import Control.Monad

initDie = Die 3 4 1 6 2 5
initPoint = Point 1 1

-- TODO memoize results in DiceMap
getScore :: Point -> Int
getScore dest = getScore' initPoint initDie where
    getScore' point die =
        if point <= dest then
            let right = getScore' (shiftRight point) (rollRight die) 
                down = getScore' (shiftDown point) (rollDown die) 
            in top die + max right down 
        else 0

main :: IO ()
main = readLn >>= \count ->
    forM_ [1..count] $ \_ -> do
        [n,m] <- (map read . words) <$> getLine :: IO [Int]
        print $ getScore $ Point m n
