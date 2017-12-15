module Main where

import Dice
import Point
import DiceMap

import Control.Monad
import Data.IORef
import qualified Data.Map as Map

-- TODO memoize results of getScore in DiceMap
memoize :: IORef DiceMap -> DiceKey -> DiceValue -> IO ()
memoize cRef k v = modifyIORef cRef $ \m -> Map.insert k v m

getScore :: Point -> Int
getScore dest = snd $ getScore' initPoint initDie where
    getScore' :: Point -> Die -> (Die,Int)
    getScore' point die =
        if point <= dest then
            let right = getScore' (shiftRight point) (rollRight die) 
                down = getScore' (shiftDown point) (rollDown die) 
                (finalDie, maxScore) = if (snd right) > (snd down) then right else down
            in (finalDie, top die + maxScore)
        else (die, 0)

main :: IO ()
main = readLn >>= \count -> do
    cache <- newIORef blankDiceMap
    forM_ [1..count] $ \_ -> do
        [n,m] <- (map read . words) <$> getLine :: IO [Int]
        print $ getScore $ Point m n
