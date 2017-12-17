module Main where

import Dice
import Point
import DiceMap

import Control.Monad
import Data.Maybe
import Data.IORef
import qualified Data.Map as Map

type MapRef = IORef DiceMap

memoize :: MapRef -> DiceKey -> DiceValue -> IO ()
memoize cRef k v = modifyIORef cRef $ \m -> Map.insert k v m

getScore' :: MapRef -> Die -> Point -> Point -> IO DiceValue
getScore' cRef die current dest
    | current <= dest = do
        cache <- readIORef cRef
        let currentKey = (die, current, dest)
        let result = Map.lookup currentKey cache

        if isNothing result then do
            right <- getScore' cRef (rollRight die) (shiftRight current) dest
            down <- getScore' cRef (rollDown die) (shiftDown current) dest
            let (finalDie, childMax) = if (snd right) > (snd down) then right else down
            let value = (finalDie, childMax + top die)
            memoize cRef currentKey value
            return value
        else
            return $ fromJust result
    | otherwise = return (die, 0)

getScore :: MapRef -> Point -> IO Int
getScore cRef dest = snd <$> (getScore' cRef initDie initPoint dest)

main :: IO ()
main = readLn >>= \count -> do
    cRef <- newIORef blankDiceMap
    forM_ [1..count] $ \_ -> do
        [n,m] <- (map read . words) <$> getLine :: IO [Int]
        result <- getScore cRef $ Point m n
        print result

