module Main where

import Dice
import Point
import DiceMap

import Control.Monad
import Control.Monad.ST
import Data.Maybe
import Data.STRef
import qualified Data.Map as Map

memoize :: STRef s DiceMap -> DiceKey -> DiceValue -> ST s ()
memoize cRef k v = modifySTRef cRef $ \m -> Map.insert k v m

getScore' :: STRef s DiceMap -> Die -> Point -> Point -> ST s (Die, Int)
getScore' cRef die current dest
    | current <= dest = do
        cache <- readSTRef cRef
        let currentKey = (die, current)
        let result = Map.lookup currentKey cache

        if isNothing result then do
            right <- getScore' cRef (rollRight die) (shiftRight current) dest
            down <- getScore' cRef (rollDown die) (shiftDown current) dest
            let (finalDie, childMax) = if (snd right) > (snd down) then right else down
            let maxScore = childMax + top die
            memoize cRef currentKey (finalDie, maxScore)
            return (finalDie, maxScore)
        else
            return $ fromJust result
    | otherwise = return (die, 0)

getScore :: Point -> ST s Int
getScore dest = do
    cRef <- newSTRef blankDiceMap
    snd <$> (getScore' cRef initDie initPoint dest)

main :: IO ()
main = readLn >>= \count -> do
    forM_ [1..count] $ \_ -> do
        [n,m] <- (map read . words) <$> getLine :: IO [Int]
        print $ runST $ getScore $ Point m n

