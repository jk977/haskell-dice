module DiceMap where

import Dice
import Point

import Data.Map (Map)
import qualified Data.Map as Map

type DiceKey = (Die,Point)
type DiceValue = (Die,Int)
type DiceMap = Map DiceKey DiceValue

initKey :: DiceKey
initKey = (initDie,initPoint)

blankDiceMap :: DiceMap
blankDiceMap = Map.fromList states where
    x = cycle [1..60]
    y = concatMap (replicate 60) [1..60]
    points = uncurry Point <$> (zip x y)
    allPoints = concatMap (replicate $ length possibleDice) points
    possibilities = zip (cycle possibleDice) allPoints
    states = zip possibilities $ repeat (initDie, -1)

