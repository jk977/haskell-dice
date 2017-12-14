module DiceMap where

import Dice
import Point

import Data.Map (Map)
import qualified Data.Map as Map

type DiceKey = (Point,Die)
type DiceMap = Map DiceKey Int

blankDiceMap :: DiceMap
blankDiceMap = Map.fromList states where
    points = [Point x y | (x,y) <- zip (cycle [1..60]) $ concatMap (replicate 60) [1..60]]
    allPoints = concatMap (replicate $ length possibleDice) points
    possibilities = zip allPoints $ cycle possibleDice
    states = zip possibilities $ repeat (-1)
