module DiceMap where

import Dice
import Point

import Data.List (permutations)
import Data.Map (Map)
import qualified Data.Map as Map

type DiceKey = (Point,Die)
type DiceMap = Map DiceKey Int

blankMap :: DiceMap
blankMap = Map.fromList states where
    points = [Point x y | x <- cycle [1..60], y <- concat $ replicate 60 <$> [1..60]]
    dice = cycle [Die a b c d e f | [a,b,c,d,e,f] <- permutations [1..6]]
    possibilities = zip points dice
    states = zip possibilities $ repeat (-1)
