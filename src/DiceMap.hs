module DiceMap where

import Dice
import Point
import Data.List (permutations)

type DiceKey = (Point,Die)
type DiceMap = [(DiceKey,Int)]

blankMap :: DiceMap
blankMap = states where
    points = [Point x y | x <- cycle [1..60], y <- concat $ replicate 60 <$> [1..60]]
    dice = cycle [Die a b c d e f | [a,b,c,d,e,f] <- permutations [1..6]]
    possibilities = zip points dice
    states = zip possibilities $ repeat (-1)

