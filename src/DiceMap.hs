module DiceMap where

import Dice
import Point

import Data.Map (Map)
import qualified Data.Map as Map

type DiceKey = (Die,Int,Int)    -- orientation, x delta, y delta
type DiceValue = (Die,Int)          -- orientation and score
type DiceMap = Map DiceKey DiceValue

blankDiceMap :: DiceMap
blankDiceMap = Map.fromList []

