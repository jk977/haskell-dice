module DiceMap where

import Dice
import Point

import Data.Map (Map)
import qualified Data.Map as Map

type DiceKey = (Die,Point)
type DiceValue = (Die,Int)
type DiceMap = Map DiceKey DiceValue

blankDiceMap :: DiceMap
blankDiceMap = Map.fromList []

