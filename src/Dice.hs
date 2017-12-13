module Dice where

data Die = Die {
    left    :: Int,
    right   :: Int,
    top     :: Int, -- side facing up
    bottom  :: Int,
    front   :: Int,
    back    :: Int
} deriving (Eq, Ord)

rollRight :: Die -> Die
rollRight die = Die
    (bottom die)    -- new left
    (top die)       -- new right
    (left die)      -- new top
    (right die)     -- new bottom
    (front die)     -- new front
    (back die)      -- new back

rollDown :: Die -> Die
rollDown die = Die
    (left die)      -- new left
    (right die)     -- new right
    (back die)      -- new top
    (front die)     -- new bottom
    (top die)       -- new front
    (bottom die)    -- new back

