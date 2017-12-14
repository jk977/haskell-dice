module Dice where

data Die = Die {
    left    :: Int,
    right   :: Int,
    top     :: Int, -- side facing up
    bottom  :: Int,
    front   :: Int,
    back    :: Int
} deriving (Eq, Ord, Show)

initDie = Die 3 4 1 6 2 5

dieFromList :: [Int] -> Die
dieFromList [a,b,c,d,e,f] = Die a b c d e f

rotateClockwise :: Die -> Die
rotateClockwise die = Die
    (back die)      -- new left
    (front die)     -- new right
    (top die)       -- new top
    (bottom die)    -- new bottom
    (left die)      -- new front
    (right die)     -- new back

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

rollUp :: Die -> Die
rollUp die = Die
    (left die)      -- new left
    (right die)     -- new right
    (front die)     -- new top
    (back die)      -- new bottom
    (bottom die)    -- new front
    (top die)       -- new back

possibleDice :: [Die]
possibleDice = concatMap orientations faces where
    transform :: (Die -> Die) -> Die -> [Die]
    transform f start = transform' start 1 where
        transform' current n | n < 4 = [current] ++ (transform' (f current) $ n+1)
                             | otherwise = [current]

    row = transform rollRight
    orientations = transform rotateClockwise
    faces = row initDie ++ [rollDown initDie, rollUp initDie]
