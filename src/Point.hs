module Point where

data Point = Point {x :: Int, y :: Int} deriving (Eq, Show)

instance Ord Point where
    (Point x1 y1) <= (Point x2 y2) = (x1 <= x2) && (y1 <= y2)
    (Point x1 y1) >= (Point x2 y2) = (x1 >= x2) && (y1 >= y2)
    (Point x1 y1) < (Point x2 y2) = (x1 < x2) && (y1 < y2)
    (Point x1 y1) > (Point x2 y2) = (x1 > x2) && (y1 > y2)

initPoint = Point 1 1

xDelta :: Point -> Point -> Int
xDelta a b = (x a) - (x b)

yDelta :: Point -> Point -> Int
yDelta a b = (y a) - (y b)

shiftRight :: Point -> Point
shiftRight (Point x y) = Point (x+1) y

shiftDown :: Point -> Point
shiftDown (Point x y) = Point x (y+1)
