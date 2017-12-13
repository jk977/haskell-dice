module Point where

data Point = Point Int Int deriving Eq

instance Ord Point where
    (Point x1 y1) <= (Point x2 y2) = (x1 <= x2) && (y1 <= y2)
    (Point x1 y1) >= (Point x2 y2) = (x1 >= x2) && (y1 >= y2)
    (Point x1 y1) < (Point x2 y2) = (x1 < x2) && (y1 < y2)
    (Point x1 y1) > (Point x2 y2) = (x1 > x2) && (y1 > y2)

shiftRight :: Point -> Point
shiftRight (Point x y) = Point (x+1) y

shiftDown :: Point -> Point
shiftDown (Point x y) = Point x (y+1)

