module App.Geometry where

import Prelude
import Data.List (List(..), foldl, head, last)
import Data.Maybe (Maybe(..))
import Math (pow, Radians, atan2)

data Point = Point Number Number

instance ptEq :: Eq Point where
  eq (Point x1 y1) (Point x2 y2) = x1 == x2 && y1 == y2

instance ptShow :: Show Point where
  show (Point x y) = "(" <> show x <> ", " <> show y <> ")"

-- | arbitrarily compare xs first then ys
instance ptOrd :: Ord Point where
  compare (Point x1 y1) (Point x2 y2) | x1 == x2 = compare y1 y2
  compare (Point x1 _ ) (Point x2 _) = compare x1 x2

distance :: Point -> Point -> Number
distance (Point x1 y1) (Point x2 y2) = pow (x1 - x2) 2.0 + pow (y1 - y2) 2.0

getNearestPoint :: Point -> List Point -> Maybe Point
getNearestPoint _ Nil = Nothing
getNearestPoint p ps = foldl updateMax Nothing ps
  where
    updateMax :: Maybe Point -> Point -> Maybe Point
    updateMax Nothing p1 = Just p1
    updateMax (Just p1) p2 = if (distance p p1) < (distance p p2) then Just p1 else Just p2

data Stroke = Line Point Point

firstPoint :: Stroke -> Point
firstPoint (Line p1 _) = p1

secondPoint :: Stroke -> Point
secondPoint (Line _ p2) = p2

angle :: Stroke -> Radians
angle (Line (Point x1 y1) (Point x2 y2)) =
  atan2 (x2 - x1) (y2 - y1)

reverse :: Stroke -> Stroke
reverse (Line p1 p2) = Line p2 p1

isCycle :: List Stroke -> Boolean
isCycle strokes = compareFirstLast (head strokes) (last strokes)
  where
    compareFirstLast (Just (Line p1 _)) (Just (Line _ p4)) = p1 == p4
    compareFirstLast _ _ = false

instance strokeEq :: Eq Stroke where
  eq (Line p1 p2) (Line q1 q2) =
    (p1 == q1 && p2 == q2)
    || (p1 == q2 && p2 == q1)

-- | important that there's a deterministic order since this is used as a key in
-- | a strmap
instance strokeShow :: Show Stroke where
  show (Line p1 p2) =
    "Line(" <> show p1 <> " -- " <> show p2 <> ")"

instance strokeClockwiseOrd :: Ord Stroke where
  compare s1 s2 =
    if p1 /= p2
       then compare p1 p2
       else compare (angle s1) (angle s2)
       -- TODO - if a line and arc have the same angle, sort based on arc curvature
    where
      p1 = firstPoint s1
      p2 = firstPoint s2
