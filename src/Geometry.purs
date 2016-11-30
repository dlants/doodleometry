module App.Geometry where

import Prelude
import Data.List (List(..), foldl)
import Data.Maybe (Maybe(..))
import Math (pow)

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
