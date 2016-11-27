module App.Geometry where

import Prelude
import Math (pow)
import Data.List (List(..), foldl)
import Data.Maybe (Maybe(..))

type Point =
  { x :: Number
  , y :: Number
  }

distance :: Point -> Point -> Number
distance p1 p2 = pow (p1.x - p2.x) 2.0 + pow (p1.y - p2.y) 2.0

getNearestPoint :: Point -> List Point -> Maybe Point
getNearestPoint _ Nil = Nothing
getNearestPoint p ps = foldl updateMax Nothing ps
  where
    updateMax :: Maybe Point -> Point -> Maybe Point
    updateMax Nothing p1 = Just p1
    updateMax (Just p1) p2 = if (distance p p1) < (distance p p2) then Just p1 else Just p2

getSnapPoint :: Point -> List Point -> Maybe Point
getSnapPoint p ps =
  case getNearestPoint p ps of
    Just np | distance p np < 10.0 -> Just np
    _ -> Nothing
