module App.Snap where

import Prelude
import App.Geometry (Point, Stroke(..), distance, getNearestPoint)
import Data.List (List(..), concatMap, nub, (:))
import Data.Maybe (Maybe(..))

snapToPoint :: Point -> List Stroke -> Maybe Point
snapToPoint p strokes =
  case nearestPoint of
       Just snapPoint -> if (distance p snapPoint < 20.0) then Just snapPoint
                                                          else Nothing
       _ -> Nothing
  where nearestPoint = getNearestPoint p $ nub $ concatMap (getSnapPoints p) strokes

getSnapPoints :: Point -> Stroke -> List Point
getSnapPoints _ (Line p1 p2) = p1 : p2 : Nil
getSnapPoints _ (Arc c p q _) = c : p : q : Nil
