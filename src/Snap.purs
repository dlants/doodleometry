module App.Snap where

import Prelude
import App.Background (Background(..))
import App.Geometry (Point(..), Stroke(..), distance, getNearestPoint)
import Data.List (List(..), concatMap, nub, (:))
import Data.Maybe (Maybe(..))
import Math (floor)

snapToPoint :: Point -> Background -> List Stroke -> Maybe Point
snapToPoint p bg strokes =
  case nearestPoint of
       Just snapPoint -> if (distance p snapPoint < 20.0) then Just snapPoint
                                                          else Nothing
       _ -> Nothing
  where nearestPoint = getNearestPoint p $ nub $ ( concatMap (getSnapPoints p) strokes
                                                 <> getGridPoints p bg)

getSnapPoints :: Point -> Stroke -> List Point
getSnapPoints _ (Line p1 p2) = p1 : p2 : Nil
getSnapPoints _ (Arc c p q _) = c : p : q : Nil

getGridPoints :: Point -> Background -> List Point
getGridPoints _ Clear = Nil
getGridPoints (Point x y) Square =
  let gridSize = 20.0
      minx = (floor (x / gridSize)) * gridSize
      miny = (floor (y / gridSize)) * gridSize
   in   (Point minx miny)
      : (Point minx (miny + gridSize))
      : (Point (minx + gridSize) miny)
      : (Point (minx + gridSize) (miny + gridSize))
      : Nil
