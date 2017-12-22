module App.Snap where

import Prelude

import App.Background (Background(..))
import App.Geometry (Point(..), Stroke(..), distance, getNearestPoint)
import App.Graph (Graph(..), edges)
import Data.Function.Uncurried (runFn2)
import Data.List (List(..), concatMap, nub, (:))
import Data.Maybe (Maybe(..))
import Math (floor)

snapToPoint :: Point -> Background -> List Point -> Maybe Point
snapToPoint p bg snapPoints =
  case nearestPoint of
       Just snapPoint -> if (runFn2 distance p snapPoint < 20.0) then Just snapPoint
                                                          else Nothing
       _ -> Nothing
  where nearestPoint = getNearestPoint p $ snapPoints <> getGridPoints p bg

snapPoints :: Graph -> List Point
snapPoints g = nub $ concatMap strokeSnapPoints (edges g)

strokeSnapPoints :: Stroke -> List Point
strokeSnapPoints (Line p1 p2) = p1 : p2 : Nil
strokeSnapPoints (Arc c p q _) = c : p : q : Nil

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
