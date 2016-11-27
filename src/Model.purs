module App.Model where

import Prelude
import App.Geometry (Graph, Point(..), distance, emptyGraph, getNearestPoint, pushStroke)
import Data.List (List(..), (:))
import Data.Maybe (Maybe(..))

data Action
  = Click Point
  | Move Point

type State =
  { graph :: Graph
  , click :: Maybe Point
  , hover :: Point
  }

init :: State
init =
  { graph: emptyGraph
  , click: Nothing
  , hover: Point {x: 0.0, y: 0.0}
  }

updateForClick :: Point -> State -> State
updateForClick p s@{click: Nothing} = s {click = Just p}
updateForClick p2 s@{click: Just p1}
  = s { click = Nothing
      , graph = pushStroke p1 p2 s.graph
      }

snapToPoint :: Point -> State -> Point
snapToPoint p s =
  case maybeSnapPoint of
       Just snapPoint -> if (distance p snapPoint < 20.0) then snapPoint else p
       _ -> p
  where maybeSnapPoint = getNearestPoint p s.graph.vertices

update :: Action -> State -> State
update (Click p) s = updateForClick (snapToPoint p s) s
update (Move p) s = s {hover = p}
