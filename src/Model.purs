module App.Model where

import Prelude
import App.Geometry (Point(..), Stroke(..), distance, getNearestPoint)
import App.Graph (Graph, addStroke, emptyGraph)
import Data.Map (keys)
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
  , hover: Point 0.0 0.0
  }

updateForClick :: Point -> State -> State
updateForClick p s@{click: Nothing} = s {click = Just p}
updateForClick p2 s@{click: Just p1}
  = s { click = Nothing
      , graph = addStroke stroke s.graph
      }
  where
    stroke = Line p1 p2

snapToPoint :: Point -> State -> Point
snapToPoint p s =
  case maybeSnapPoint of
       Just snapPoint -> if (distance p snapPoint < 20.0) then snapPoint else p
       _ -> p
  where maybeSnapPoint = getNearestPoint p (keys s.graph)

update :: Action -> State -> State
update (Click p) s = updateForClick (snapToPoint p s) s
update (Move p) s = s {hover = p}
