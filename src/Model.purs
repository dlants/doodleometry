module App.Model where

import Prelude
import App.ColorScheme (ColorScheme)
import App.Geometry (Point(..), Stroke(..), distance, getNearestPoint)
import App.Graph (Cycle(..), Graph, addStroke, emptyGraph, findCycles, updateCycles)
import Data.List (List(..))
import Data.Map (Map, empty, keys)
import Data.Map (update) as Map
import Data.Maybe (Maybe(..))

data Action
  = Click Point
  | Move Point
  | Select Tool
  | Color Cycle ColorScheme

data Tool
  = LineTool
  | ColorTool ColorScheme

instance eqTool :: Eq Tool where
  eq LineTool LineTool = true
  eq (ColorTool c1) (ColorTool c2) = c1 == c2
  eq _ _ = false

type State =
  { graph :: Graph
  , cycles :: Map Cycle ColorScheme
  , click :: Maybe Point
  , hover :: Point
  , tool :: Tool
  }

init :: State
init =
  { graph: emptyGraph
  , cycles: empty
  , click: Nothing
  , hover: Point 0.0 0.0
  , tool: LineTool
  }

updateForClick :: Point -> State -> State
updateForClick p s@{click: Nothing} = s {click = Just p}
updateForClick p2 s@{click: Just p1}
  = s { click = Nothing
      , graph = newGraph
      , cycles = updateCycles s.cycles newGraph stroke
      }
  where
    stroke = Line p1 p2
    newGraph = addStroke stroke s.graph

snapToPoint :: Point -> State -> Point
snapToPoint p s =
  case maybeSnapPoint of
       Just snapPoint -> if (distance p snapPoint < 20.0) then snapPoint else p
       _ -> p
  where maybeSnapPoint = getNearestPoint p (keys s.graph)

update :: Action -> State -> State
update (Click p) s = updateForClick (snapToPoint p s) s
update (Move p) s = s {hover = p}
update (Select t) s = s {tool = t, click = Nothing}
update (Color cycle colorScheme) s =
  s {cycles = Map.update (\color -> (Just colorScheme)) cycle s.cycles}
