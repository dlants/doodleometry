module App.Model where

import Prelude
import App.ColorScheme (ColorScheme(..))
import App.Cycle (Cycle(..), joinCycles)
import App.Geometry (Point(..), Stroke(..), distance, getNearestPoint, split)
import App.Graph (Graph, addStrokes, applyIntersections, emptyGraph, findIntersections)
import App.Update (updateCycles)
import Data.List (List(..), concat, mapMaybe, nub, singleton, (:))
import Data.Map (Map, empty, insert, keys, lookup, pop)
import Data.Map (update) as Map
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..), snd)

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
  , hover :: Maybe Point
  , tool :: Tool
  }

init :: State
init =
  { graph: emptyGraph
  , cycles: empty
  , click: Nothing
  , hover: Nothing
  , tool: LineTool
  }

snapToPoint :: Point -> State -> Point
snapToPoint p s =
  case maybeSnapPoint of
       Just snapPoint -> if (distance p snapPoint < 20.0) then snapPoint else p
       _ -> p
  where maybeSnapPoint = getNearestPoint p (keys s.graph)

update :: Action -> State -> State
update (Click p) s = updateForClick (snapToPoint p s) s
update (Move p) s = s {hover = Just p}
update (Select t) s = s {tool = t, click = Nothing, hover = Nothing}
update (Color cycle colorScheme) s =
  s {cycles = Map.update (\color -> (Just colorScheme)) cycle s.cycles}

updateForClick :: Point -> State -> State
updateForClick p s@{click: Nothing} = s {click = Just p}
updateForClick p2 s@{click: Just p1}
  = s { click = Nothing
      , graph = newGraph
      , cycles = newCycles
      }
  where
    stroke = Line p1 p2
    intersections = findIntersections stroke s.graph
    splitStroke = case lookup stroke intersections of
                       Just ss -> ss
                       _ -> singleton stroke
    newGraph = addStrokes splitStroke $ applyIntersections intersections s.graph
    newCycles = updateCycles s.cycles newGraph intersections splitStroke
