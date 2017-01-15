module App.Model where

import Prelude
import App.ColorScheme (ColorScheme(..))
import App.Cycle (Cycle(..), updateCycles)
import App.Geometry (Point(..), Stroke(..), constructArc, distance, getNearestPoint, split)
import App.Graph (Graph, applyIntersections, emptyGraph, findIntersections)
import Data.List (List(..), concat, mapMaybe, nub, singleton, (:))
import Data.Map (Map, empty, insert, keys, lookup, pop)
import Data.Map (update) as Map
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..), snd)
import Debug.Trace (spy)

data Action
  = Click Point
  | Move Point
  | Select Tool
  | Color Cycle ColorScheme

data Tool
  = LineTool
  | ArcTool
  | ColorTool ColorScheme

instance eqTool :: Eq Tool where
  eq LineTool LineTool = true
  eq ArcTool ArcTool = true
  eq (ColorTool c1) (ColorTool c2) = c1 == c2
  eq _ _ = false

type State =
  { graph :: Graph
  , cycles :: Map Cycle ColorScheme
  , click :: Maybe Point
  , hover :: Maybe Point
  , currentStroke :: Maybe Stroke
  , tool :: Tool
  }

init :: State
init =
  { graph: emptyGraph
  , cycles: empty
  , click: Nothing
  , hover: Nothing
  , currentStroke: Nothing
  , tool: LineTool
  }

snapToPoint :: Point -> State -> Point
snapToPoint p s =
  case maybeSnapPoint of
       Just snapPoint -> if (distance p snapPoint < 20.0) then snapPoint else p
       _ -> p
  where maybeSnapPoint = getNearestPoint p (keys s.graph)

newStroke :: State -> Point -> Maybe Stroke
newStroke s p =
  case s.click of
       Just c ->
         let snappedPoint = snapToPoint p s
          in case s.tool of
                  ArcTool -> Just $ constructArc c snappedPoint
                  LineTool -> Just $ Line c snappedPoint
                  _ -> Nothing

       Nothing -> Nothing

update :: Action -> State -> State
update (Click p) s =
  case s.click of
       Nothing -> s {click = Just $ snapToPoint p s}
       Just c ->
         case newStroke s p of
              Nothing -> s
              Just stroke -> updateForStroke s (spy stroke)

update (Move p) s = s {hover = Just p, currentStroke = newStroke s p}

update (Select t) s = s {tool = t, click = Nothing, hover = Nothing, currentStroke = Nothing}

update (Color cycle colorScheme) s =
  s {cycles = Map.update (\color -> (Just colorScheme)) cycle s.cycles}

updateForStroke :: State -> Stroke -> State
updateForStroke s stroke
  = s { click = Nothing
      , currentStroke = Nothing
      , graph = newGraph
      , cycles = newCycles
      }
  where
    intersections = findIntersections stroke s.graph
    splitStroke = case lookup stroke intersections of
                       Just ss -> ss
                       _ -> singleton stroke
    newGraph = applyIntersections intersections s.graph
    newCycles = updateCycles s.cycles newGraph intersections splitStroke
