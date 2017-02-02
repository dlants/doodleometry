module App.Model where

import Prelude
import App.ColorScheme (ColorScheme(..))
import App.Cycle (Cycle(..), findCycles, updateCyclesForInsert, updateCyclesForRemove)
import App.Geometry (Point(..), Stroke(..), distance, getNearestPoint, split)
import App.Graph (Graph, applyIntersections, edges, emptyGraph, findIntersections, removeMultiple)
import App.Snap (snapToPoint)
import Data.List (List(..), concat, mapMaybe, nub, singleton, (:))
import Data.Map (Map, empty, insert, keys, lookup, pop)
import Data.Map (update) as Map
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..), snd)
import Signal (Signal)

data Action
  = Click Point
  | EraserDown Point
  | EraserMove Point
  | EraserUp Point
  | Move Point
  | Select Tool
  | Color Cycle ColorScheme
  | WindowResize Int Int

data Tool
  = LineTool
  | ArcTool
  | ColorTool ColorScheme
  | EraserTool

derive instance eqTool :: Eq Tool

type State =
  { graph :: Graph
  , cycles :: Map Cycle ColorScheme
  , click :: Maybe Point
  , hover :: Maybe Point
  , snapPoint :: Maybe Point
  , lastEraserPoint :: Maybe Point
  , currentStroke :: Maybe Stroke
  , tool :: Tool
  , windowWidth :: Int
  , windowHeight :: Int
  }

-- the actual width and height will be overwritten in index.js
init :: State
init =
  { graph: emptyGraph
  , cycles: empty
  , click: Nothing
  , hover: Nothing
  , snapPoint: Nothing
  , lastEraserPoint: Nothing
  , currentStroke: Nothing
  , tool: LineTool
  , windowWidth: 0
  , windowHeight: 0
  }


inputs :: Array (Signal Action)
inputs = []

newStroke :: State -> Point -> Maybe Stroke
newStroke s p =
  case s.click of
       Just c ->
          case s.tool of
               ArcTool -> Just $ (Arc c p p true)
               LineTool -> Just $ Line c p
               _ -> Nothing

       Nothing -> Nothing

update :: Action -> State -> State
update (Click p) s =
  let newPt = case snapToPoint p (edges s.graph) of
                   Just sp -> sp
                   _ -> p
   in case s.click of
       Nothing -> s { click = Just newPt }
       Just c ->
         case newStroke s newPt of
              Nothing -> s
              Just stroke -> (updateForStroke s stroke) { click = Nothing
                                                        , currentStroke = Nothing
                                                        , hover = Nothing
                                                        , snapPoint = Nothing
                                                        }

update (Move p) s =
  let sp = snapToPoint p (edges s.graph)
      newPt = case sp of Just sp -> sp
                         _ -> p
   in s { hover = Just p
        , currentStroke = newStroke s newPt
        , snapPoint = sp
        }

update (Select t) s
  = s { tool = t
      , click = Nothing
      , hover = Nothing
      , snapPoint = Nothing
      , currentStroke = Nothing
      }

update (Color cycle colorScheme) s =
  s {cycles = Map.update (\color -> (Just colorScheme)) cycle s.cycles}

update (EraserDown pt) s =
  let eraserPt = if s.tool == EraserTool then Just pt else Nothing
   in s {lastEraserPoint = eraserPt}

update (EraserUp pt) s = s {lastEraserPoint = Nothing}

update (EraserMove pt) s =
  case s.lastEraserPoint of Nothing -> s
                            Just eraserPt -> eraseLine s eraserPt pt

update (WindowResize w h) s =
  s {windowWidth = w, windowHeight = h}

eraseLine :: State -> Point -> Point -> State
eraseLine s ptFrom ptTo =
  s { graph = newGraph
    , cycles = newCycles
    , lastEraserPoint = Just ptTo
    }
  where
    intersections = findIntersections (Line ptFrom ptTo) s.graph
    newGraph = removeMultiple (keys intersections) s.graph
    newCycles = updateCyclesForRemove s.cycles newGraph

updateForStroke :: State -> Stroke -> State
updateForStroke s stroke
  = s { graph = newGraph
      , cycles = newCycles
      }
  where
    intersections = findIntersections stroke s.graph
    splitStroke = case lookup stroke intersections of
                       Just ss -> ss
                       _ -> singleton stroke
    newGraph = applyIntersections intersections s.graph
    newCycles = updateCyclesForInsert s.cycles newGraph intersections
