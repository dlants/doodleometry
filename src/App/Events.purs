module App.Events where

import App.Background (Background)
import App.ColorScheme (ColorScheme)
import App.Cycle (Cycle, updateCycles, updateCyclesForRemove)
import App.Geometry (Point, Stroke(Line, Arc))
import App.Graph (applyIntersections, edges, findIntersections, removeMultiple)
import App.Snap (snapToPoint)
import App.State (State, Tool(LineTool, ArcTool, EraserTool))
import Data.Function (($))
import Data.List (singleton)
import Data.Map (keys, lookup)
import Data.Map (update) as Map
import Data.Maybe (Maybe(..))
import Network.HTTP.Affjax (AJAX)
import Prelude (unit, (==))
import Pux (EffModel, noEffects)

data Event
  = Click Point
  | EraserDown Point
  | EraserMove Point
  | EraserUp Point
  | Move Point
  | Select Tool
  | Color Cycle ColorScheme
  | WindowResize Int Int
  | ChangeBackground Background
  | NoOp


type AppEffects fx = (ajax :: AJAX | fx)

foldp :: ∀ fx. Event -> State -> EffModel State Event (AppEffects fx)
foldp evt st = noEffects $ update evt st

update :: Event -> State -> State
update (Click p) s =
  let newPt = case snapToPoint p s.background (edges s.graph) of
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
  let sp = snapToPoint p s.background (edges s.graph)
      newPt = case sp of Just sp' -> sp'
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

update (ChangeBackground b) s =
  s {background = b, snapPoint = Nothing}

update NoOp s = s

newStroke :: State -> Point -> Maybe Stroke
newStroke s p =
  case s.click of
       Just c ->
          case s.tool of
               ArcTool -> Just $ (Arc c p p true)
               LineTool -> Just $ Line c p
               _ -> Nothing

       Nothing -> Nothing

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
    newCycles = updateCycles s.cycles newGraph intersections
