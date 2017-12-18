module App.Events where

import App.Background (Background)
import App.Cycle (Cycle, findCycles)
import App.Geometry (Point, Stroke(Line, Arc))
import App.Graph (applyIntersections, edges, findIntersections, removeMultiple)
import App.Snap (snapToPoint)
import App.State (State, Tool(LineTool, ArcTool, EraserTool))
import CSS.Color (Color)
import Data.Function (($))
import Data.List (List(..), singleton, (:))
import Data.Map (keys, lookup)
import Data.Map (update) as Map
import Data.Maybe (Maybe(..))
import KeyDown (KeyData)
import Prelude ((==))
import Pux (EffModel, noEffects)

data Event
  = Draw Point
  | EraserDown Point
  | EraserMove Point
  | EraserUp Point
  | Move Point
  | Select Tool
  | SelectCycle Cycle
  | ApplyColor Cycle Color
  | WindowResize Int Int
  | ChangeBackground Background
  | Key KeyData
  | NoOp

foldp :: forall fx. Event -> State -> EffModel State Event fx
foldp evt st = noEffects $ update evt st

update :: Event -> State -> State
update (Draw p) s =
  let newPt = case snapToPoint p s.background (edges s.drawing.graph) of
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
  let sp = snapToPoint p s.background (edges s.drawing.graph)
      newPt = case sp of Just sp' -> sp'
                         _ -> p
   in s { hover = Just p
        , currentStroke = newStroke s newPt
        , snapPoint = sp
        }

update (Select tool) s
  = s { tool = tool
      , click = Nothing
      , hover = Nothing
      , snapPoint = Nothing
      , currentStroke = Nothing
      , selection = Nil
      }

update (ApplyColor cycle color) s =
  s { drawing =
      { cycles: Map.update (\c -> (Just color)) cycle s.drawing.cycles
      , graph: s.drawing.graph
      }
    , undos = s.drawing : s.undos
    , redos = Nil
    }

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

update (Key k) s =
  let undoState = case s.undos of Nil -> s
                                  lastDrawing : rest -> s { drawing = lastDrawing
                                                          , undos = rest
                                                          , redos = s.drawing : s.redos
                                                          }

      redoState = case s.redos of Nil -> s
                                  nextDrawing : rest -> s { drawing = nextDrawing
                                                          , undos = s.drawing : s.undos
                                                          , redos = rest
                                                          }

   in case k of {code: "KeyZ", meta: true, shift: false} -> undoState
                {code: "KeyZ", ctrl: true, shift: false} -> undoState
                {code: "KeyZ", meta: true, shift: true} -> redoState
                {code: "KeyZ", ctrl: true, shift: true} -> redoState
                _ -> s

update (SelectCycle cycle) state = state {selection = singleton cycle}

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
  s { drawing =
      { graph: newGraph
      , cycles: newCycles
      }
    , lastEraserPoint = Just ptTo
    , undos = s.drawing : s.undos
    , redos = Nil
    }
  where
    intersections = findIntersections (Line ptFrom ptTo) s.drawing.graph
    newGraph = removeMultiple (keys intersections) s.drawing.graph
    newCycles = findCycles newGraph

updateForStroke :: State -> Stroke -> State
updateForStroke s stroke
  = s { drawing =
        { graph: newGraph
        , cycles: newCycles
        }
      , undos = s.drawing : s.undos
      , redos = Nil
      }
  where
    intersections = findIntersections stroke s.drawing.graph
    splitStroke = case lookup stroke intersections of
                       Just ss -> ss
                       _ -> singleton stroke
    newGraph = applyIntersections intersections s.drawing.graph
    newCycles = findCycles newGraph
