module App.Events where

import App.Background (Background)
import App.Cycle (Cycle, findCycles)
import App.Geometry (Point, Stroke(Line, Arc), closeToPoint)
import App.Graph (applyIntersections, edges, findIntersections, removeMultiple)
import App.Snap (snapToPoint, snapPoints)
import App.State (State, Tool(..))
import CSS.Color (Color)
import Data.Function (($))
import Data.List (List(..), filter, singleton, (:))
import Data.Map (keys, lookup)
import Data.Map (update) as Map
import Data.Maybe (Maybe(..))
import KeyDown (KeyData)
import Mouse (MouseData(..))
import Prelude ((==))
import Pux (EffModel, noEffects)

data Event
  = Select Tool
  | SelectCycle Cycle
  | ApplyColor Cycle Color
  | WindowResize Int Int
  | ChangeBackground Background
  | Key KeyData
  | Mouse MouseData
  | NoOp

foldp :: forall fx. Event -> State -> EffModel State Event fx
foldp evt st = noEffects $ update evt st



update :: Event -> State -> State
update (Mouse (MouseDown p)) s =
  case s.tool of LineTool -> drawPoint s p
                 ArcTool -> drawPoint s p
                 EraserTool opts -> erase s {tool = EraserTool opts {down=true, pt=p}}
                 _ -> s

update (Mouse (MouseMove p)) s =
  case s.tool of LineTool -> updateSnapPoint s p
                 ArcTool -> updateSnapPoint s p
                 EraserTool opts -> erase s {tool = EraserTool opts {pt=p}}
                 _ -> s

update (Mouse (MouseUp pt)) s =
  case s.tool of EraserTool opts -> s {tool = EraserTool opts {down=false, pt=pt}}
                 _ -> s

update (Select tool) s
  = s { tool = tool
      , click = Nothing
      , hover = Nothing
      , snapPoint = Nothing
      , currentStroke = Nothing
      , selection = Nil
      }

update (ApplyColor cycle color) s =
  s { drawing { cycles = Map.update (\c -> (Just color)) cycle s.drawing.cycles }
    , undos = s.drawing : s.undos
    , redos = Nil
    }

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

drawPoint :: State -> Point -> State
drawPoint s p =
  let newPt = case snapToPoint p s.background s.drawing.snapPoints of
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

updateSnapPoint :: State -> Point -> State
updateSnapPoint s p =
  let sp = snapToPoint p s.background s.drawing.snapPoints
      newPt = case sp of Just sp' -> sp'
                         _ -> p
   in s { hover = Just p
        , currentStroke = newStroke s newPt
        , snapPoint = sp
        }


newStroke :: State -> Point -> Maybe Stroke
newStroke s p =
  case s.click of
       Just c ->
          case s.tool of
               ArcTool -> Just $ (Arc c p p true)
               LineTool -> Just $ Line c p
               _ -> Nothing

       Nothing -> Nothing

-- erase around the given point
erase :: State -> State
erase s@{tool: EraserTool {down: true, pt}} =
  s { drawing =
      { graph: newGraph
      , cycles: newCycles
      , snapPoints: snapPoints newGraph
      }
    , undos = s.drawing : s.undos
    , redos = Nil
    }
  where
    erasedStrokes = filter (closeToPoint pt 20.0) $ edges s.drawing.graph
    newGraph = if erasedStrokes == Nil then s.drawing.graph else removeMultiple erasedStrokes s.drawing.graph
    newCycles = if erasedStrokes == Nil then s.drawing.cycles else findCycles newGraph
erase s = s

updateForStroke :: State -> Stroke -> State
updateForStroke s stroke
  = s { drawing =
        { graph: newGraph
        , cycles: newCycles
        , snapPoints: snapPoints newGraph
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
