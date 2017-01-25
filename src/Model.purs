module App.Model where

import Prelude
import App.ColorScheme (ColorScheme(..))
import App.Cycle (Cycle(..), findCycles, updateCycles)
import App.Geometry (Point(..), Stroke(..), distance, getNearestPoint, split)
import App.Graph (Graph, applyIntersections, edges, emptyGraph, findIntersections)
import App.Snap (snapToPoint)
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
  , snapPoint :: Maybe Point
  , currentStroke :: Maybe Stroke
  , tool :: Tool
  }

init :: State
init =
  { graph: emptyGraph
  , cycles: empty
  , click: Nothing
  , hover: Nothing
  , snapPoint: Nothing
  , currentStroke: Nothing
  , tool: LineTool
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
    newCycles = findCycles newGraph
