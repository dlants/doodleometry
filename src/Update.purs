module App.Update where

import Prelude
import App.ColorScheme (ColorScheme(..))
import App.Cycle (Cycle(..), joinCycles)
import App.Geometry (Intersections, Path, Point(..), Stroke(..), distance, flip, getNearestPoint, split, swapEdge)
import App.Graph (Graph, addStroke, addStrokes, applyIntersections, emptyGraph, findCycle, findIntersections)
import Data.List (List(..), concat, foldr, mapMaybe, nub, (:))
import Data.Map (Map, empty, insert, keys, lookup, pop, toList)
import Data.Map (update) as Map
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..), snd)

type CyclesMap = Map Cycle ColorScheme

updateCycles :: CyclesMap -> Graph -> Intersections -> Path -> CyclesMap
updateCycles cycles g intersections splitStroke =
  let newCycles = splitCycles cycles intersections
   in foldr (\s c -> insertStroke s c g) newCycles splitStroke

splitCycles :: CyclesMap -> Intersections -> CyclesMap
splitCycles cMap intersections =
  let
    insertCycle :: (Tuple Cycle ColorScheme) -> CyclesMap -> CyclesMap
    insertCycle (Tuple c sch) cMap =
      insert (splitCycle intersections c) sch cMap
  in
    foldr insertCycle empty (toList cMap)

splitCycle :: Intersections -> Cycle -> Cycle
splitCycle intersections (Cycle path) =
  let
    maybeSwapEdge edge path =
      case lookup edge intersections of
           Just newEdges -> swapEdge path edge newEdges
           _ -> path
  in
    Cycle (foldr maybeSwapEdge path path)


-- find new cycles in both directions, nub them
-- if there is 1 cycle, we haven't split any existing cycles - just add it
-- if there are 2 cycles, we split an existing cycle. joinCycles the 2 new cycles,
-- find the existing cycle and remove it.
insertStroke :: Stroke -> CyclesMap -> Graph -> CyclesMap
insertStroke stroke cycles g =
  let
    newCycles = nub $ mapMaybe (findCycle g) $ stroke : (flip stroke) : Nil
  in
    case newCycles of
         Nil -> cycles -- no new cycles, just return old cycles
         (c : Nil) -> insert c White cycles -- one new cycle - push it on the front
         (c1 : c2 : _) -> -- two new cycles. They must have split an existing cycle
            let
              joined = joinCycles c1 c2 stroke
            in
              case pop joined cycles of
                   Just (Tuple previousColor newCycles) ->
                     insert c1 previousColor $ insert c2 previousColor $ newCycles
                   _ -> insert c1 White $ insert c2 White $ cycles
