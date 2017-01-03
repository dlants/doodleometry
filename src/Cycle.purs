module App.Cycle where

import Prelude
import App.ColorScheme (ColorScheme(..))
import App.Geometry (Intersections, Path, Stroke, findWrap, firstPoint, flipStroke, secondPoint, swapEdge)
import App.Graph (Graph, traverseLeftWall)
import App.Helpers (rotateList)
import Data.List (List(..), delete, drop, elem, foldr, head, last, mapMaybe, nub, reverse, sort, (:))
import Data.Map (Map, empty, insert, lookup, pop, toList)
import Data.Maybe (Maybe(..))
import Data.Set (Set)
import Data.Set (empty) as Set
import Data.Tuple (Tuple(..))

newtype Cycle = Cycle Path

instance cycleEq :: Eq Cycle where
  eq (Cycle p1@(a : rest)) (Cycle p2) =
    (p1 == rotateList a p2)
    || (p1 == rotateList a (flipStroke <$> reverse p2))
  eq (Cycle Nil) (Cycle Nil) = true
  eq _ _ = false

instance cycleShow :: Show Cycle where
  show (Cycle edges) = show edges

instance cycleOrd :: Ord Cycle where
  compare (Cycle p1) (Cycle p2) =
    compare (sort p1) (sort p2)

cut :: Cycle -> Stroke -> Path
cut (Cycle edges) edge =
  let
    hasEdge = elem edge edges
  in
    if hasEdge then drop 1 $ rotateList edge edges
               else drop 1 $ rotateList edge (flipStroke <$> reverse edges)

-- when we traverse to find a cycle, we can end up visiting an edge and then going back along that same edge
-- like O--
-- simplify removes such edges from the cycle, leaving O
simplify :: Path -> Path
simplify (stroke : rest) =
  if elem (flipStroke stroke) rest then simplify (delete (flipStroke stroke) rest)
                                   else stroke : (simplify rest)
simplify Nil = Nil

-- TODO: make this work with multiple shared edges
-- for now, assumes a single shared edge
joinCycles :: Cycle -> Cycle -> Stroke -> Cycle
joinCycles c1 c2 stroke =
  let
    path1 = cut c1 stroke -- if stroke is p1p2, path1 takes from p2 to p1
    path2 = cut c2 (flipStroke stroke) -- if stroke is p1p2, path2 takes from p1 to p2
  in
    Cycle (simplify $ path1 <> path2)

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
-- if there are 2 cycles, we split an existing cycle - we need to remove it.
insertStroke :: Stroke -> CyclesMap -> Graph -> CyclesMap
insertStroke stroke cycles g =
  let
    newCycles = nub $ mapMaybe (findCycle g) $ stroke : (flipStroke stroke) : Nil
  in
    case newCycles of
         Nil -> cycles -- no new cycles, just return old cycles
         (c : Nil) -> insert c White cycles -- one new cycle - push it on the front
         (c1 : c2 : _) -> -- two new cycles. They may have split an existing cycle
            let
              joined = joinCycles c1 c2 stroke
            in
              case pop joined cycles of
                   Just (Tuple previousColor newCycles) ->
                     insert c1 previousColor $ insert c2 previousColor $ newCycles
                   _ -> insert c1 White $ insert c2 White $ cycles

findCycle :: Graph -> Stroke -> Maybe Cycle
findCycle g stroke =
  case path of
       Nil -> Nothing
       strokes -> if firstPoint <$> head strokes == secondPoint <$> last strokes
                     then if findWrap path > 0.0 then Just (Cycle path)
                                                 else Nothing
                     else Nothing
  where
    path = simplify $ traverseLeftWall stroke g Set.empty
