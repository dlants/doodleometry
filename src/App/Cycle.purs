module App.Cycle where

import Prelude

import App.BoundingBox as BoundingBox
import App.Geometry (SplitMap, Path, Stroke, firstPoint, flipStroke, secondPoint, toBoundingBox)
import App.Graph (Graph, edges, traverseLeftWall)
import App.Helpers (rotateList)
import CSS.Color (Color, white)
import Data.Function (on)
import Data.List (List(Nil), all, filter, foldl, head, last, mapMaybe, nub, reverse, sort, (:))
import Data.Map (Map, empty, fromFoldable, insert, lookup, member, toUnfoldable)
import Data.Maybe (Maybe(..))
import Data.Set as Set
import Data.Tuple (Tuple(..))

newtype Cycle = Cycle Path

mapToList :: forall k v. Map k v -> List (Tuple k v)
mapToList = toUnfoldable

instance cycleEq :: Eq Cycle where
  eq (Cycle p1@(a : rest)) (Cycle p2) =
    (p1 == rotateList a p2)
    || (p1 == rotateList a (flipStroke <$> reverse p2))
  eq (Cycle Nil) (Cycle Nil) = true
  eq _ _ = false

instance cycleShow :: Show Cycle where
  show (Cycle edges) = show edges

size :: Cycle -> Number
size (Cycle p) =
  case toBoundingBox <$> p of
       Nil -> 0.0
       (b: rest) -> BoundingBox.size $ foldl (<>) b rest

instance cycleOrd :: Ord Cycle where
  compare c1@(Cycle p1) c2@(Cycle p2) =
    compare (size c1) (size c2) <>
    compare (sort $ sortOrder <$> p1) (sort $ sortOrder <$> p2)
      where
        sortOrder :: Stroke -> Stroke
        sortOrder s = min s (flipStroke s)

compareSize :: Cycle -> Cycle -> Ordering
compareSize = compare `on` size

type CyclesMap = Map Cycle Color

findCycles :: Graph -> CyclesMap
findCycles g =
  foldl (\c s -> insertStroke s c g) empty (edges g)

copyColors :: CyclesMap -> CyclesMap -> CyclesMap
copyColors fromMap toMap =
  fromFoldable $ do
    (Tuple cycle@(Cycle strokes) toColor) <- toUnfoldable toMap
    stroke <- strokes
    case lookup stroke fromColors of Just fromColor -> pure $ Tuple cycle fromColor
                                     Nothing -> pure $ Tuple cycle toColor

  where
    fromColors = fromFoldable $ do
      (Tuple (Cycle cycle) color) <- toUnfoldable fromMap
      stroke <- cycle
      pure $ Tuple stroke color

updateCyclesForRemove :: CyclesMap -> Graph -> CyclesMap
updateCyclesForRemove oldCycles g =
  let newCycles = findCycles g
   in copyColors oldCycles newCycles

-- remove any cycle affected by an intersection
trimCycles :: CyclesMap -> SplitMap -> CyclesMap
trimCycles cMap splitMap =
  let unaffected stroke = not (member stroke splitMap)
      unaffectedCycle cycle@(Cycle edges) = all unaffected edges
      unaffectedCycles = filter (\(Tuple cycle _) -> unaffectedCycle cycle) (mapToList cMap)
   in fromFoldable unaffectedCycles

-- find new cycles in both directions, nub them
-- if there is 1 cycle, we haven't split any existing cycles - just add it
-- if there are 2 cycles, we split an existing cycle - we need to remove it.
insertStroke :: Stroke -> CyclesMap -> Graph -> CyclesMap
insertStroke stroke cycles g =
  let
    newCycles = nub $ mapMaybe (findCycle g) $ stroke : (flipStroke stroke) : Nil
   in foldl (\m newCycle -> insert newCycle white m) cycles newCycles

findCycle :: Graph -> Stroke -> Maybe Cycle
findCycle g stroke =
  case path of
       Nil -> Nothing
       strokes -> if firstPoint <$> head strokes == secondPoint <$> last strokes
                  then Just (Cycle path)
                  else Nothing
  where
    path = traverseLeftWall stroke g Set.empty
