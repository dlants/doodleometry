module App.Cycle where

import Prelude
import App.Geometry (Path, Stroke(..), orderedEq, flip)
import App.Helpers (rotateList)
import Data.List (List(..), any, drop, head, last, reverse, sort, (:))
import Data.Maybe (Maybe(..))

isCycle :: List Stroke -> Boolean
isCycle strokes = compareFirstLast (head strokes) (last strokes)
  where
    compareFirstLast (Just (Line p1 _)) (Just (Line _ p4)) = p1 == p4
    compareFirstLast _ _ = false

newtype Cycle = Cycle Path

instance cycleEq :: Eq Cycle where
  eq (Cycle p1@(a : rest)) (Cycle p2) =
    (p1 == rotateList a p2)
    || (p1 == rotateList a (flip <$> reverse p2))
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
    hasEdge = any (orderedEq edge) edges
  in
    if hasEdge then drop 1 $ rotateList edge edges
               else drop 1 $ rotateList edge (flip <$> reverse edges)

-- TODO: make this work with multiple shared edges
-- for now, assumes a single shared edge
joinCycles :: Cycle -> Cycle -> Stroke -> Cycle
joinCycles c1 c2 stroke =
  let
    path1 = cut c1 stroke -- if stroke is p1p2, path1 takes from p2 to p1
    path2 = cut c2 (flip stroke) -- if stroke is p1p2, path2 takes from p1 to p2
  in
    Cycle (path1 <> path2)
