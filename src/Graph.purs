module App.Graph where

import Prelude
import App.ColorScheme (ColorScheme(..))
import App.Cycle (Cycle(..), isCycle)
import App.Geometry (Intersections, Path, Point(..), Stroke(..), compareClockwise, firstPoint, flip, intersect, intersectMultiple, orderedEq, secondPoint, split, findWrap)
import App.Helpers (rotateList)
import Control.MonadPlus (guard)
import Data.Foldable (foldr)
import Data.List (List(..), any, concat, delete, drop, dropWhile, elem, filter, head, insertBy, mapMaybe, nub, reverse, singleton, sort, takeWhile, (:))
import Data.List.Lazy (filter, head) as Lazy
import Data.Map (Map, alter, empty, insert, keys, lookup, pop, toList, update, values)
import Data.Maybe (Maybe(..))
import Data.Ord (Ordering(..))
import Data.Tuple (Tuple(..))
import Data.Unfoldable (unfoldr)

-- a vertex is a point along with outbound strokes, organized in clockwise order and with the vertexPoint first
type Graph = Map Point (List Stroke)

emptyGraph :: Graph
emptyGraph = empty

edges :: Graph -> List Stroke
edges g =
  nub $ concat $ values g

pushUnique :: forall a. (Eq a) => a -> List a -> List a
pushUnique a as = if elem a as then as else a : as

-- push an ordered stroke into a graph
addStroke' :: Stroke -> Graph -> Graph
addStroke' s@(Line p1 p2) g =
  alter pushStrokeToPoint p1 g
    where
      pushStrokeToPoint Nothing = Just (singleton s)
      pushStrokeToPoint (Just list) = Just (insertBy compareClockwise s list)

-- push an unordered stroke into a graph
addStroke :: Stroke -> Graph -> Graph
addStroke s g =
  addStroke' (flip s) $ addStroke' s g

addStrokes :: List Stroke -> Graph -> Graph
addStrokes strokes g =
  foldr (\s g' -> addStroke s g') g strokes

-- remove an ordered stroke from a graph
removeStroke' :: Stroke -> Graph -> Graph
removeStroke' stroke g =
  foldr updatePoint g (keys g)
    where
      updatePoint pt g = update removeFromEdges pt g
      removeFromEdges edges =
        case delete stroke edges of
             Nil -> Nothing
             l -> Just l

-- remove an unordered stroke from a graph
removeStroke :: Stroke -> Graph -> Graph
removeStroke stroke g =
  removeStroke' stroke $ removeStroke' (flip stroke) $ g

getNextEdge :: Stroke -> Graph -> Maybe Stroke
getNextEdge stroke g =
  case lookup (firstPoint stroke) g of
       Nothing -> Nothing
       (Just strokes) ->
          head $ flip <$> (drop 1 $ rotateList stroke strokes) -- drop the stroke itself

findCycle :: Graph -> Stroke -> Maybe Cycle
findCycle g stroke =
  if (findWrap path > 0.0) then Just (Cycle path) else Nothing
  where
    beginning = secondPoint stroke
    notAtBeginning nextStroke = secondPoint nextStroke /= beginning

    traverseRight s =
      case (getNextEdge s g) of
           Just nextEdge -> Just (Tuple nextEdge nextEdge)
           _ -> Nothing

    -- path is (p1, _) : ... : (_, p1)
    path = stroke : (takeWhile notAtBeginning $ unfoldr traverseRight stroke)

findIntersections :: Stroke -> Graph -> Intersections
findIntersections stroke g =
  intersectMultiple stroke (edges g)

applyIntersections :: Intersections -> Graph -> Graph
applyIntersections intersections g =
  foldr applyIntersection g $ toList intersections
  where
    applyIntersection (Tuple stroke strokes) g =
      addStrokes strokes $ removeStroke stroke g
