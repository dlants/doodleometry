module App.Graph where

import Prelude
import App.Geometry (Intersections, Path, Point(..), Stroke(..), findWrap, firstPoint, flipStroke, intersectMultiple, secondPoint, unorderedEq)
import App.Helpers (rotatePast)
import Data.Foldable (foldr)
import Data.List (List(..), any, concat, delete, drop, dropWhile, elem, filter, foldl, head, insert, insertBy, last, length, mapMaybe, nub, nubBy, reverse, singleton, snoc, sort, takeWhile, (:))
import Data.Map (Map, alter, empty, keys, lookup, toList, update, values)
import Data.Maybe (Maybe(..))
import Data.Set (Set, member)
import Data.Set (insert, empty) as Set
import Data.Tuple (Tuple(..))

-- a vertex is a point along with outbound strokes, organized in clockwise order and with the vertexPoint first
type Graph = Map Point (List Stroke)

emptyGraph :: Graph
emptyGraph = empty

edges :: Graph -> List Stroke
edges g =
  nubBy unorderedEq $ concat $ values g

points :: Graph -> List Point
points g =
  keys g

pushUnique :: forall a. (Eq a) => a -> List a -> List a
pushUnique a as = if elem a as then as else a : as

-- push an ordered stroke into a graph
addStroke' :: Stroke -> Graph -> Graph
addStroke' s g =
  alter pushStrokeToPoint (firstPoint s) g
    where
      pushStrokeToPoint Nothing = Just (singleton s)
      pushStrokeToPoint (Just list) = Just (nub $ insert s list)

-- push an unordered stroke into a graph
addStroke :: Stroke -> Graph -> Graph
addStroke s g =
  addStroke' (flipStroke s) $ addStroke' s g

addStrokes :: List Stroke -> Graph -> Graph
addStrokes strokes g =
  foldl (\g' s -> addStroke s g') g strokes

-- remove an ordered stroke from a graph
removeStroke' :: Stroke -> Graph -> Graph
removeStroke' stroke g =
  update removeFromEdges (firstPoint stroke) g
    where
      removeFromEdges edges =
        case delete stroke edges of
             Nil -> Nothing
             l -> Just l

-- remove an unordered stroke from a graph
removeStroke :: Stroke -> Graph -> Graph
removeStroke stroke g =
  removeStroke' stroke $ removeStroke' (flipStroke stroke) $ g

-- remove multiple unordered strokes from the graph
removeMultiple :: List Stroke -> Graph -> Graph
removeMultiple strokes g =
  foldl (flip removeStroke) g strokes

cleanGraph :: Graph -> Graph
cleanGraph g =
  let alterVertex :: Maybe (List Stroke) -> Maybe (List Stroke)
      alterVertex (Just edges) | length edges == 0 = Nothing
      alterVertex a = a
   in foldl (\g' pt -> alter alterVertex pt g') g (keys g)

-- if stroke is (p1 p2) next edge should be (p2 p3), clockwise out out p2
getNextEdge :: Stroke -> Graph -> Maybe Stroke
getNextEdge stroke g =
  case lookup (secondPoint stroke) g of
       Nothing -> Nothing
       (Just strokes) ->
          -- strokes are all of form (p2 _)
          head $ rotatePast (flipStroke stroke) strokes

traverseLeftWall :: Stroke -> Graph -> Set Stroke -> Path
traverseLeftWall s g visited =
  if member s visited then Nil
                      else s :
                        case getNextEdge s g of Just nextEdge -> traverseLeftWall nextEdge g (Set.insert s visited)
                                                _ -> Nil

findIntersections :: Stroke -> Graph -> Intersections
findIntersections stroke g =
  intersectMultiple stroke (edges g)

applyIntersections :: Intersections -> Graph -> Graph
applyIntersections intersections g =
  cleanGraph $ foldr applyIntersection g $ toList intersections
  where
    applyIntersection (Tuple stroke strokes) g =
      addStrokes strokes $ removeStroke stroke g
