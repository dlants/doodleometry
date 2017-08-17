module App.Graph where

import Prelude

import App.Geometry (Intersections, Path, Point(..), Stroke(..), findWrap, firstPoint, flipStroke, intersectMultiple, secondPoint, unorderedEq)
import App.Helpers (rotatePast)
import Data.Foldable (fold, foldr)
import Data.List (List(..), any, concat, delete, drop, dropWhile, elem, filter, foldl, fromFoldable, head, insert, insertBy, last, length, mapMaybe, nub, nubBy, reverse, singleton, snoc, sort, takeWhile, (:))
import Data.Map (Map, alter, empty, keys, lookup, toUnfoldable, update, values)
import Data.Maybe (Maybe(..))
import Data.Set (Set, member)
import Data.Set (insert, empty) as Set
import Data.Tuple (Tuple(..))

mapToList :: forall k v. Map k v -> List (Tuple k v)
mapToList = toUnfoldable

-- a vertex is a point along with outbound strokes, organized in clockwise order and with the vertexPoint first
newtype Graph = Graph (Map Point (List Stroke))
instance graphShow :: Show Graph where
  show (Graph g) =
    let showPt :: (Tuple Point (List Stroke)) -> String
        showPt (Tuple pt edges) = (show pt) <> ": " <> show edges
     in "(Graph " <> (fold $ showPt <$> mapToList g) <> ")"

emptyGraph :: Graph
emptyGraph = Graph empty

edges :: Graph -> List Stroke
edges (Graph g) =
  nubBy unorderedEq $ concat $ values g

points :: Graph -> List Point
points (Graph g) =
  keys g

getEdgesForPt :: Point -> Graph -> Maybe (List Stroke)
getEdgesForPt p (Graph g) = lookup p g

pushUnique :: forall a. (Eq a) => a -> List a -> List a
pushUnique a as = if elem a as then as else a : as

-- push an ordered stroke into a graph
addStroke' :: Stroke -> Graph -> Graph
addStroke' s (Graph g) =
  Graph $ alter pushStrokeToPoint (firstPoint s) g
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
removeStroke' stroke (Graph g) =
  Graph $ update removeFromEdges (firstPoint stroke) g
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
cleanGraph (Graph g) =
  let alterVertex :: Maybe (List Stroke) -> Maybe (List Stroke)
      alterVertex (Just edges) | length edges == 0 = Nothing
      alterVertex a = a
   in Graph $ foldl (\g' pt -> alter alterVertex pt g') g (keys g)

-- if stroke is (p1 p2) next edge should be (p2 p3), clockwise out out p2
getNextEdge :: Stroke -> Graph -> Maybe Stroke
getNextEdge stroke (Graph g) =
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
  cleanGraph $ foldr applyIntersection g $ mapToList intersections
  where
    applyIntersection (Tuple stroke strokes) g =
      addStrokes strokes $ removeStroke stroke g
