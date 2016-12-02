module App.Graph where

import Prelude
import App.Geometry (Point, Stroke(..), firstPoint, isCycle, reverse, secondPoint)
import Control.MonadZero (guard)
import Data.List (List(..), dropWhile, elem, elemIndex, head, insert, singleton, (:))
import Data.Map (Map, alter, empty, lookup, toList)
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))

-- a vertex is a point along with outbound strokes, organized in clockwise order and with the vertexPoint first
type Graph = Map Point (List Stroke)

emptyGraph :: Graph
emptyGraph = empty

pushUnique :: forall a. (Eq a) => a -> List a -> List a
pushUnique a as =
  case elemIndex a as of
    Nothing -> a : as
    _ -> as

-- push an ordered stroke into a graph
addStroke' :: Stroke -> Graph -> Graph
addStroke' s@(Line p1 p2) g =
  alter pushStrokeToPoint p1 g
    where
      pushStrokeToPoint Nothing = Just (singleton s)
      pushStrokeToPoint (Just list) = Just (insert s list)

-- push an unordered stroke into a graph
addStroke :: Stroke -> Graph -> Graph
addStroke s g =
  addStroke' (reverse s) $ addStroke' s g

-- get the element after el in list, with wraparound
nextElement :: forall a. (Eq a) => a -> List a -> Maybe a
nextElement el list =
  case dropWhile (\a -> a /= el) list of
       (a : b : _) -> Just b
       (a : Nil) -> head list
       _ -> Nothing

getNextRightEdge :: Stroke -> Graph -> Maybe Stroke
getNextRightEdge stroke g =
  case lookup p g of -- stroke should be Line p q
       Nothing -> Nothing
       (Just strokes) ->
         case nextElement stroke strokes of
           Just nextStroke | stroke /= nextStroke -> Just (reverse nextStroke) -- Line _ p
           _ -> Nothing
  where
    p = firstPoint stroke

traverseRight :: List Stroke -> Graph -> List Stroke
traverseRight path@(stroke: _) g =
  case getNextRightEdge stroke g of
    (Just nextStroke) ->
      if elem nextStroke path then path
                              else traverseRight (nextStroke : path) g
    _ -> path

traverseRight _ _ = Nil

findCycles :: Graph -> List (List Stroke)
findCycles g = do
  (Tuple pt strokes) <- toList g
  startStroke <- strokes
  let path = traverseRight (singleton startStroke) g
  guard $ isCycle path
  pure path
