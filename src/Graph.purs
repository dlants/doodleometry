module App.Graph where

import Prelude
import App.Geometry (Point, Stroke(..), firstPoint, isCycle, reverse, secondPoint)
import Control.MonadZero (guard)
import Data.List (List(..), concat, drop, dropWhile, elem, elemIndex, head, insert, singleton, takeWhile, (:))
import Data.List.Lazy (filter, head) as Lazy
import Data.Map (Map, alter, empty, lookup, toList)
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Data.Unfoldable (unfoldr)

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

getNextEdges :: Path -> Graph -> List Stroke
getNextEdges (stroke : _) g =
  case lookup p g of -- stroke should be Line p q
       Nothing -> Nil
       (Just strokes) ->
          let unequal = (\a -> a /= stroke)
              front = takeWhile unequal strokes
              back = drop 1 $ dropWhile unequal strokes -- drop stroke itself
          in
            reverse <$> (back <> front)
  where
    p = firstPoint stroke

getNextEdges _ _ = Nil

type Path = List Stroke
data Traversal = Traversal (List Path) Graph

{--
at each iteration, pop the first path. Put possible extensions at
the front of the "toexplore" list.
--}
traverseRight :: Traversal -> Maybe (Tuple Path Traversal)
traverseRight (Traversal (path : rest) g) =
  case path of
       (stroke : strokes) | not elem stroke strokes ->
         Just (Tuple path (pushTraversals path rest))
       -- don't do anything with a path that loops on itself
       _ -> traverseRight $ Traversal rest g
  where
    nextPaths = (\s -> s : path) <$> getNextEdges path g
    pushTraversals path paths =
      Traversal (nextPaths <> paths) g

traverseRight _ = Nothing

findCycle :: Stroke -> Graph -> Maybe Path
findCycle stroke g =
  Lazy.head
  $ Lazy.filter isCycle
  $ unfoldr traverseRight (Traversal (singleton $ singleton stroke) g)

findCycles :: Graph -> List (List Stroke)
findCycles g = do
  (Tuple pt strokes) <- toList g
  startStroke <- strokes
  case findCycle startStroke g of
       Just path -> pure path
       _ -> Nil
