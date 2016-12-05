module App.Graph where

import Prelude
import App.Geometry (Point, Stroke(..), firstPoint, isCycle, reverse) as G
import Control.MonadZero (guard)
import Data.List (List(..), drop, dropWhile, elem, insert, nub, reverse, singleton, takeWhile, (:))
import Data.List.Lazy (filter, head) as Lazy
import Data.Map (Map, alter, empty, lookup, toList)
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Data.Unfoldable (unfoldr)

-- a vertex is a point along with outbound strokes, organized in clockwise order and with the vertexPoint first
type Graph = Map G.Point (List G.Stroke)

emptyGraph :: Graph
emptyGraph = empty

pushUnique :: forall a. (Eq a) => a -> List a -> List a
pushUnique a as = if elem a as then as else a : as

-- push an ordered stroke into a graph
addStroke' :: G.Stroke -> Graph -> Graph
addStroke' s@(G.Line p1 p2) g =
  alter pushStrokeToPoint p1 g
    where
      pushStrokeToPoint Nothing = Just (singleton s)
      pushStrokeToPoint (Just list) = Just (insert s list)

-- push an unordered stroke into a graph
addStroke :: G.Stroke -> Graph -> Graph
addStroke s g =
  addStroke' (G.reverse s) $ addStroke' s g

rotateList :: forall a. (Eq a) => a -> List a -> List a
rotateList el list =
  let unequal = (\a -> a /= el)
      front = takeWhile unequal list
      back = dropWhile unequal list
  in back <> front

getNextEdges :: Path -> Graph -> List G.Stroke
getNextEdges (stroke : _) g =
  case lookup (G.firstPoint stroke) g of
       Nothing -> Nil
       (Just strokes) ->
          G.reverse <$> (drop 1 $ rotateList stroke strokes) -- drop the stroke itself

getNextEdges _ _ = Nil

type Path = List G.Stroke
newtype Cycle = Cycle Path
data Traversal = Traversal (List Path) Graph

instance cycleEq :: Eq Cycle where
  eq (Cycle p1@(a : rest)) (Cycle p2) =
    (p1 == rotateList a p2)
    || (p1 == rotateList a (G.reverse <$> reverse p2))
  eq (Cycle Nil) (Cycle Nil) = true
  eq _ _ = false

instance cycleShow :: Show Cycle where
  show (Cycle edges) = show edges

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

findCycle :: G.Stroke -> Graph -> Maybe Path
findCycle stroke g =
  Lazy.head
  $ Lazy.filter G.isCycle
  $ unfoldr traverseRight (Traversal (singleton $ singleton stroke) g)

findCycles :: Graph -> List Cycle
findCycles g = nub cycles
  where
    cycles = do
      (Tuple pt strokes) <- toList g
      startStroke <- strokes
      case findCycle startStroke g of
           Just path -> pure (Cycle path)
           _ -> Nil
