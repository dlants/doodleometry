module App.Graph where

import Prelude
import App.ColorScheme (ColorScheme(..))
import App.Geometry (orderedEq)
import App.Geometry (Point, Stroke(..), firstPoint, isCycle, reverse) as G
import Control.MonadZero (guard)
import Data.List (List(..), any, drop, dropWhile, elem, filter, insert, mapMaybe, nub, reverse, singleton, sort, takeWhile, (:))
import Data.List.Lazy (filter, head) as Lazy
import Data.Map (insert) as Map
import Data.Map (Map, alter, empty, lookup, pop, toList)
import Data.Maybe (Maybe(..))
import Data.Ord (Ordering(..))
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

-- rotate a list so that el is first
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

instance cycleOrd :: Ord Cycle where
  compare (Cycle p1) (Cycle p2) =
    compare (sort p1) (sort p2)

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

findCycle :: Graph -> G.Stroke -> Maybe Cycle
findCycle g stroke =
  Cycle
  <$> (
    Lazy.head
    $ Lazy.filter G.isCycle
    $ unfoldr traverseRight (Traversal (singleton $ singleton stroke) g)
  )

findCycles :: Graph -> List Cycle
findCycles g = nub cycles
  where
    cycles = do
      (Tuple pt strokes) <- toList g
      startStroke <- strokes
      case findCycle g startStroke of
           Just cycle -> pure cycle
           _ -> Nil

cut :: Cycle -> G.Stroke -> Path
cut (Cycle edges) edge =
  let
    hasEdge = any (orderedEq edge) edges
  in
    if hasEdge then drop 1 $ rotateList edge edges
               else drop 1 $ rotateList edge (reverse $ G.reverse <$> edges)

-- TODO: make this work with multiple shared edges
-- for now, assumes a single shared edge
joinCycles :: Cycle -> Cycle -> G.Stroke -> Cycle
joinCycles c1 c2 stroke =
  let
    path1 = cut c1 stroke -- if stroke is p1p2, path1 takes from p2 to p1
    path2 = cut c2 (G.reverse stroke) -- if stroke is p1p2, path2 takes from p1 to p2
  in
    Cycle (path1 <> path2)

-- find new cycles in both directions, nub them
-- if there is 1 cycle, we haven't split any existing cycles - just add it
-- if there are 2 cycles, we split an existing cycle. joinCycles the 2 new cycles,
-- find the existing cycle and remove it.
updateCycles :: Map Cycle ColorScheme -> Graph -> G.Stroke -> Map Cycle ColorScheme
updateCycles cycles g stroke =
  let
    newCycles = nub $ mapMaybe (findCycle g) $ stroke : (G.reverse stroke) : Nil
  in
    case newCycles of
         Nil -> cycles -- no new cycles, just return old cycles
         (c : Nil) -> Map.insert c White cycles -- one new cycle - push it on the front
         (c1 : c2 : _) -> -- two new cycles. They must have split an existing cycle
            let
              joined = joinCycles c1 c2 stroke
            in
              case pop joined cycles of
                   Just (Tuple newColor newCycles) ->
                     Map.insert c1 newColor $ Map.insert c2 newColor $ newCycles
                   _ -> Map.insert c1 White $ Map.insert c2 White $ cycles
