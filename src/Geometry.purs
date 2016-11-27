module App.Geometry where

import Prelude
import Data.List (List(..), elemIndex, filter, foldl, mapMaybe, (:))
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Math (pow)

newtype Point = Point
  { x :: Number
  , y :: Number
  }

instance ptEq :: Eq Point where
  eq (Point p1) (Point p2) = p1.x == p2.x && p1.y == p2.y

distance :: Point -> Point -> Number
distance (Point p1) (Point p2) = pow (p1.x - p2.x) 2.0 + pow (p1.y - p2.y) 2.0

getNearestPoint :: Point -> List Point -> Maybe Point
getNearestPoint _ Nil = Nothing
getNearestPoint p ps = foldl updateMax Nothing ps
  where
    updateMax :: Maybe Point -> Point -> Maybe Point
    updateMax Nothing p1 = Just p1
    updateMax (Just p1) p2 = if (distance p p1) < (distance p p2) then Just p1 else Just p2

data Stroke
  = Line (Tuple Point Point)
  {--| Arc { center :: Point
        , radius :: Number
        , radStart :: Number
        , radEnd :: Number
        }--}

instance strokeEq :: Eq Stroke where
  eq (Line (Tuple p1 p2)) (Line (Tuple q1 q2)) =
    (p1 == q1 && p2 == q2)
    || (p1 == q2 && p2 == q1)

type Cycle = List Stroke

type Graph =
  { vertices :: List Point
  , edges :: List Stroke
  , cycles :: List Cycle
  }

emptyGraph :: Graph
emptyGraph = { vertices: Nil
             , edges: Nil
             , cycles: Nil
             }

type Path =
  { edges :: List Stroke
  , v1 :: Point
  , v2 :: Point
  }

edgeIsTouching :: Point -> Stroke -> Boolean
edgeIsTouching v (Line (Tuple v1 v2)) = v1 == v || v2 == v

-- | find all edges touching point in graph
outboundEdges :: Point -> (List Stroke -> List Stroke)
outboundEdges v = filter (edgeIsTouching v)

otherPoint :: Point -> Stroke -> Point
otherPoint v (Line (Tuple v1 v2)) =
  if v == v1 then v2 else v1

notInPath path edge = elemIndex edge path.edges == Nothing

extendPathByEdge :: Path -> Stroke -> Path
extendPathByEdge path edge =
  path { edges = (edge : path.edges)
       , v2 = otherPoint path.v1 edge
       }

-- | v1 and v2 were not in a cycle before, so now there must be 0 or 1 path connecting them
findPath' :: Graph -> Point -> Path -> Maybe Path
findPath' g target path@{edges, v1, v2} =
  if v2 == target then Just path
  else let
      nextEdges = filter (notInPath path) $ outboundEdges v2 g.edges
      nextPaths = extendPathByEdge path <$> nextEdges
      searchResults = mapMaybe (findPath' g target) nextPaths
    in
      case searchResults of
           (Cons a _) -> Just a
           _ -> Nothing

findPath :: Graph -> Point -> Point -> Maybe Path
findPath g v1 v2 = findPath' g v1 {edges: Nil, v1, v2}

pushUnique :: forall a. (Eq a) => a -> List a -> List a
pushUnique a as =
  case elemIndex a as of
    Nothing -> a : as
    _ -> as

-- | at this point we assume we've checked that there is no existing cycle containing these two points, and resolved
-- | any intersections
newCycle s@(Line (Tuple p1 p2)) g =
  case findPath g p1 p2 of
     Just {edges} -> (s : edges) : g.cycles
     _ -> g.cycles

pushStroke :: Point -> Point -> Graph -> Graph
pushStroke p1 p2 g =
  { vertices: pushUnique p1 $ pushUnique p2 g.vertices
  , edges: pushUnique stroke g.edges
  , cycles: newCycle stroke g}
  where
    stroke = Line (Tuple p1 p2)
