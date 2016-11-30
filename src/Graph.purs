module App.Graph where

import App.Geometry (Point)

import Prelude
import Data.List (List(..), elemIndex, filter, foldl, mapMaybe, singleton, (:))
import Data.Maybe (Maybe(..))

type Graph =
  { vertices :: List Point
  , edges :: List Edge
  , cycles :: List Path
  }

emptyGraph :: Graph
emptyGraph = { vertices: Nil
             , edges: Nil
             , cycles: Nil
             }

type Path = List Point

data Edge = Edge Point Point

instance edgeEq :: Eq Edge where
  eq (Edge p1 p2) (Edge q1 q2) =
    (p1 == q1 && p2 == q2)
    || (p1 == q2 && p2 == q1)

-- | important that there's a deterministic order since this is used as a key in
-- | a strmap
instance edgeShow :: Show Edge where
  show (Edge p1 p2) | p1 < p2 =
    "Edge(" <> show p1 <> " -- " <> show p2 <> ")"
  show (Edge p1 p2) =
    "Edge(" <> show p2 <> " -- " <> show p1 <> ")"

pushUnique :: forall a. (Eq a) => a -> List a -> List a
pushUnique a as =
  case elemIndex a as of
    Nothing -> a : as
    _ -> as

-- | find all edges touching point in graph
nextPoints :: Point -> List Edge -> List Point
nextPoints p ((Edge p1 p2) : edges) | p == p1 = p2 : nextPoints p edges
nextPoints p ((Edge p1 p2) : edges) | p == p2 = p1 : nextPoints p edges
nextPoints p ((Edge p1 p2) : edges) = nextPoints p edges
nextPoints _ _ = Nil

-- | v1 and v2 were not in a cycle before, so now there must be 0 or 1 path connecting them
findPath' :: Graph -> Point -> Path -> Maybe Path
findPath' _ _ Nil = Nothing
findPath' _ target path@(p : _) | p == target = Just path
findPath' g target path@(p : _) =
  case searchResults of
       a : _ -> Just a
       _ -> Nothing
  where
    ps = filter (\p -> elemIndex p path == Nothing) $ nextPoints p g.edges
    nextPaths = (\p -> p : path) <$> ps
    searchResults = mapMaybe (findPath' g target) nextPaths

findPath :: Graph -> Point -> Point -> Maybe Path
findPath g v1 v2 = findPath' g v1 (singleton v2)

cycleToEdges :: Path -> List Edge
cycleToEdges (p1 : p2 : rest) = (Edge p1 p2) : (cycleToEdges (p2 : rest))
cycleToEdges _ = Nil

-- | at this point we assume we've checked that there is no existing cycle containing these two points, and resolved
-- | any intersections
newCycle :: Edge -> Graph -> List Path
newCycle s@(Edge p1 p2) g =
  case findPath g p1 p2 of
     Just path -> path : g.cycles
     _ -> g.cycles

addEdge :: Edge -> Graph -> Graph
addEdge edge@(Edge p1 p2) g =
  { vertices: pushUnique p1 $ pushUnique p2 g.vertices
  , edges: pushUnique edge g.edges
  , cycles: newCycle edge g}
