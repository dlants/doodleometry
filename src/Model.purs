module App.Model where

import Prelude
import App.Geometry (Point(..), distance, getNearestPoint)
import App.Graph (Edge(..), Graph, addEdge, emptyGraph)
import Data.List (List(..), (:))
import Data.Maybe (Maybe(..))
import Data.StrMap (StrMap, empty, insert)

data Action
  = Click Point
  | Move Point

data Stroke
  = Line
  {--| Arc { center :: Point
        , radius :: Number
        , radStart :: Number
        , radEnd :: Number
        }--}

type Embedding = StrMap Stroke

type State =
  { graph :: Graph
  , embedding :: Embedding
  , click :: Maybe Point
  , hover :: Point
  }

init :: State
init =
  { graph: emptyGraph
  , embedding: empty
  , click: Nothing
  , hover: Point 0.0 0.0
  }

updateForClick :: Point -> State -> State
updateForClick p s@{click: Nothing} = s {click = Just p}
updateForClick p2 s@{click: Just p1}
  = s { click = Nothing
      , graph = addEdge edge s.graph
      , embedding = insert (show edge) Line s.embedding
      }
  where
    edge = Edge p1 p2

snapToPoint :: Point -> State -> Point
snapToPoint p s =
  case maybeSnapPoint of
       Just snapPoint -> if (distance p snapPoint < 20.0) then snapPoint else p
       _ -> p
  where maybeSnapPoint = getNearestPoint p s.graph.vertices

update :: Action -> State -> State
update (Click p) s = updateForClick (snapToPoint p s) s
update (Move p) s = s {hover = p}
