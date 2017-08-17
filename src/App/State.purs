module App.State where

import Prelude
import App.Background (Background(..))
import App.ColorScheme (ColorScheme)
import App.Cycle (Cycle)
import App.Geometry (Point, Stroke)
import App.Graph (Graph, emptyGraph)
import Data.Map (Map, empty)
import Data.Maybe (Maybe(..))

data Tool
  = LineTool
  | ArcTool
  | ColorTool ColorScheme
  | EraserTool

derive instance eqTool :: Eq Tool

type State =
  { graph :: Graph
  , cycles :: Map Cycle ColorScheme
  , click :: Maybe Point
  , hover :: Maybe Point
  , snapPoint :: Maybe Point
  , lastEraserPoint :: Maybe Point
  , currentStroke :: Maybe Stroke
  , tool :: Tool
  , windowWidth :: Int
  , windowHeight :: Int
  , background :: Background
  }

-- the actual width and height will be overwritten in index.js
init :: State
init =
  { graph: emptyGraph
  , cycles: empty
  , click: Nothing
  , hover: Nothing
  , snapPoint: Nothing
  , lastEraserPoint: Nothing
  , currentStroke: Nothing
  , tool: LineTool
  , windowWidth: 1000
  , windowHeight: 1000
  , background: Square
  }
