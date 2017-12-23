module App.State where

import Prelude

import App.Background (Background(..))
import App.Cycle (Cycle)
import App.Geometry (Point, Stroke)
import App.Graph (Graph, emptyGraph)
import CSS.Color (Color)
import Data.List (List(..))
import Data.Map (Map, empty)
import Data.Maybe (Maybe(..))

data Tool
  = LineTool
  | ArcTool
  | ColorTool Color
  | EraserTool {down :: Boolean, pt :: Point, size :: Number}
  | SelectTool

derive instance eqTool :: Eq Tool

type Drawing =
  { graph :: Graph
  , cycles :: Map Cycle Color
  , snapPoints :: List Point
  }

type State =
  { drawing :: Drawing
  , undos :: List Drawing
  , redos :: List Drawing
  , click :: Maybe Point
  , hover :: Maybe Point
  , snapPoint :: Maybe Point
  , currentStroke :: Maybe Stroke
  , tool :: Tool
  , windowWidth :: Int
  , windowHeight :: Int
  , background :: Background
  , selection :: List Cycle
  }

-- the actual width and height will be overwritten in index.js
init :: State
init =
  { drawing:
    { graph: emptyGraph
    , cycles: empty
    , snapPoints: Nil
    }
  , undos: Nil
  , redos: Nil
  , click: Nothing
  , hover: Nothing
  , snapPoint: Nothing
  , currentStroke: Nothing
  , tool: LineTool
  , windowWidth: 1000
  , windowHeight: 1000
  , background: Square
  , selection: Nil
  }
