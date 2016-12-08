module App.Model where

import Prelude
import App.Geometry (Point(..), Stroke(..), distance, getNearestPoint)
import App.Graph (Graph, addStroke, emptyGraph)
import Data.Map (keys)
import Data.Maybe (Maybe(..))
import Pux.CSS (Color, blue, green, red)

data Action
  = Click Point
  | Move Point
  | Select Tool


data ColorScheme
  = Red
  | Green
  | Blue

instance eqColorScheme :: Eq ColorScheme where
  eq Red Red = true
  eq Green Green = true
  eq Blue Blue = true
  eq _ _ = false

instance showScheme :: Show ColorScheme where
  show Red = "red"
  show Green = "green"
  show Blue = "blue"

toColor :: ColorScheme -> Color
toColor Red = red
toColor Green = green
toColor Blue = blue

data Tool
  = LineTool
  | ColorTool ColorScheme

instance eqTool :: Eq Tool where
  eq LineTool LineTool = true
  eq (ColorTool c1) (ColorTool c2) = c1 == c2
  eq _ _ = false

type State =
  { graph :: Graph
  , click :: Maybe Point
  , hover :: Point
  , tool :: Tool
  }

init :: State
init =
  { graph: emptyGraph
  , click: Nothing
  , hover: Point 0.0 0.0
  , tool: LineTool
  }

updateForClick :: Point -> State -> State
updateForClick p s@{click: Nothing} = s {click = Just p}
updateForClick p2 s@{click: Just p1}
  = s { click = Nothing
      , graph = addStroke stroke s.graph
      }
  where
    stroke = Line p1 p2

snapToPoint :: Point -> State -> Point
snapToPoint p s =
  case maybeSnapPoint of
       Just snapPoint -> if (distance p snapPoint < 20.0) then snapPoint else p
       _ -> p
  where maybeSnapPoint = getNearestPoint p (keys s.graph)

update :: Action -> State -> State
update (Click p) s = updateForClick (snapToPoint p s) s
update (Move p) s = s {hover = p}
update (Select t) s = s {tool = t, click = Nothing}
