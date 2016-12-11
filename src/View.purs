module App.View where

import Prelude
import App.ColorScheme (ColorScheme, toColor)
import App.Geometry (Point(..), Stroke(..), distance, getNearestPoint)
import App.Graph (Cycle(..), findCycles)
import App.Model (Action(..), State, Tool(..))
import App.Tool.View (view) as ToolView
import Data.Array (fromFoldable)
import Data.List (List, concat, foldl, (:))
import Data.Map (Map, keys, toList, values)
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Pux.CSS (absolute, bottom, left, position, px, right, style, toHexString, top)
import Pux.Html (Attribute, Html, circle, div, g, line, svg, path)
import Pux.Html.Attributes (cx, cy, d, fill, height, r, stroke, strokeDasharray, width, x1, x2, y1, y2)
import Pux.Html.Events (onClick, onMouseMove)

drawLine :: Array (Attribute Action) -> Point -> Point -> Html Action
drawLine strokeStyle (Point px1 py1) (Point px2 py2) =
  line ([ x1 $ show px1
       , y1 $ show py1
       , x2 $ show px2
       , y2 $ show py2
       ] <> strokeStyle) []

drawStroke :: Array (Attribute Action) -> Stroke -> Html Action
drawStroke strokeStyle (Line p1 p2) = drawLine strokeStyle p1 p2

drawStrokes :: Array (Attribute Action) -> List Stroke -> Html Action
drawStrokes strokeStyle strokes =
    g [] $ fromFoldable $ drawStroke strokeStyle <$> strokes

drawCycle :: Tuple Cycle ColorScheme -> Html Action
drawCycle (Tuple (Cycle strokes) colorScheme) =
  path (pathAttrs strokes <> [fill $ toHexString $ toColor colorScheme]) []

pathAttrs strokes@((Line (Point x0 y0) _): _) =
  [ d (foldl append ("M " <> show x0 <> " " <> show y0 <> " ") (dCommand <$> strokes))
  ]

pathAttrs _ = []

dCommand (Line _ (Point x2 y2)) = "L " <> show x2 <> " " <> show y2 <> " "

drawCycles :: Map Cycle ColorScheme -> Html Action
drawCycles cycles =
  g [] $ fromFoldable $ drawCycle <$> toList cycles

drawPoint :: Point -> Number -> Html Action
drawPoint (Point x y) size =
  circle [ cx $ show x
         , cy $ show y
         , r $ show size
         ] []

drawSnapPoint :: Point -> List Point -> Html Action
drawSnapPoint p ps =
  case getNearestPoint p ps of
    Nothing -> g [] []
    Just np -> drawPoint np (if distance p np < 20.0 then 3.0 else 2.0)

drawCurrentStroke :: Maybe Point -> Point -> Html Action
drawCurrentStroke Nothing _ = g [] []
drawCurrentStroke (Just p1) p2 =
  drawLine [ stroke "black"
           , strokeDasharray "5 5"
           ] p1 p2

svgListeners :: Tool -> Array (Attribute Action)
svgListeners LineTool =
  [ (onClick \{pageX, pageY} -> Click (Point pageX pageY))
  , (onMouseMove \{pageX, pageY} -> Move (Point pageX pageY))
  ]
svgListeners _ = []

drawing :: State -> Html Action
drawing state =
  svg (svgListeners state.tool <> [width "800px", height "400px"])
    [ drawStrokes [stroke "black"] (concat (values state.graph))
    , drawCycles state.cycles
    , drawSnapPoint state.hover (keys state.graph)
    , drawCurrentStroke state.click state.hover
    ]

view :: State -> Html Action
view state =
  div [] [ (drawing state)
         , (ToolView.view state.tool)
         ]
