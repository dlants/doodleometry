module App.View where

import Prelude
import App.ColorScheme (ColorScheme, toColor)
import App.Cycle (Cycle(..))
import App.Geometry (Point(..), Stroke(..), distance, firstPoint, getNearestPoint, ptX, ptY)
import App.Graph (edges)
import App.Model (Action(..), State, Tool(..))
import App.Tool.View (view) as ToolView
import Data.Array (fromFoldable)
import Data.List (List, foldl, (:))
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
drawStroke strokeStyle (Arc c r a s) = g [] []

drawStrokes :: Array (Attribute Action) -> List Stroke -> Html Action
drawStrokes strokeStyle strokes =
    g [] $ fromFoldable $ drawStroke strokeStyle <$> strokes

drawCycle :: Tool -> Tuple Cycle ColorScheme -> Html Action
drawCycle tool (Tuple cycle@(Cycle strokes) colorScheme) =
  let
    listeners = case tool of
                     ColorTool newColorScheme ->
                       [(onClick \_ -> Color cycle newColorScheme)]
                     _ -> []
  in
    path (pathAttrs strokes <> [fill $ toHexString $ toColor colorScheme] <> listeners) []

pathAttrs strokes@(s1 : _) =
  let p = firstPoint s1
   in [ d (foldl append ("M " <> (show $ ptX p) <> " " <> (show $ ptY p) <> " ") (dCommand <$> strokes))]

pathAttrs _ = []

dCommand (Line _ (Point x2 y2)) = "L " <> show x2 <> " " <> show y2 <> " "
dCommand (Arc _ _ _ _) = ""

drawCycles :: Tool -> Map Cycle ColorScheme -> Html Action
drawCycles tool cycles =
  g [] $ fromFoldable $ drawCycle tool <$> toList cycles

drawPoint :: Point -> Number -> Html Action
drawPoint (Point x y) size =
  circle [ cx $ show x
         , cy $ show y
         , r $ show size
         ] []

drawSnapPoint :: Maybe Point -> List Point -> Html Action
drawSnapPoint Nothing _ = g [] []
drawSnapPoint (Just p) ps =
  case getNearestPoint p ps of
    Nothing -> g [] []
    Just np -> drawPoint np (if distance p np < 20.0 then 3.0 else 2.0)

drawCurrentStroke :: Maybe Point -> Maybe Point -> Html Action
drawCurrentStroke (Just p1) (Just p2) =
  drawLine [ stroke "black"
           , strokeDasharray "5 5"
           ] p1 p2
drawCurrentStroke _ _ = g [] []

svgListeners :: Tool -> Array (Attribute Action)
svgListeners LineTool =
  [ (onClick \{pageX, pageY} -> Click (Point pageX pageY))
  , (onMouseMove \{pageX, pageY} -> Move (Point pageX pageY))
  ]
svgListeners _ = []

drawing :: State -> Html Action
drawing state =
  svg (svgListeners state.tool <> [width "800px", height "400px"])
    [ drawCycles state.tool state.cycles
    , drawStrokes [stroke "black"] (edges state.graph)
    , drawSnapPoint state.hover (keys state.graph)
    , drawCurrentStroke state.click state.hover
    ]

view :: State -> Html Action
view state =
  div [] [ (drawing state)
         , (ToolView.view state.tool)
         ]
