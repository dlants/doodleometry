module App.View where

import Prelude
import App.ColorScheme (ColorScheme, toColor)
import App.Cycle (Cycle(..))
import App.Geometry (Point(..), Stroke(..), distance, firstPoint, getNearestPoint, ptX, ptY, secondPoint)
import App.Graph (edges)
import App.Model (Action(..), State, Tool(..))
import App.Tool.View (view) as ToolView
import Data.Array (fromFoldable)
import Data.Foldable (elem)
import Data.List (List, foldl, (:))
import Data.Map (Map, keys, toList, values)
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Math (abs, pi)
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
drawStroke strokeStyle s =
  let command = (mCommand $ firstPoint s) <> (dCommand s)
   in path (strokeStyle <> [d command]) []

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
   in [ d (foldl append (mCommand p) (dCommand <$> strokes))]

pathAttrs _ = []

mCommand (Point x y) = "M " <> (show $ x) <> " " <> (show $ y) <> " "

dCommand (Line _ (Point x2 y2)) = "L " <> show x2 <> " " <> show y2 <> " "
dCommand arc@(Arc (Point cx cy) r a s) =
  let endPoint = secondPoint arc
   in "A "
      <> show r <> " " -- rx
      <> show r -- ry
      <> " 0 " -- x-axis-rotation
      <> show (if abs s > pi then 1 else 0) -- large sweep flag (sweep > 180 degrees)
      <> " "
      <> show (if s > 0.0 then 1 else 0) -- sweep flag (sweep starts positive or negative)
      <> " "
      <> show (ptX endPoint) <> " "
      <> show (ptY endPoint)

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

drawCurrentStroke :: Maybe Stroke -> Html Action
drawCurrentStroke Nothing = g [] []
drawCurrentStroke (Just s) =
  drawStroke [stroke "black" , strokeDasharray "5 5", fill "transparent"] s

svgListeners :: Tool -> Array (Attribute Action)
svgListeners tool =
  if elem tool [LineTool, ArcTool] then
    [ (onClick \{pageX, pageY} -> Click (Point pageX pageY))
    , (onMouseMove \{pageX, pageY} -> Move (Point pageX pageY))
    ]
  else []


drawing :: State -> Html Action
drawing state =
  svg (svgListeners state.tool <> [width "800px", height "400px"])
    [ drawCycles state.tool state.cycles
    , drawStrokes [stroke "black"] (edges state.graph)
    , drawSnapPoint state.hover (keys state.graph)
    , drawCurrentStroke state.currentStroke
    ]

view :: State -> Html Action
view state =
  div [] [ (drawing state)
         , (ToolView.view state.tool)
         ]
