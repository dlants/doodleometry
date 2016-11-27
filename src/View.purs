module App.View where

import Prelude
import App.Geometry (Point(..), Stroke(..), distance, getNearestPoint)
import App.Model (Action(..), State)
import Data.Array (fromFoldable)
import Data.List (List(..), concatMap)
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Pux.Html (Attribute, Html, circle, g, line, svg)
import Pux.Html.Attributes (cx, cy, height, r, stroke, strokeDasharray, width, x1, x2, y1, y2)
import Pux.Html.Events (onClick, onMouseMove)

drawLine :: Array (Attribute Action) -> Point -> Point -> Html Action
drawLine strokeStyle (Point p1) (Point p2) =
  line ([ x1 $ show p1.x
       , y1 $ show p1.y
       , x2 $ show p2.x
       , y2 $ show p2.y
       ] <> strokeStyle) []

drawStroke :: Array (Attribute Action) -> Stroke -> Html Action
drawStroke strokeStyle (Line (Tuple p1 p2)) = drawLine strokeStyle p1 p2

drawStrokes :: Array (Attribute Action) -> List Stroke -> Html Action
drawStrokes strokeStyle strokes =
  g [] $ fromFoldable $ (drawStroke strokeStyle) <$> strokes

drawCycles :: List (List Stroke) -> Html Action
drawCycles cycles =
  g [] $ fromFoldable $ (drawStrokes [stroke "red"]) <$> cycles

drawPoint :: Point -> Number -> Html Action
drawPoint (Point p) size =
  circle [ cx $ show p.x
         , cy $ show p.y
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

view :: State -> Html Action
view state =
  svg [ (onClick \{pageX, pageY} -> Click (Point {x: pageX, y: pageY}))
    , (onMouseMove \{pageX, pageY} -> Move (Point {x: pageX, y: pageY}))
    , width "100%"
    , height "100%"
    ]
    [ drawStrokes [stroke "black"] state.graph.edges
    , drawCycles state.graph.cycles
    , drawSnapPoint state.hover state.graph.vertices
    , drawCurrentStroke state.click state.hover
    ]
