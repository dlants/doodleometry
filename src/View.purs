module App.View where

import Prelude
import App.Geometry (Point(..), Stroke(..), distance, getNearestPoint)
import App.Graph (Cycle(..), findCycles)
import App.Model (Action(..), State)
import Data.Array (fromFoldable)
import Data.List (List, concat)
import Data.Map (keys, values)
import Data.Maybe (Maybe(..))
import Pux.Html (Attribute, Html, circle, g, line, svg)
import Pux.Html.Attributes (cx, cy, height, r, stroke, strokeDasharray, width, x1, x2, y1, y2)
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

drawCycles :: List Cycle -> Html Action
drawCycles cycles =
  g [] $ fromFoldable $ drawCycle <$> cycles
  where
    drawCycle (Cycle strokes) = drawStrokes [stroke "red"] strokes

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

view :: State -> Html Action
view state =
  svg [ (onClick \{pageX, pageY} -> Click (Point pageX pageY))
    , (onMouseMove \{pageX, pageY} -> Move (Point pageX pageY))
    , width "100%"
    , height "100%"
    ]
    [ drawStrokes [stroke "black"] (concat (values state.graph))
    , drawCycles (findCycles state.graph)
    , drawSnapPoint state.hover (keys state.graph)
    , drawCurrentStroke state.click state.hover
    ]
