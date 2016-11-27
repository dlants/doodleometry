module App.View where

import Prelude
import App.Geometry (Point, distance, getNearestPoint)
import App.Model (Action(..), Stroke(..), State)
import Data.Array (fromFoldable)
import Data.List (List(..))
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Pux.Html (Html, line, circle, svg, g)
import Pux.Html.Attributes (x1, y1, x2, y2, cx, cy, r, stroke, width, height)
import Pux.Html.Events (onClick, onMouseMove)

drawLine :: Point -> Point -> Html Action
drawLine p1 p2 = line [ x1 $ show p1.x
       , y1 $ show p1.y
       , x2 $ show p2.x
       , y2 $ show p2.y
       , stroke "black"] []

drawStroke :: Stroke -> Html Action
drawStroke (Line (Tuple p1 p2)) = drawLine p1 p2

drawStrokes :: List Stroke -> Html Action
drawStrokes strokes =
  g [] $ fromFoldable $ drawStroke <$> strokes

drawPoint :: Point -> Number -> Html Action
drawPoint p size =
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
drawCurrentStroke (Just p1) p2 = drawLine p1 p2

view :: State -> Html Action
view state =
  svg [ (onClick \{pageX, pageY} -> Click {x: pageX, y: pageY})
    , (onMouseMove \{pageX, pageY} -> Move {x: pageX, y: pageY})
    , width "100%"
    , height "100%"
    ]
    [ drawStrokes state.strokes
    , drawSnapPoint state.hover state.poiList
    , drawCurrentStroke state.click state.hover
    ]
