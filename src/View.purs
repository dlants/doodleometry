module App.View where

import Prelude
import App.Geometry (Point(..), distance, getNearestPoint)
import App.Graph (Edge(..), Path, cycleToEdges)
import App.Model (Action(..), State, Stroke(..), Embedding)
import Data.Array (fromFoldable)
import Data.List (List(..), concatMap)
import Data.Maybe (Maybe(..))
import Data.StrMap (lookup)
import Data.Tuple (Tuple(..))
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

drawEdge :: Array (Attribute Action) -> Embedding -> Edge -> Html Action
drawEdge strokeStyle embedding edge@(Edge p1 p2) =
  case lookup (show edge) embedding of
    Just Line -> drawLine strokeStyle p1 p2
    Nothing -> g [] []

drawStrokes :: Array (Attribute Action) -> List Edge -> Embedding -> Html Action
drawStrokes strokeStyle edges embedding =
    g [] $ fromFoldable $ drawEdge strokeStyle embedding <$> edges

drawCycles :: List Path -> Embedding -> Html Action
drawCycles cycles embedding =
  g [] $ fromFoldable $ drawCycle <$> cycles
    where
      drawCycle cycle = drawStrokes [stroke "red"] (cycleToEdges cycle) embedding

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
    [ drawStrokes [stroke "black"] state.graph.edges state.embedding
    , drawCycles state.graph.cycles state.embedding
    , drawSnapPoint state.hover state.graph.vertices
    , drawCurrentStroke state.click state.hover
    ]
