module App.Strokes where

import Prelude
import DOM.HTML.HTMLElement (offsetHeight)
import Data.Array (fromFoldable)
import Data.List (List(..), foldl, (:))
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Math (pow)
import Pux.Html (Html, circle, g, line, svg)
import Pux.Html.Attributes (cx, cy, r, stroke, x1, x2, y1, y2)
import Pux.Html.Events (onClick, onMouseMove)

data Action
  = Click Point
  | Move Point

type Point =
  { x :: Number
  , y :: Number
  }

data Stroke
  = Line (Tuple Point Point)
  {--| Arc { center :: Point
        , radius :: Number
        , radStart :: Number
        , radEnd :: Number
        }--}

type State =
  { strokes :: List Stroke
  , click :: Maybe Point
  , hover :: Point
  , poiList :: List Point
  }

distance :: Point -> Point -> Number
distance p1 p2 = pow (p1.x - p2.x) 2.0 + pow (p1.y - p2.y) 2.0

init :: State
init =
  { strokes: Nil
  , click: Nothing
  , hover: {x: 0.0, y: 0.0}
  , poiList: Nil
  }

updatePoiList :: Stroke -> List Point -> List Point
updatePoiList (Line (Tuple p1 p2)) poiList =
  p1 : p2 : poiList

update :: Action -> State -> State
update (Click p) s@{click: Nothing} = s {click = Just p}
update (Click p2) s@{click: Just p1}
  = s { click = Nothing
      , strokes = stroke : s.strokes
      , poiList = updatePoiList stroke s.poiList
      }
    where
      stroke = Line (Tuple p1 p2)
update (Move p) s = s {hover = p}

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

drawPoint :: Point -> Html Action
drawPoint p =
  circle [ cx $ show p.x
         , cy $ show p.y
         , r "3"
         ] []

getNearestPoint :: Point -> List Point -> Maybe Point
getNearestPoint _ Nil = Nothing
getNearestPoint p ps = foldl updateMax Nothing ps
  where
    updateMax :: Maybe Point -> Point -> Maybe Point
    updateMax Nothing p1 = Just p1
    updateMax (Just p1) p2 = if (distance p p1) < (distance p p2) then Just p1 else Just p2

getSnapPoint :: Point -> List Point -> Maybe Point
getSnapPoint p ps =
  case getNearestPoint p ps of
    Just np | distance p np < 10.0 -> Just np
    _ -> Nothing

drawSnapPoint :: Point -> List Point -> Html Action
drawSnapPoint p ps =
  case getNearestPoint p ps of
    Nothing -> g [] []
    Just np -> drawPoint np

drawCurrentStroke :: Maybe Point -> Point -> Html Action
drawCurrentStroke Nothing _ = g [] []
drawCurrentStroke (Just p1) p2 = drawLine p1 p2

view :: State -> Html Action
view state =
  svg [ (onClick \{pageX, pageY} -> Click {x: pageX, y: pageY})
    , (onMouseMove \{pageX, pageY} -> Move {x: pageX, y: pageY})]
    [ drawStrokes state.strokes
    , drawSnapPoint state.hover state.poiList
    , drawCurrentStroke state.click state.hover
    ]
