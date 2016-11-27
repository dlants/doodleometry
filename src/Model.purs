module App.Model where

import Prelude
import App.Geometry (Point, distance, getNearestPoint)
import Data.List (List(..), (:))
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))

data Action
  = Click Point
  | Move Point

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

updateForClick :: Point -> State -> State
updateForClick p s@{click: Nothing} = s {click = Just p}
updateForClick p2 s@{click: Just p1}
  = s { click = Nothing
      , strokes = stroke : s.strokes
      , poiList = updatePoiList stroke s.poiList
      }
    where
      stroke = Line (Tuple p1 p2)

snapToPoint :: Point -> State -> Point
snapToPoint p s =
  case maybeSnapPoint of
       Just snapPoint -> if (distance p snapPoint < 20.0) then snapPoint else p
       _ -> p
  where maybeSnapPoint = getNearestPoint p s.poiList

update :: Action -> State -> State
update (Click p) s = updateForClick (snapToPoint p s) s
update (Move p) s = s {hover = p}
