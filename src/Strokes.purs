module App.Strokes where

import Prelude
import App.Counter as Counter
import App.NotFound as NotFound
import App.Routes (Route(Home, NotFound))
import Data.Array (fromFoldable)
import Data.List (List(..))
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Pux.Html (Html, div, h1, line, p, svg, text)
import Pux.Html.Attributes (stroke, x1, x2, y1, y2)
import Pux.Html.Events (MouseEvent, onClick)

data Action
  = Click Point

type Point =
  { x :: Number
  , y :: Number
  }

data Stroke
  = Line (Tuple Point Point)
  | Arc { center :: Point
        , radius :: Number
        , radStart :: Number
        , radEnd :: Number
        }

l1 = Line (Tuple {x: 0.0, y: 0.0} {x: 50.0, y: 50.0})
l2 = Line (Tuple {x: 1.0, y: 1.0} {x: 50.0, y: 0.0})

type State =
  { strokes :: List Stroke
  , click :: Maybe Point
  }

init :: State
init =
  { strokes: Cons l1 $ Cons l2 Nil
  , click: Nothing
  }


update :: Action -> State -> State
update (Click p) s@{click: Nothing} = s {click = Just p}
update (Click p2) s@{click: Just p1}
  = s { click = Nothing
      , strokes = Cons (Line (Tuple p1 p2)) s.strokes
      }

drawStroke :: Stroke -> Html Action
drawStroke (Line (Tuple p1 p2)) =
  line [ x1 $ show p1.x
       , y1 $ show p1.y
       , x2 $ show p2.x
       , y2 $ show p2.y
       , stroke "black"] []
drawStroke (Arc _) = line [] []

view :: State -> Html Action
view state =
  svg
    [ onClick (\{pageX, pageY} -> Click {x: pageX, y: pageY}) ]
    $ fromFoldable $ drawStroke <$> state.strokes
