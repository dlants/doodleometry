module App.Background.View where

import Prelude
import App.Background (Background(..))
import App.ColorScheme (ColorScheme(..))
import App.Model (Action(..), State, Tool(..))
import Data.Array (range)
import Data.Int (toNumber)
import Pux.CSS (Color, absolute, backgroundColor, black, border, borderBox, boxShadow, boxSizing, display, fromHexString, gray, height, inlineBlock, insetBoxShadow, left, margin, padding, position, px, solid, style, top, white, width)
import Pux.Html (Html, div, g, line, svg, text)
import Pux.Html.Attributes (fill, opacity, stroke, x1, x2, y1, y2)
import Pux.Html.Events (onClick)

backgrounds :: Array Background
backgrounds =
  [ Square
  , Clear
  ]

fillBackground Clear _ _ =
  g [] []

fillBackground Square w h =
  let gridSize = 20
      strokeStyle = [stroke "black", fill "transparent", opacity "0.25"]
      horizontalLine y = line ([ x1 $ show 0
                               , x2 $ show w
                               , y1 $ show y
                               , y2 $ show y] <> strokeStyle) []
      verticalLine x = line ([ x1 $ show x
                             , x2 $ show x
                             , y1 $ show 0
                             , y2 $ show h] <> strokeStyle) []
   in g [] ( (horizontalLine <$> ((gridSize * _) <$> range 0 (h/gridSize)))
           <>(verticalLine <$> ((gridSize * _) <$> range 0 (w/gridSize)))
           )

drawBackground :: Background -> Background -> Html Action
drawBackground currentBg bg =
  let selected = currentBg == bg
      w = 60
      h = 60
   in svg [ (onClick \_ -> ChangeBackground bg)
          , style $ do
              backgroundColor white
              boxSizing borderBox
              if selected then border solid (2.0 # px) black
                          else border solid (1.0 # px) gray
              width $ (toNumber w) # px
              height $ (toNumber h) # px
              display inlineBlock
          ] [ fillBackground bg w h]

view :: Background -> Html Action
view currentBackground =
  div
    [ style $ do
        position absolute
        top (0.0 # px)
        left (100.0 # px)
    ] $ (drawBackground currentBackground) <$> backgrounds
