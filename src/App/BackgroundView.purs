module App.Background.View where

import Prelude

import App.Background (Background(..))
import App.Events (Event(..))
import CSS (absolute, left, position, top)
import CSS.Background (backgroundColor)
import CSS.Border (border, solid)
import CSS.Box (borderBox, boxSizing)
import CSS.Color (black, gray, white)
import CSS.Display (display, inlineBlock)
import CSS.Geometry (height, width)
import CSS.Size (px)
import Data.Array (range)
import Data.Foldable (for_)
import Data.Int (toNumber)
import Data.Monoid (mempty)
import Pux.DOM.Events (onClick)
import Pux.DOM.HTML (HTML)
import Pux.DOM.HTML.Attributes (style)
import Text.Smolder.HTML (div)
import Text.Smolder.Markup (attribute, (!), (#!))
import Text.Smolder.SVG (g, line, svg)
import Text.Smolder.SVG.Attributes (fill, fillOpacity, stroke, x1, x2, y1, y2)

backgrounds :: Array Background
backgrounds =
  [ Square
  , Clear
  ]

fillBackground :: Background -> Int -> Int -> HTML Event
fillBackground Square w h =
  let gridSize = 20
      horizontalLine y =
        line ! (x1 $ show 0)
             ! (x2 $ show w)
             ! (y1 $ show y)
             ! (y2 $ show y)
             ! stroke "black"
             ! fill "transparent"
             ! attribute "strokeOpacity" "0.25"

      verticalLine x =
        line ! (x1 $ show x)
             ! (x2 $ show x)
             ! (y1 $ show 0)
             ! (y2 $ show h)
             ! stroke "black"
             ! fill "transparent"
             ! attribute "strokeOpacity" "0.25"

   in g $ do
     for_ (range 0 (h/gridSize)) \n -> horizontalLine $ gridSize * n
     for_ (range 0 (w/gridSize)) \n -> verticalLine $ gridSize * n
fillBackground _ _ _ = pure unit

drawBgToggle :: Background -> Background -> HTML Event
drawBgToggle currentBg bg =
  let selected = currentBg == bg
      w = 60
      h = 60
   in svg  ! style do
            backgroundColor white
            boxSizing borderBox
            if selected then border solid (2.0 # px) black
                        else border solid (1.0 # px) gray
            width $ (toNumber w) # px
            height $ (toNumber h) # px
            display inlineBlock
          #! onClick (const $ ChangeBackground bg)
          $ fillBackground bg w h

view :: Background -> HTML Event
view currentBackground =
  div ! style do
          position absolute
          top (0.0 # px)
          left (200.0 # px)
      $ do
        drawBgToggle currentBackground Clear
        drawBgToggle currentBackground Square
