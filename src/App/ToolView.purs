module App.Tool.View where

import Prelude

import App.Events (Event(..))
import App.Geometry (Point(..))
import App.State (Tool(..))
import CSS (Color, backgroundColor, height, px, rgba, white, width)
import CSS.Border (border, solid)
import CSS.Color (black, gray, red, green, blue)
import CSS.Display (absolute, position)
import CSS.Geometry (left, top)
import Data.Foldable (for_)
import Pux.DOM.Events (onClick)
import Pux.DOM.HTML (HTML)
import Pux.DOM.HTML.Attributes (style)
import Text.Smolder.HTML (div)
import Text.Smolder.Markup (text, (!), (#!))

toolBelt :: Array Tool
toolBelt =
  [ SegmentTool
  , ArcTool
  , EraserTool {down: false, pt: (Point 0.0 0.0), size: 25.0}
  , SelectTool
  , ColorTool $ white
  , ColorTool $ rgba 199 68 64 1.0
  , ColorTool $ rgba 45 112 179 1.0
  , ColorTool $ rgba 250 126 25 1.0
  , ColorTool $ rgba 96 66 166 1.0
  , ColorTool $ rgba 56 140 70 1.0
  , ColorTool $ rgba 254 235 161 1.0
  , ColorTool $ rgba 245 168 166 1.0
  ]

colorTool :: Color -> HTML Event
colorTool color =
  div ! style do
    width $ 200.0 # px
    height $ 20.0 # px
    backgroundColor color
  $ pure unit

drawTool :: Tool -> Tool -> HTML Event
drawTool selected tool =
  div #! onClick (const $ Select tool)
      ! style do
        if selected == tool then border solid (2.0 # px) black
                            else border solid (1.0 # px) gray
      $ div $ case tool of
                   SegmentTool -> text "Segment"
                   ArcTool -> text "Circle"
                   ColorTool color -> colorTool color
                   EraserTool _ -> text "Erase"
                   SelectTool -> text "Select"

view :: Tool -> HTML Event
view tool =
  div ! style do
          position absolute
          top (0.0 # px)
          left (0.0 # px)
      $ do
        for_ toolBelt (drawTool tool)
