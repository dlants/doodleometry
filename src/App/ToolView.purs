module App.Tool.View where

import Prelude

import App.ColorScheme (ColorScheme(..))
import App.Events (Event(..))
import App.State (Tool(EraserTool, ColorTool, ArcTool, LineTool))
import CSS.Border (border, solid)
import CSS.Color (black, gray)
import CSS.Display (absolute, position)
import CSS.Geometry (left, top)
import CSS.Size (px)
import Data.Foldable (for_)
import Pux.DOM.Events (onClick)
import Pux.DOM.HTML (HTML)
import Pux.DOM.HTML.Attributes (style)
import Text.Smolder.HTML (div)
import Text.Smolder.Markup (text, (!), (#!))

toolBelt :: Array Tool
toolBelt =
  [ LineTool
  , ArcTool
  , ColorTool Red
  , ColorTool Green
  , ColorTool Blue
  , EraserTool
  ]

drawTool :: Tool -> Tool -> HTML Event
drawTool selected tool =
  div #! onClick (const $ Select tool)
      ! style do
        if selected == tool then border solid (2.0 # px) black
                            else border solid (1.0 # px) gray
      $ div $ case tool of
                   LineTool -> text "Line"
                   ArcTool -> text "Arc"
                   ColorTool color -> text $ "Color: " <> show color
                   EraserTool -> text "Erase"

view :: Tool -> HTML Event
view tool =
  div ! style do
          position absolute
          top (0.0 # px)
          left (0.0 # px)
      $ do
        for_ toolBelt (drawTool tool)
