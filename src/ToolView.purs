module App.Tool.View where

import Prelude
import App.ColorScheme (ColorScheme(..))
import App.Model (Action(..), State, Tool(..))
import Pux.CSS (Color, absolute, black, border, fromHexString, gray, left, position, px, solid, style, top)
import Pux.Html (Html, div, text)
import Pux.Html.Events (onClick)

toolBelt :: Array Tool
toolBelt =
  [ LineTool
  , ArcTool
  , ColorTool Red
  , ColorTool Green
  , ColorTool Blue
  , EraserTool
  ]

drawTool :: Tool -> Tool -> Html Action
drawTool selected tool =
  div
    [ (onClick  \_ -> Select tool)
    , style $ if selected == tool then border solid (2.0 # px) black
                                  else border solid (1.0 # px) gray
    ]
    [ case tool of
         LineTool -> div [] [text "Line"]
         ArcTool -> div [] [text "Arc"]
         ColorTool color -> div [] [text ("Color: " <> show color)]
         EraserTool -> div [] [text "Erase"]
    ]

view :: Tool -> Html Action
view tool =
  div
    [ style $ do
        position absolute
        top (0.0 # px)
        left (0.0 # px)
    ] $ (drawTool tool) <$> toolBelt
