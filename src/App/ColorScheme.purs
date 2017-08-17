module App.ColorScheme where

import Prelude
import CSS (Color, blue, green, red, white)

data ColorScheme
  = Red
  | Green
  | Blue
  | White

instance eqColorScheme :: Eq ColorScheme where
  eq Red Red = true
  eq Green Green = true
  eq Blue Blue = true
  eq White White = true
  eq _ _ = false

instance showScheme :: Show ColorScheme where
  show Red = "red"
  show Green = "green"
  show Blue = "blue"
  show White = "white"

toColor :: ColorScheme -> Color
toColor Red = red
toColor Green = green
toColor Blue = blue
toColor White = white

