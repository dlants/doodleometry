module App.Background where

import Prelude

data Background
  = Clear
  | Square

derive instance eqBg :: Eq Background
