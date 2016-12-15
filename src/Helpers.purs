module App.Helpers where

import Prelude
import Data.List (List, dropWhile, takeWhile)

-- rotate a list so that el is first
rotateList :: forall a. (Eq a) => a -> List a -> List a
rotateList el list =
  let unequal = (\a -> a /= el)
      front = takeWhile unequal list
      back = dropWhile unequal list
  in back <> front

