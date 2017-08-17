module App.Helpers where

import Prelude
import Data.List (List, drop, elemIndex, take)
import Data.Maybe (Maybe(..))

rotateListN :: forall a. Int -> List a -> List a
rotateListN n list =
  (drop n list) <> (take n list)

-- rotate a list so that el is first
rotateList :: forall a. (Eq a) => a -> List a -> List a
rotateList el list =
  case elemIndex el list of
       Just n -> rotateListN n list
       _ -> list

-- rotate a list 1 past el
rotatePast :: forall a. (Eq a) => a -> List a -> List a
rotatePast el list =
  case elemIndex el list of
       Just n -> rotateListN (n+1) list
       _ -> list
