module App.Lib where

import Prelude

import Data.List (List(..), (:))
import Data.Maybe (Maybe(..))

first :: forall a b. (a -> Maybe b) -> List a -> Maybe b
first _ Nil = Nothing
first predicate (a : rest) =
  case predicate a of Just b -> Just b
                      Nothing -> first predicate rest

