module App.BoundingBox where

import Prelude

data BoundingBox = BoundingBox {
  top:: Number,
  left:: Number,
  bottom:: Number,
  right:: Number
}

instance semigroupBoundingBox :: Semigroup BoundingBox where
  append (BoundingBox b1) (BoundingBox b2) =
    BoundingBox {
      top: max b1.top b2.top,
      left: min b1.left b2.left,
      bottom: min b1.bottom b2.bottom,
      right: max b1.right b2.right
    }

instance showBoundingBox :: Show BoundingBox where
  show (BoundingBox {top, left, bottom, right}) =
    "(BoundingBox {top: " <> show top <>
      ", left: " <> show left <>
      ", bottom: " <> show bottom <>
      ", right: " <> show right <> "})"

instance eqBoundingBox :: Eq BoundingBox where
  eq (BoundingBox b1) (BoundingBox b2) =
    eq b1.top b2.top &&
    eq b1.bottom b2.bottom &&
    eq b1.left b2.left &&
    eq b1.right b2.right

size :: BoundingBox -> Number
size (BoundingBox {top, left, bottom, right}) =
  (top - bottom) * (right - left)
