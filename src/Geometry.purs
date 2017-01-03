module App.Geometry where

import Prelude
import Data.List (List(..), foldl, foldr, head, last, nub, reverse, singleton, sort, zipWith, (:))
import Data.Map (Map, empty, insert)
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..), fst, snd)
import Math (Radians, atan2, pi, pow)

data Point = Point Number Number

instance ptEq :: Eq Point where
  eq (Point x1 y1) (Point x2 y2) = x1 == x2 && y1 == y2

instance ptShow :: Show Point where
  show (Point x y) = "(" <> show x <> ", " <> show y <> ")"

instance ptRing :: Ring Point where
  sub (Point x1 y1) (Point x2 y2) = Point (x1 - x2) (y1 - y2)

instance ptSemiring :: Semiring Point where
  add (Point x1 y1) (Point x2 y2) = Point (x1 + x2) (y1 + y2)
  mul (Point x1 y1) (Point x2 y2) = Point (x1 * x2) (y1 * y2)
  zero = Point 0.0 0.0
  one = Point 1.0 1.0

mapPt :: (Number -> Number) -> Point -> Point
mapPt f (Point x y) = Point (f x) (f y)

-- | arbitrarily compare xs first then ys
instance ptOrd :: Ord Point where
  compare (Point x1 y1) (Point x2 y2) | x1 == x2 = compare y1 y2
  compare (Point x1 _ ) (Point x2 _) = compare x1 x2

distance :: Point -> Point -> Number
distance (Point x1 y1) (Point x2 y2) = pow (x1 - x2) 2.0 + pow (y1 - y2) 2.0

crossProduct :: Point -> Point -> Number
crossProduct (Point x1 y1) (Point x2 y2) = x1 * y2 - x2 * y1

dotProduct :: Point -> Point -> Number
dotProduct (Point x1 y1) (Point x2 y2) = x1*x2 + y1*y2

getNearestPoint :: Point -> List Point -> Maybe Point
getNearestPoint _ Nil = Nothing
getNearestPoint p ps = foldl updateMax Nothing ps
  where
    updateMax :: Maybe Point -> Point -> Maybe Point
    updateMax Nothing p1 = Just p1
    updateMax (Just p1) p2 = if (distance p p1) < (distance p p2) then Just p1 else Just p2

data Stroke = Line Point Point

instance strokeEq :: Eq Stroke where
  eq (Line p1 p2) (Line q1 q2) = p1 == q1 && p2 == q2

unorderedEq :: Stroke -> Stroke -> Boolean
unorderedEq s1 s2 = s1 == s2 || (flipStroke s1) == s2

instance strokeShow :: Show Stroke where
  show (Line p1 p2) =
    "[" <> show p1 <> " --- " <> show p2 <> "]"

instance strokeOrd :: Ord Stroke where
  compare (Line p1 p2) (Line q1 q2) =
    case compare p1 q1 of
         EQ -> compare p2 q2
         ord -> ord

firstPoint :: Stroke -> Point
firstPoint (Line p1 _) = p1

secondPoint :: Stroke -> Point
secondPoint (Line _ p2) = p2

angle :: Stroke -> Radians
angle (Line (Point x1 y1) (Point x2 y2)) =
  atan2 (x2 - x1) (y2 - y1)

-- clockwise rotation -> positive angle
-- ->/ == negative
-- ->\ == positive
-- TODO: what if we do a 180? (currently should never happen due to simplify)
angleDiff :: Stroke -> Stroke -> Radians
angleDiff strokeFrom strokeTo =
  let diff = angle strokeFrom - angle strokeTo
   in if diff > pi then diff - pi
      else if diff < - pi then diff + pi
      else diff

findWrap :: Path -> Radians
findWrap Nil = 0.0
findWrap path@(_ : rest) =
  foldl (+) 0.0 angles
  where
    angles = zipWith angleDiff path rest

flipStroke :: Stroke -> Stroke
flipStroke (Line p1 p2) = Line p2 p1

type Path = List Stroke
type Intersections = Map Stroke (List Stroke)

reversePath :: Path -> Path
reversePath path =
  flipStroke <$> reverse path

swapEdge :: Path -> Stroke -> Path -> Path
swapEdge (c : cs) s ss
  | c == s = ss <> cs
  | c == (flipStroke s) = (reversePath ss) <> cs
  | otherwise = c : (swapEdge cs s ss)
swapEdge Nil _ _ = Nil

compareAngle :: Stroke -> Stroke -> Ordering
compareAngle s1 s2 =
  compare (angle s1) (angle s2)
  -- TODO - if a line and arc have the same angle, sort based on arc curvature

intersect :: Stroke -> Stroke -> (List Point)
intersect (Line p p') (Line q q') =
  let
    r = p' - p
    s = q' - q
    rxs = r `crossProduct` s
    qp = q - p
    qpxs = qp `crossProduct` s
    qpxr = qp `crossProduct` r
  in
    if rxs == 0.0 then
      if qpxr == 0.0 then -- segments are colinear
        let
          rr = (r `dotProduct` r)
          t0 = (qp `dotProduct` r) / rr
          t1 = ((qp + s) `dotProduct` r) / rr
          s1 = min t0 t1
          s2 = max t0 t1
        in Nil -- TODO: segments are colinear and overlapping
      else Nil -- segments are parallel and not colinear
    else
      let
        t = qpxs / rxs
        u = qpxr / rxs
      in
        if 0.0 <= t && t <= 1.0 && 0.0 <= u && u <= 1.0 then singleton (p + ((*) t) `mapPt` r)
                                                        else Nil

intersectMultiple :: Stroke -> List Stroke -> Intersections
intersectMultiple stroke strokes =
  let
    insertIntersection edge result@(Tuple allPoints intersections) =
      case intersect stroke edge of
           Nil -> result
           newPoints -> Tuple (nub $ newPoints <> allPoints) (insert edge (split edge newPoints) intersections)
    results = foldr insertIntersection (Tuple Nil empty) strokes
    allPoints = fst results
    intersections = snd results
  in
    insert stroke (split stroke allPoints) intersections

-- given a stroke and a list of intersections, return a list of strokes
-- TODO: currently relies on sorting points since the result will be correct for lines
-- ... need to sort 'along the arc' for arcs
split :: Stroke -> List Point -> List Stroke
split (Line p1 p2) points =
  case head lines of -- ensure we still go from p1 to p2
       Just (Line p _) | p == p1 -> lines
       _ -> reversePath lines
  where
    sortedPoints = sort $ nub $ p1 : p2 : points
    makeLines (p : q : rest) = (Line p q) : (makeLines (q : rest))
    makeLines _ = Nil
    lines = makeLines sortedPoints
