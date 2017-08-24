module App.Geometry where

import Prelude
import Data.Function (on)
import Data.Function.Uncurried (Fn2, runFn2)
import Data.List (List(..), concatMap, filter, foldl, head, length, mapMaybe, nub, reverse, singleton, snoc, sort, sortBy, zipWith, (:))
import Data.Map (Map, empty, insert, lookup, values)
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..), snd)
import Math (Radians, abs, atan2, cos, pi, pow, sin, sqrt, trunc)

data Point = Point Number Number

{--
approxEq :: Number -> Number -> Boolean
approxEq a b =
  abs (a - b) < 1.0
--}

instance ptOrd :: Ord Point where
  compare (Point x1 y1) (Point x2 y2) = compare x1 x2 <> compare y1 y2

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

ptX :: Point -> Number
ptX (Point x _) = x

ptY :: Point -> Number
ptY (Point _ y) = y

scalePt :: Point -> Number -> Point
scalePt (Point x y) c = Point (c * x) (c * y)

normalize :: Point -> Point
normalize p@(Point x y) =
  let m = runFn2 distance (Point 0.0 0.0) p
   in Point (x/m) (y/m)

rotate :: Point -> Radians -> Point
rotate (Point x y) theta =
  Point (x * (cos theta) - y * (sin theta)) (x * (sin theta) + y * (cos theta))

foreign import ptAngle :: Fn2 Point Point Radians
foreign import distance :: Fn2 Point Point Number

getAngleDiff :: Radians -> Radians -> Boolean -> Radians
getAngleDiff a1 a2 ccw =
  case a2 - a1 of diff | diff < 0.0 && ccw -> diff + 2.0 * pi
                       | diff > 0.0 && (not ccw) -> diff - 2.0 * pi
                       | otherwise -> diff

ptSweep :: Point -> Point -> Point -> Boolean -> Radians
ptSweep c p q ccw =
  getAngleDiff (runFn2 ptAngle c p) (runFn2 ptAngle c q) ccw

mapPt :: (Number -> Number) -> Point -> Point
mapPt f (Point x y) = Point (f x) (f y)


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
    updateMax (Just p1) p2 = if (runFn2 distance p p1) < (runFn2 distance p p2) then Just p1 else Just p2

data Stroke = Line Point Point
            | Arc Point Point Point Boolean

instance strokeEq :: Eq Stroke where
  eq (Line p1 p2) (Line q1 q2) = p1 == q1 && p2 == q2
  eq (Arc c p q ccw) (Arc c' p' q' ccw') = c == c' && p == p' && q == q' && ccw == ccw'
  eq _ _ = false

unorderedEq :: Stroke -> Stroke -> Boolean
unorderedEq s1 s2 = s1 == s2 || (flipStroke s1) == s2

instance strokeShow :: Show Stroke where
  show (Line p1 p2) =
    "[" <> show p1 <> " --- " <> show p2 <> "]"

  show (Arc c p q ccw) =
    "( center: " <> show c <> ", p: " <> show p <> ", q: " <> show q <> ", ccw: " <> show ccw <> ")"

compareMap :: forall a. (Ord a) => List (Stroke -> a) -> Stroke -> Stroke -> Ordering
compareMap (fn : rest) s1 s2 = case compare (fn s1) (fn s2) of
                                    EQ -> compareMap rest s1 s2
                                    ord -> ord
compareMap Nil _ _ = EQ

instance strokeOrd :: Ord Stroke where
  compare =  (compare `on` firstPoint)
          <> (compare `on` (outboundAngle >>> truncTo5))
          <> (compare `on` curvature)
          <> (compare `on` strokeLength)

firstPoint :: Stroke -> Point
firstPoint (Line p1 _) = p1
firstPoint (Arc _ p _ _) = p

secondPoint :: Stroke -> Point
secondPoint (Line _ p2) = p2
secondPoint (Arc _ _ q _) = q

sweep :: Stroke -> Radians
sweep (Line _ _) = 0.0
sweep (Arc c p q ccw) =
  case ptSweep c p q ccw of s | s == 0.0 -> if ccw then 2.0 * pi else -2.0 * pi
                              | otherwise -> s

truncTo5 :: Number -> Number
truncTo5 num = (trunc (num * 100000.0)) / 100000.0

outboundAngle :: Stroke -> Radians
outboundAngle (Line (Point x1 y1) (Point x2 y2)) =
  atan2 (y2 - y1) (x2 - x1)

outboundAngle (Arc c p _ ccw) =
  let a = runFn2 ptAngle c p
   in atan2Radians $ a + if ccw then (pi / 2.0) else (- pi / 2.0)

inboundAngle :: Stroke -> Radians
inboundAngle s@(Line _ _) = outboundAngle s
inboundAngle (Arc c _ q ccw) =
  let a = runFn2 ptAngle c q
   in a + if ccw then (pi / 2.0) else (- pi / 2.0)

-- communicates how aggressively the curve turns. Lines don't turn at all, so have curvature 0
-- arcs, when sorted by curvature will go from small-radii counterclockwise arcs, to large-radii ones (with a limit
-- of a line at 0), then large-radii clockwise arcs (which are negative) and finally small-radii counterclockwise
-- arcs (which are the most negative).
curvature :: Stroke -> Number
curvature (Line _ _) = 0.0
curvature (Arc c p _ ccw) =
  let r = (runFn2 distance c p)
   in if ccw then 1.0 / r else - 1.0 / r

foreign import strokeLength :: Stroke -> Number
-- strokeLength :: Stroke -> Number
-- strokeLength (Line (Point x1 y1) (Point x2 y2)) = (pow (x2 - x1) 2.0) + (pow (y2 - y1) 2.0)
-- strokeLength a@(Arc c p _ _) = (sweep a) * (runFn2 distance c p)

-- clockwise rotation -> positive angle
-- ->/ == negative
-- ->\ == positive
angleDiff :: Stroke -> Stroke -> Radians
angleDiff strokeIn strokeOut =
  let a1 = outboundAngle strokeOut
      a2 = inboundAngle strokeIn
      diff = atan2Radians (a1 - a2)
   in if (abs $ truncTo5 diff) == truncTo5 pi then case compare strokeOut (flipStroke strokeIn) of EQ -> -pi
                                                                                                   LT -> pi
                                                                                                   GT -> -pi
                                       else diff

findWrap :: Path -> Radians
findWrap Nil = 0.0
findWrap path@(s1 : rest) =
  foldl (+) 0.0 (angles <> (sweep <$> path))
  where
    -- include the angle from the last to the first stroke as well
    angles = zipWith angleDiff path (snoc rest s1)

flipStroke :: Stroke -> Stroke
flipStroke (Line p1 p2) = Line p2 p1
flipStroke (Arc c p q ccw) = (Arc c q p (not ccw))

positiveRadians :: Radians -> Radians
positiveRadians a
  | a < 0.0 = a + 2.0 * pi
  | a > 2.0 * pi = a - 2.0 * pi
  | otherwise = a

atan2Radians :: Radians -> Radians
atan2Radians a
  | a > pi = a - 2.0 * pi
  | a < -pi = a + 2.0 * pi
  | otherwise = a

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

-- is the potential solution within the bounds of this stroke?
withinBounds :: Stroke -> Point -> Boolean
withinBounds (Line (Point x1 y1) (Point x2 y2)) (Point x y) =
  let between a a1 a2 = (a1 <= a && a <= a2) || (a1 >= a && a >= a2)
   in (between x x1 x2) && (between y y1 y2)

withinBounds arc@(Arc c p _ ccw) sol =
  let adiff = getAngleDiff (runFn2 ptAngle c p) (runFn2 ptAngle c sol) ccw
   in (abs adiff) <= (abs (sweep arc))

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

intersect line@(Line (Point lx1 ly1) (Point lx2 ly2)) arc@(Arc c@(Point cx cy) p q ccw) =
  let check sol = (withinBounds line sol) && (withinBounds arc sol)
   in if lx2 == lx1 then
     let r = runFn2 distance c p
         disc = r * r - (pow (cx - lx1) 2.0)

         getSolution d =
           let y = cy + d
               solution = Point lx1 y
            in if check solution then Just solution
                                 else Nothing

      in case disc of
              _ | disc < 0.0 -> Nil
                | disc == 0.0 -> mapMaybe getSolution (0.0 : Nil)
                | otherwise -> mapMaybe getSolution (sqrt disc : -(sqrt disc) : Nil)

    else
      let m = (ly2 - ly1) / (lx2 - lx1)
          b = ly1 - (m * lx1)
          cA = m * m + 1.0
          cB = 2.0 * (m * b - m * cy - cx)
          r = runFn2 distance c p
          cC = cy * cy - r * r + cx * cx - 2.0 * b * cy + b * b
          disc = cB*cB - 4.0 * cA * cC

          -- plug in a value for the determinant
          getSolution d =
            let x = (-cB + d) / (2.0 * cA)
                y = m * x + b
                solution = Point x y
             in if check solution then Just solution
                                  else Nothing

       in case disc of
               _ | disc < 0.0 -> Nil
                 | disc == 0.0 -> mapMaybe getSolution (0.0 : Nil)
                 | otherwise -> mapMaybe getSolution (sqrt disc : -(sqrt disc) : Nil)

intersect arc1@(Arc c1 p1 q1 ccw1) arc2@(Arc c2 p2 q2 ccw2) =
  let r1 = runFn2 distance c1 p1
      r2 = runFn2 distance c2 p2
      d = runFn2 distance c1 c2
      a = (r1 * r1 - r2 * r2 + d * d) / (2.0 * d)
      cvec = normalize (c2 - c1)
      check sol = (withinBounds arc1 sol) && (withinBounds arc2 sol)
   in filter check (
     case true of _ | d > r1 + r2 -> Nil -- circles too far apart
                    | d < abs (r1 - r2) -> Nil -- one circle inside the other
                    | d == r1 + r2 -> ((c1 + (scalePt cvec r1)) : Nil) -- circles are touching
                    | otherwise ->
                        let h = sqrt (r1 * r1 - a * a)
                            hvec = scalePt (rotate cvec (pi/2.0)) h
                            pt = c1 + (scalePt cvec a) -- where the center line and intersection line meet
                         in ((pt + hvec) : (pt - hvec) : Nil)
   )

intersect a@(Arc _ _ _ _) l@(Line _ _) = intersect l a

insertSplitStroke ::  Stroke -> Path -> Intersections -> Intersections
insertSplitStroke  stroke splitStroke intersections =
  insert stroke splitStroke $ insert (flipStroke stroke) (reversePath splitStroke) intersections

-- look over the points in order. If a point is close to an earlier point, map from that point to the earlier point
makePtMap :: List Point -> Map Point Point
makePtMap points =
  let mapPoint map p =
        let nearbyPt = head $ filter (\existingPt -> (runFn2 distance p existingPt) < 0.05) (values map)
         in case nearbyPt of
                 Just pt -> insert p pt map
                 Nothing -> insert p p map

   in foldl mapPoint empty points

intersectMultiple :: Stroke -> List Stroke -> Intersections
intersectMultiple stroke strokes =
  let
    intersectionTuples = do
      toIntersect <- strokes
      case intersect stroke toIntersect of
           Nil -> Nil
           newPoints -> pure $ Tuple toIntersect newPoints

    getPoints (Line p1 p2) = p1 : p2 : Nil
    getPoints (Arc c p q _) = c : p : q : Nil

    -- we don't want slight inequalities in floating point arithmetic to cause multiple nearby points to be created
    -- since we rely on point equality to do cycle detection.
    -- if an intersection point is near an existing point, or another intersection point, replace it
    ptMap = makePtMap $ (concatMap getPoints strokes) <> (concatMap snd intersectionTuples)
    shrunkTuples =
      let shrinkPoint pt = case lookup pt ptMap of
                                Just newPt -> newPt
                                Nothing -> pt
       in (\(Tuple edge intersections) -> (Tuple edge (shrinkPoint <$> intersections))) <$> intersectionTuples

    insertPoints i (Tuple edge points) =
      insertSplitStroke edge (split edge points) i

    intersections = foldl insertPoints empty shrunkTuples
  in
    insertSplitStroke stroke (split stroke (concatMap snd shrunkTuples)) intersections

nubAdjacent :: forall a. (Eq a) => List a -> List a
nubAdjacent (a : b : rest) | a == b = (nubAdjacent $ a : rest)
                           | otherwise = a : (nubAdjacent $ b : rest)

nubAdjacent (a : Nil) = a : Nil
nubAdjacent Nil = Nil

-- given a stroke and a list of intersections, return a list of strokes
split :: Stroke -> List Point -> List Stroke
split (Line p1 p2) points =
  case head lines of -- ensure we still go from p1 to p2
       Just (Line p _) | p == p1 -> lines
       _ -> reversePath lines
  where
    sortedPoints = sort $ nub $ p1 : p2 : points
    zipPointPairs (p : q : rest) = (Line p q) : (zipPointPairs (q : rest))
    zipPointPairs _ = Nil
    lines = zipPointPairs sortedPoints

split arc@(Arc c p q ccw) points =
  let
      intersectionSweep i = ptSweep c p i ccw

      -- remove p and q from intersections since we will add them back later
      intersections = sortBy (compare `on` intersectionSweep) $
                      (filter (\pt -> pt /= p && pt /= q) points)

      -- sandwich the intersections, in the right order, between p and q
      orderedIntersections = nubAdjacent $
         p : (snoc (if ccw then intersections else reverse intersections) q)

      zipAnglePairs (p1 : p2 : rest) =
        (Arc c p1 p2 ccw) : zipAnglePairs (p2 : rest)
      zipAnglePairs _ = Nil

   in if (length orderedIntersections > 2) then (zipAnglePairs orderedIntersections)
                                           else (singleton arc)
