module Test.Intersections where

import App.Geometry
import Prelude

import Data.List (List(..), sort, (:))
import Data.Map (empty, insert, lookup)
import Data.Maybe (Maybe(..))
import Data.Monoid (mempty)
import Math (pi)
import Test.Spec (describe, describeOnly, it, itOnly)
import Test.Spec.Assertions (shouldEqual)

spec = do
  describe "intersect" do
    it "intersecting lines" do
      intersect (Line (Point 0.0 0.0) (Point 1.0 1.0)) (Line (Point 0.0 1.0) (Point 1.0 0.0))
        `shouldEqual` ((Point 0.5 0.5) : Nil)

    it "parallel lines" do
      intersect (Line (Point 0.0 0.0) (Point 0.0 1.0)) (Line (Point 1.0 0.0) (Point 1.0 1.0))
        `shouldEqual` Nil

    it "line and circle" do
      intersect (Line (Point 0.0 10.0) (Point 20.0 10.0)) (Arc (Point 10.0 10.0) (Point 0.0 10.0) (Point 0.0 10.0) true)
        `shouldEqual` ((Point 20.0 10.0) : (Point 0.0 10.0) : Nil)

    it "arc and horizontal tangent" do
      intersect (Line (Point 0.0 0.0) (Point 20.0 0.0)) (Arc (Point 10.0 10.0) (Point 0.0 10.0) (Point 0.0 10.0) true)
        `shouldEqual` ((Point 10.0 0.0) : Nil)

    it "arc and vertical tangent" do
      intersect (Line (Point 0.0 0.0) (Point 0.0 20.0)) (Arc (Point 10.0 10.0) (Point 0.0 10.0) (Point 0.0 10.0) true)
        `shouldEqual` ((Point 0.0 10.0) : Nil)

    it "intersection outside of arc sweep (cc)" do
      intersect (Line (Point 0.0 10.0) (Point 20.0 10.0)) (Arc (Point 10.0 10.0) (Point 20.0 10.0) (Point 10.0 20.0) true)
        `shouldEqual` ((Point 20.0 10.0) : Nil)

      intersect (Line (Point 0.0 10.0) (Point 20.0 10.0)) (Arc (Point 10.0 10.0) (Point 10.0 20.0) (Point 20.0 10.0) true)
        `shouldEqual` ((Point 20.0 10.0) : (Point 0.0 10.0) : Nil)

    it "intersection outside of arc sweep (ccw)" do
      intersect (Line (Point 0.0 10.0) (Point 20.0 10.0)) (Arc (Point 10.0 10.0) (Point 20.0 10.0) (Point 10.0 20.0) false)
        `shouldEqual` ((Point 20.0 10.0) : (Point 0.0 10.0) : Nil)

      intersect (Line (Point 0.0 10.0) (Point 20.0 10.0)) (Arc (Point 10.0 10.0) (Point 10.0 20.0) (Point 20.0 10.0) false)
        `shouldEqual` ((Point 20.0 10.0) : Nil)

    it "tangent circles" do
      let c0 = Point 0.0 5.0
          c1 = Point 10.0 5.0
          p1 = Point 5.0 5.0

      intersect (Arc c0 p1 p1 true) (Arc c1 p1 p1 true) `shouldEqual` (p1 : Nil)

    it "intersecting circles" do
      let c0 = Point 0.0 5.0
          c1 = Point 9.0 5.0
          p1 = Point 4.0 2.0
          p2 = Point 4.0 8.0

      intersect (Arc c0 p1 p1 true) (Arc c1 p1 p1 true) `shouldEqual` (p2 : p1 : Nil)

  describe "splitMap" do
    it "two lines" do
      (splitMap (Line (Point 0.0 0.0) (Point 10.0 10.0)) ((Line (Point 0.0 10.0) (Point 10.0 0.0)) : Nil)) `shouldEqual`
        ( insert (Line (Point 0.0 0.0) (Point 10.0 10.0)) ((Line (Point 0.0 0.0) (Point 5.0 5.0)) : (Line (Point 5.0 5.0) (Point 10.0 10.0)) : Nil)
        $ insert (Line (Point 10.0 10.0) (Point 0.0 0.0)) ((Line (Point 10.0 10.0) (Point 5.0 5.0)) : (Line (Point 5.0 5.0) (Point 0.0 0.0)) : Nil)
        $ insert (Line (Point 0.0 10.0) (Point 10.0 0.0)) ((Line (Point 0.0 10.0) (Point 5.0 5.0)) : (Line (Point 5.0 5.0) (Point 10.0 0.0)) : Nil)
        $ insert (Line (Point 10.0 0.0) (Point 0.0 10.0)) ((Line (Point 10.0 0.0) (Point 5.0 5.0)) : (Line (Point 5.0 5.0) (Point 0.0 10.0)) : Nil)
        empty
        )

    it "two circles" do
      (splitMap (Arc (Point 100.0 100.0) (Point 0.0 100.0) (Point 0.0 100.0) true)
        ((Arc (Point 200.0 100.0) (Point 300.0 100.0) (Point 300.0 100.0) true): Nil)) `shouldEqual`
        ( insert (Arc (Point 100.0 100.0) (Point 0.0 100.0) (Point 0.0 100.0) true)
          ( (Arc (Point 100.0 100.0) (Point 0.0 100.0) (Point 150.0 13.397459621556138) true)
          : (Arc (Point 100.0 100.0) (Point 150.0 13.397459621556138) (Point 150.0 186.60254037844385) true)
          : (Arc (Point 100.0 100.0) (Point 150.0 186.60254037844385) (Point 0.0 100.0) true)
          : Nil
          )
        $ insert (Arc (Point 100.0 100.0) (Point 0.0 100.0) (Point 0.0 100.0) false)
          ( (Arc (Point 100.0 100.0) (Point 0.0 100.0) (Point 150.0 186.60254037844385) false)
          : (Arc (Point 100.0 100.0) (Point 150.0 186.60254037844385) (Point 150.0 13.397459621556138) false)
          : (Arc (Point 100.0 100.0) (Point 150.0 13.397459621556138) (Point 0.0 100.0) false)
          : Nil
          )
        $ insert (Arc (Point 200.0 100.0) (Point 300.0 100.0) (Point 300.0 100.0) false)
          ( (Arc (Point 200.0 100.0) (Point 300.0 100.0) (Point 150.0 13.397459621556138) false)
          : (Arc (Point 200.0 100.0) (Point 150.0 13.397459621556138) (Point 150.0 186.60254037844385) false)
          : (Arc (Point 200.0 100.0) (Point 150.0 186.60254037844385) (Point 300.0 100.0) false)
          : Nil
          )
        $ insert (Arc (Point 200.0 100.0) (Point 300.0 100.0) (Point 300.0 100.0) true)
          ( (Arc (Point 200.0 100.0) (Point 300.0 100.0) (Point 150.0 186.60254037844385) true)
          : (Arc (Point 200.0 100.0) (Point 150.0 186.60254037844385) (Point 150.0 13.397459621556138) true)
          : (Arc (Point 200.0 100.0) (Point 150.0 13.397459621556138) (Point 300.0 100.0) true)
          : Nil
          )
        empty
        )


    it "non-intersecting arcs" do
      let c0 = Point 0.0 5.0
          p1 = Point 4.0 2.0
          p2 = Point (-4.0) 2.0
          c1 = Point 9.0 5.0
          p3 = Point 4.0 8.0

      intersect (Arc c0 p1 p2 false) (Arc c1 p3 p3 true) `shouldEqual` (p1 : Nil)
