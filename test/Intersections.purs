module Test.Intersections where

import App.Geometry
import Prelude

import Data.List (List(..), sort, (:))
import Data.Map (lookup)
import Data.Maybe (Maybe(..))
import Data.Monoid (mempty)
import Math (pi)
import Test.Spec (describe, describeOnly, it, itOnly)
import Test.Spec.Assertions (shouldEqual)

spec = do
  describeOnly "intersect" do
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

    it "splitMap two circles" do
       let a2ib = (Arc (Point 200.0 100.0) (Point 100.0 100.0) (Point 150.0 13.397459621556138) true)
           a2it = (Arc (Point 200.0 100.0) (Point 100.0 100.0) (Point 150.0 186.60254037844385) false)
           a2o  = (Arc (Point 200.0 100.0) (Point 150.0 13.397459621556138) (Point 150.0 186.60254037844385) true)
           a1ib = (Arc (Point 100.0 100.0) (Point 150.0 13.397459621556138) (Point 200.0 100.0) true)
           a1it = (Arc (Point 100.0 100.0) (Point 150.0 186.60254037844385) (Point 200.0 100.0) false)
           a1o  = (Arc (Point 100.0 100.0) (Point 150.0 13.397459621556138) (Point 150.0 186.60254037844385) false)
           map = splitMap
             (Arc (Point 150.0 186.60254037844385) (Point 100.0 100.0) (Point 100.0 100.0) true)
             ( a1ib : a1it : a1o : a2ib : a2it : a2o : Nil)

       lookup a1ib map `shouldEqual` Nothing
       lookup a1it map `shouldEqual` Nothing
       lookup a1o map `shouldEqual` Just (
          (Arc (Point 100.0 100.0) (Point 150.0 13.397459621556138) (Point 50.0 186.6025403784439)  false):
          (Arc (Point 100.0 100.0) (Point 50.0 186.6025403784439) (Point 150.0 186.60254037844385) false):
          Nil
        )

       lookup a2ib map `shouldEqual` Nothing
       lookup a2it map `shouldEqual` Nothing
       lookup a2o map `shouldEqual` Just (
         (Arc (Point 200.0 100.0) (Point 150.0 13.397459621556138) (Point 250.0 186.6025403784439) true):
         (Arc (Point 200.0 100.0) (Point 250.0 186.6025403784439) (Point 150.0 186.60254037844385) true):
         Nil
       )

    it "non-intersecting arcs" do
      let c0 = Point 0.0 5.0
          p1 = Point 4.0 2.0
          p2 = Point (-4.0) 2.0
          c1 = Point 9.0 5.0
          p3 = Point 4.0 8.0

      intersect (Arc c0 p1 p2 false) (Arc c1 p3 p3 true) `shouldEqual` (p1 : Nil)
