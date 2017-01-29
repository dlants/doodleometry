module Test.Geometry where

import Prelude
import App.Geometry
import Data.List (List(..), sort, (:))
import Math (pi)
import Test.Spec (describe, it)
import Test.Spec.Assertions (shouldEqual)

spec = do
  describe "App.Geometry" do
    describe "sweep" do
      it "should produce a sweep of 2pi for same start/end point" do
        sweep (Arc (Point 10.0 10.0) (Point 20.0 10.0) (Point 20.0 10.0) true) `shouldEqual` (2.0 * pi)
        sweep (Arc (Point 10.0 10.0) (Point 20.0 10.0) (Point 20.0 10.0) false) `shouldEqual` (- 2.0 * pi)

      it "should produce a different sweep depending on ccw flag" do
        sweep (Arc (Point 10.0 10.0) (Point 20.0 10.0) (Point 10.0 20.0) true) `shouldEqual` (pi / 2.0)
        sweep (Arc (Point 10.0 10.0) (Point 20.0 10.0) (Point 10.0 20.0) false) `shouldEqual` -(3.0 * pi / 2.0)

      it "should work across the 0 boundary" do
        sweep (Arc (Point 10.0 10.0) (Point 10.0 0.0) (Point 10.0 20.0) true) `shouldEqual` pi
        sweep (Arc (Point 10.0 10.0) (Point 10.0 0.0) (Point 10.0 20.0) false) `shouldEqual` (-pi)
        sweep (Arc (Point 10.0 10.0) (Point 10.0 20.0) (Point 10.0 0.0) true) `shouldEqual` pi
        sweep (Arc (Point 10.0 10.0) (Point 10.0 20.0) (Point 10.0 0.0) false) `shouldEqual` (-pi)

    describe "outboundAngle" do
      it "should produce correct angles for arcs" do
        outboundAngle (Arc (Point 10.0 10.0) (Point 20.0 10.0) (Point 20.0 10.0) true) `shouldEqual` (pi/2.0)
        outboundAngle (Arc (Point 10.0 10.0) (Point 20.0 10.0) (Point 20.0 10.0) false) `shouldEqual` (-pi/2.0)
        outboundAngle (Arc (Point 10.0 10.0) (Point 10.0 20.0) (Point 20.0 10.0) true) `shouldEqual` pi
        outboundAngle (Arc (Point 10.0 10.0) (Point 10.0 20.0) (Point 20.0 10.0) false) `shouldEqual` 0.0

    describe "strokeOrd" do
      it "should sort strokes by 'traverseLeftWall' order" do
        let l1 = Line (Point 10.0 10.0) (Point 20.0 10.0) -- this has a different outboundAngle so should go last
            l2 = Line (Point 10.0 10.0) (Point 20.0 20.0) -- this has curvature 0 so should be between cw and ccw arcs
            a1 = Arc (Point 10.0 20.0) (Point 10.0 10.0) (Point 10.0 10.0) true -- curves away so should be last arc
            a2 = Arc (Point 10.0 0.0) (Point 10.0 10.0) (Point 10.0 10.0) false
            a3 = Arc (Point 10.0 5.0) (Point 10.0 10.0) (Point 10.0 10.0) false -- shorter radius so should be first

        sort ( l2 : l1 : a1 : a2 : a3 : Nil) `shouldEqual` (a3 : a2 : l1 : a1 : l2 : Nil)

    describe "intersect" do
      it "should find an intersection point" do
        intersect (Line (Point 0.0 0.0) (Point 1.0 1.0)) (Line (Point 0.0 1.0) (Point 1.0 0.0))
          `shouldEqual` ((Point 0.5 0.5) : Nil)

      it "should not find an intersection point for parallel lines" do
        intersect (Line (Point 0.0 0.0) (Point 0.0 1.0)) (Line (Point 1.0 0.0) (Point 1.0 1.0))
          `shouldEqual` Nil

      it "should find two intersections between a line and circle" do
        intersect (Line (Point 0.0 10.0) (Point 20.0 10.0)) (Arc (Point 10.0 10.0) (Point 0.0 10.0) (Point 0.0 10.0) true)
          `shouldEqual` ((Point 20.0 10.0) : (Point 0.0 10.0) : Nil)

      it "should find one intersection between arc and tangent" do
        intersect (Line (Point 0.0 0.0) (Point 20.0 0.0)) (Arc (Point 10.0 10.0) (Point 0.0 10.0) (Point 0.0 10.0) true)
          `shouldEqual` ((Point 10.0 0.0) : Nil)

      it "should find one intersection between arc and vertical tangent" do
        intersect (Line (Point 0.0 0.0) (Point 0.0 20.0)) (Arc (Point 10.0 10.0) (Point 0.0 10.0) (Point 0.0 10.0) true)
          `shouldEqual` ((Point 0.0 10.0) : Nil)

      it "should exclude intersections between a line and arc that are outside of the arc -- positive sweep" do
        intersect (Line (Point 0.0 10.0) (Point 20.0 10.0)) (Arc (Point 10.0 10.0) (Point 20.0 10.0) (Point 10.0 20.0) true)
          `shouldEqual` ((Point 20.0 10.0) : Nil)

        intersect (Line (Point 0.0 10.0) (Point 20.0 10.0)) (Arc (Point 10.0 10.0) (Point 10.0 20.0) (Point 20.0 10.0) true)
          `shouldEqual` ((Point 20.0 10.0) : (Point 0.0 10.0) : Nil)

      it "should exclude intersections between a line and arc that are outside of the arc -- negative sweep" do
        intersect (Line (Point 0.0 10.0) (Point 20.0 10.0)) (Arc (Point 10.0 10.0) (Point 20.0 10.0) (Point 10.0 20.0) false)
          `shouldEqual` ((Point 20.0 10.0) : (Point 0.0 10.0) : Nil)

        intersect (Line (Point 0.0 10.0) (Point 20.0 10.0)) (Arc (Point 10.0 10.0) (Point 10.0 20.0) (Point 20.0 10.0) false)
          `shouldEqual` ((Point 20.0 10.0) : Nil)

      it "can find intersection between touching circles" do
        let c0 = Point 0.0 5.0
            c1 = Point 10.0 5.0
            p1 = Point 5.0 5.0

        intersect (Arc c0 p1 p1 true) (Arc c1 p1 p1 true) `shouldEqual` (p1 : Nil)

      it "can find two intersections between two circles" do
        let c0 = Point 0.0 5.0
            c1 = Point 9.0 5.0
            p1 = Point 4.0 2.0
            p2 = Point 4.0 8.0

        intersect (Arc c0 p1 p1 true) (Arc c1 p1 p1 true) `shouldEqual` (p2 : p1 : Nil)

      it "checks for arc intersections to be within arc sweep" do
        let c0 = Point 0.0 5.0
            p1 = Point 4.0 2.0
            p2 = Point (-4.0) 2.0
            c1 = Point 9.0 5.0
            p3 = Point 4.0 8.0

        intersect (Arc c0 p1 p2 false) (Arc c1 p3 p3 true) `shouldEqual` (p1 : Nil)

    describe "split" do
      it "should split a line, in order" do
        split (Line (Point 0.0 0.0) (Point 1.0 1.0)) ((Point 0.2 0.2) : (Point 0.7 0.7) : (Point 0.5 0.5) : Nil)
          `shouldEqual`
          ( (Line (Point 0.0 0.0) (Point 0.2 0.2))
          : (Line (Point 0.2 0.2) (Point 0.5 0.5))
          : (Line (Point 0.5 0.5) (Point 0.7 0.7))
          : (Line (Point 0.7 0.7) (Point 1.0 1.0))
          : Nil)

      it "should split an arc, in clockwise order" do
        let c = (Point 10.0 10.0)
            p = (Point 20.0 10.0)
            q = (Point 20.0 10.0)
            i1 = (Point 10.0 20.0)
            i2 = (Point 0.0 10.0)
            i3 = (Point 10.0 0.0)

        split (Arc c p q true) (i1 : i2 : i3 : Nil)
          `shouldEqual`
          ( (Arc c p i1 true)
          : (Arc c i1 i2 true)
          : (Arc c i2 i3 true)
          : (Arc c i3 q true)
          : Nil)

    describe "angleDiff" do
      it "should have negative angleDiff when strokes are in ascending outboundAngle order" do
        angleDiff (Line (Point 1.0 1.0) (Point 1.0 0.0)) (Line (Point 1.0 0.0) (Point 0.0 0.0))
          `shouldEqual` (- pi / 2.0)

      it "should have positive angleDiff when strokes are in descending outboundAngle order" do
        angleDiff (Line (Point 0.0 0.0) (Point 1.0 0.0)) (Line (Point 1.0 0.0) (Point 1.0 1.0))
          `shouldEqual` (pi / 2.0)

    describe "findWrap" do
      let p1 = Point 0.0 0.0
          p2 = Point 1.0 0.0
          p3 = Point 1.0 1.0

      it "should have negative wrap when we take the inner angle (counterclockwise traversal)" do
        (findWrap ( (Line p1 p3) : (Line p3 p2) : (Line p2 p1) : Nil ) < 0.0) `shouldEqual` true

      it "should have positive wrap when we take the outer angle (clockwise traversal)" do
        (findWrap ( (Line p1 p2) : (Line p2 p3) : (Line p3 p1) : Nil ) > 0.0) `shouldEqual` true

      it "wrap when dealing with arcs" do
        let c0 = Point 0.0 5.0
            c1 = Point 9.0 5.0
            p1 = Point 4.0 2.0
            p2 = Point 4.0 8.0

        (findWrap ( (Arc c0 p1 p2 true) : (Arc c1 p2 p1 true) : Nil) > 0.0) `shouldEqual` true
        (findWrap ( (Arc c0 p2 p1 false) : (Arc c1 p1 p2 false) : Nil) < 0.0 ) `shouldEqual` true

      it "wrap when dealing with 0 angleDiff arcs" do
        let c0 = Point 5.0 5.0
            c1 = Point 15.0 5.0
            p1 = Point 10.0 5.0
            p2 = Point 0.0 5.0
            p3 = Point 20.0 5.0

            a1 = Arc c0 p2 p1 true
            a2 = Arc c1 p1 p3 true

        angleDiff a1 a2 `shouldEqual` -pi
