module Test.Cycle where

import Prelude
import Test.Fixtures
import App.Cycle
import App.ColorScheme (ColorScheme(..))
import App.Geometry (Point(..), Stroke(..), angleDiff, findWrap, flipStroke)
import App.Graph (addStroke, emptyGraph, getEdgesForPt, getNextEdge)
import Data.List (List(..), singleton, (:))
import Data.Map (empty, insert, lookup)
import Data.Maybe (Maybe(..))
import Test.Spec (describe, describeOnly, it)
import Test.Spec.Assertions (shouldEqual)

spec = do
  describe "App.Cycle" do
    describe "cycleOrd" do
      it "should not depend on edge order" do
        shouldEqual
          (Cycle ( Line (Point 0.0 0.0) (Point 0.5 0.0)
                 : Line (Point 0.5 0.0) (Point 0.0 0.5)
                 : Line (Point 0.0 0.5) (Point 0.0 0.0)
                 : Nil
                 )
          )
          (Cycle ( Line (Point 0.5 0.0) (Point 0.0 0.5)
                 : Line (Point 0.0 0.5) (Point 0.0 0.0)
                 : Line (Point 0.0 0.0) (Point 0.5 0.0)
                 : Nil
                 )
          )

      it "should not depend on edge orientation" do
        shouldEqual
          (Cycle ( Line (Point 0.0 0.0) (Point 0.5 0.0)
                 : Line (Point 0.5 0.0) (Point 0.0 0.5)
                 : Line (Point 0.0 0.5) (Point 0.0 0.0)
                 : Nil
                 )
          )
          (Cycle ( Line (Point 0.5 0.0) (Point 0.0 0.0)
                 : Line (Point 0.0 0.0) (Point 0.0 0.5)
                 : Line (Point 0.0 0.5) (Point 0.5 0.0)
                 : Nil
                 )
          )

    describeOnly "simplifyCycle" do
      it "should not simplifyCycle a cycle" do
        simplifyCycle ((Line p1 p2) : (Line p2 p3) : (Line p3 p1) : Nil) `shouldEqual`
          ((Line p1 p2) : (Line p2 p3) : (Line p3 p1) : Nil)

      it "should return Nil for Nil" do
        simplifyCycle Nil `shouldEqual` Nil

      it "should return Nil for path that doubles back on itself" do
        simplifyCycle ((Line p1 p2) : (Line p2 p3) : (Line p3 p2) : (Line p2 p1) : Nil) `shouldEqual` Nil

      -- TODO: more here


    describe "cutCycle" do
      it "should cut the cycle and orient it the right way" do
        cut c123 l13 `shouldEqual` (l32 : l21 : Nil)
        cut c134 (flipStroke l13) `shouldEqual` (l14 : l43 : Nil)

    describe "joinCycles" do
      it "should join two cycles that share an edge" do
        joinCycles c123 c134 l13 `shouldEqual` c1234
        joinCycles c123 c134 l31 `shouldEqual` c1234

      it "should join two cycles that form a triangle" do
        joinCycles
          (Cycle ( Line (Point 0.0 0.0) (Point 0.5 0.0)
                 : Line (Point 0.5 0.0) (Point 0.0 0.5)
                 : Line (Point 0.0 0.5) (Point 0.0 0.0)
                 : Nil
                 )
          )
          (Cycle ( Line (Point 0.5 0.0) (Point 1.0 0.0)
                 : Line (Point 1.0 0.0) (Point 0.0 1.0)
                 : Line (Point 0.0 1.0) (Point 0.0 0.5)
                 : Line (Point 0.0 0.5) (Point 0.5 0.0)
                 : Nil
                 )
          )
          (Line (Point 0.0 0.5) (Point 0.5 0.0))
          `shouldEqual`
          (Cycle ( Line (Point 0.5 0.0) (Point 1.0 0.0)
                 : Line (Point 1.0 0.0) (Point 0.0 1.0)
                 : Line (Point 0.0 1.0) (Point 0.0 0.5)
                 : Line (Point 0.0 0.5) (Point 0.0 0.0)
                 : Line (Point 0.0 0.0) (Point 0.5 0.0)
                 : Nil
                 )
          )

    describe "findCycle" do
      it "should not find cycles in two segments" do
        findCycle g l12 `shouldEqual` Nothing
        findCycle g l23 `shouldEqual` Nothing

      it "should find the clockwise cycle in a triangle, but not the counter clockwise one" do
        findCycle g2 l12 `shouldEqual` Nothing
        findCycle g2 l21 `shouldEqual` Just (Cycle (l21 : l13 : l32 : Nil))

      it "should find bottom cycle in an hourglass (but not counter clockwise)" do
        findCycle g4 (Line (Point 1.0 0.0) (Point 0.0 0.0)) `shouldEqual`
          Just (Cycle (
                 (Line (Point 1.0 0.0) (Point 0.5 0.5)) :
                 (Line (Point 0.5 0.5) (Point 0.0 0.0)) :
                 (Line (Point 0.0 0.0) (Point 1.0 0.0)) :
                 Nil
               )
             )
        findCycle g4 (Line (Point 0.0 0.0) (Point 1.0 0.0)) `shouldEqual` Nothing

      it "should find top cycle in an hourglass (but not counter clockwise)" do
        findCycle g4 (Line (Point 0.0 1.0) (Point 1.0 1.0)) `shouldEqual`
          Just (Cycle (
                 (Line (Point 0.0 1.0) (Point 1.0 1.0)) :
                 (Line (Point 1.0 1.0) (Point 0.5 0.5)) :
                 (Line (Point 0.5 0.5) (Point 0.0 1.0)) :
                 Nil
               )
             )
        findCycle g4 (Line (Point 1.0 1.0) (Point 0.0 1.0)) `shouldEqual` Nothing

      it "should consider a single ccw circle a cycle" do
        let arc = Arc (Point 0.0 0.0) (Point 1.0 0.0) (Point 1.0 0.0) false
            gArc = addStroke arc emptyGraph

        findCycle gArc arc `shouldEqual` Just (Cycle $ arc : Nil)
        findCycle gArc (flipStroke arc) `shouldEqual` Nothing

    describe "twoCircles" do
      let c1 = Point 0.0 5.0
          c2 = Point 8.0 5.0
          p1 = Point 4.0 2.0
          p2 = Point 4.0 8.0
          a1 = Arc c1 p1 p2 true
          a2 = Arc c1 p2 p1 true
          a3 = Arc c2 p1 p2 true
          a4 = Arc c2 p2 p1 true
          gTwoCircles = addStroke a1
                      $ addStroke a2
                      $ addStroke a3
                      $ addStroke a4
                      $ emptyGraph

      it "should suggest a3 after a1 and a1 after a3" do
        getNextEdge a1 gTwoCircles `shouldEqual` Just (flipStroke a3)
        getNextEdge (flipStroke a3) gTwoCircles `shouldEqual` Just a1

      it "should give negative angleDiff for a1 : a3 and a3 : a1" do
        (angleDiff a1 (flipStroke a3) < 0.0) `shouldEqual` true
        (angleDiff (flipStroke a3) a1 < 0.0) `shouldEqual` true

      it "should give negative wrap for a1 : a3" do
        (findWrap (a1 : (flipStroke a3) : Nil) < 0.0) `shouldEqual` true

      it "should find three cycles in two intersecting circles" do
        findCycle gTwoCircles a1 `shouldEqual` Just (Cycle $ a1 : (flipStroke a3) : Nil)
        findCycle gTwoCircles (flipStroke a1) `shouldEqual` Just (Cycle $ a1 : a4 : Nil)
        findCycle gTwoCircles a4 `shouldEqual` Just (Cycle $ a4 : (flipStroke a2) : Nil)

      it "should not find the outside cycle for two circles" do
        findCycle gTwoCircles a3 `shouldEqual` Nothing
        findCycle gTwoCircles a2 `shouldEqual` Nothing

    describe "twoCirclesVertical" do
      let c1 = Point 5.0 0.0
          c2 = Point 5.0 8.0
          p1 = Point 2.0 4.0
          p2 = Point 8.0 4.0
          outer1 = Arc c1 p1 p2 true
          inner1 = Arc c1 p2 p1 true
          inner2 = Arc c2 p1 p2 true
          outer2 = Arc c2 p2 p1 true
          gTwoCircles = addStroke inner1
                      $ addStroke outer1
                      $ addStroke inner2
                      $ addStroke outer2
                      $ emptyGraph

      it "stroke order at p1" do
        getEdgesForPt p1 gTwoCircles `shouldEqual`
          Just ( outer1
               : inner2
               : (flipStroke inner1)
               : (flipStroke outer2)
               : Nil)

      it "stroke order at p2" do
        getEdgesForPt p2 gTwoCircles `shouldEqual`
          Just ( (flipStroke inner2)
               : (flipStroke outer1)
               : outer2
               : inner1
               : Nil)

      it "getNextEdge order inner1" do
        getNextEdge inner1 gTwoCircles `shouldEqual` Just (flipStroke outer2)
        --getNextEdge (flipStroke inner1) gTwoCircles `shouldEqual` Just (flipStroke inner2)

      it "getNextEdge order outer1" do
        getNextEdge outer1 gTwoCircles `shouldEqual` Just outer2
        getNextEdge (flipStroke outer1) gTwoCircles `shouldEqual` Just inner2

      it "getNextEdge order inner2" do
        getNextEdge inner2 gTwoCircles `shouldEqual` Just (flipStroke outer1)
        getNextEdge (flipStroke inner2) gTwoCircles `shouldEqual` Just (flipStroke inner1)

      it "getNextEdge order outer2" do
        getNextEdge outer2 gTwoCircles `shouldEqual` Just outer1
        getNextEdge (flipStroke outer2) gTwoCircles `shouldEqual` Just inner1
