module Test.Cycle where

import Prelude
import Test.Fixtures
import App.Cycle
import App.Geometry (Point(..), Stroke(..), flipStroke)
import App.Graph (addStroke, emptyGraph, getEdgesForPt, getNextEdge)
import CSS.Color (black, white)
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
          ai1 = Arc c1 p1 p2 true -- inner arc around center 1
          ao1 = Arc c1 p2 p1 true -- outer ...
          ai2 = Arc c2 p2 p1 true
          ao2 = Arc c2 p1 p2 true
          gTwoCircles = addStroke ai1
                      $ addStroke ao1
                      $ addStroke ai2
                      $ addStroke ao2
                      $ emptyGraph

      describe "edge ordering around points" do
        it "around p1" do
          getEdgesForPt p1 gTwoCircles `shouldEqual` Just (ai1 : (flipStroke ai2) : (flipStroke ao1) : ao2: Nil)

      describe "nextEdge around p1" do
        it "edge after ai2 should be ao1" do
          getNextEdge ai2 gTwoCircles `shouldEqual` Just (flipStroke ao1)
        it "edge after ao1 should be ao2" do
          getNextEdge ao1 gTwoCircles `shouldEqual` Just ao2
        it "edge after ao2 should be ai1" do
          getNextEdge (flipStroke ao2) gTwoCircles `shouldEqual` Just ai1
        it "edge after ai1 should be ai2" do
          getNextEdge (flipStroke ai1) gTwoCircles `shouldEqual` Just (flipStroke ai2)

      describe "nextEdge around p2" do
        it "edge after ai1 should be ao2" do
          getNextEdge ai1 gTwoCircles `shouldEqual` Just (flipStroke ao2)
        it "edge after ao2 should be ao1" do
          getNextEdge ao2 gTwoCircles `shouldEqual` Just ao1
        it "edge after ao1 should be ai2" do
          getNextEdge (flipStroke ao1) gTwoCircles `shouldEqual` Just ai2
        it "edge after ai2 should be ai1" do
          getNextEdge (flipStroke ai2) gTwoCircles `shouldEqual` Just (flipStroke ai1)

      describe "findCycle" do
        it "should find the right cycle when going up the inner arc around circle 1" do
          findCycle gTwoCircles ai1 `shouldEqual` Just (Cycle $ ai1 : (flipStroke ao2) : Nil)

        it "should find the center cycle when going down the inner arc around circle 1" do
          findCycle gTwoCircles (flipStroke ai1) `shouldEqual` Just (Cycle $ (flipStroke ai1) : (flipStroke ai2) : Nil)

        it "should find the left cycle when going up the outer arc around circle 1" do
          findCycle gTwoCircles (flipStroke ao1) `shouldEqual` Just (Cycle $ (flipStroke ao1) : ai2 : Nil)

        --it "should give negative wrap for a1 : a3" do
        -- (findWrap (a1 : (flipStroke a3) : Nil) < 0.0) `shouldEqual` true

        --it "should not find the outside cycle for two circles" do
        --  findCycle gTwoCircles a3 `shouldEqual` Nothing
        --  findCycle gTwoCircles a2 `shouldEqual` Nothing

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
          Just ( (flipStroke inner1)
               : (flipStroke outer2)
               : outer1
               : inner2
               : Nil)

      it "stroke order at p2" do
        getEdgesForPt p2 gTwoCircles `shouldEqual`
          Just ( outer2
               : inner1
               : (flipStroke inner2)
               : (flipStroke outer1)
               : Nil)

      it "getNextEdge order inner1" do
        getNextEdge inner1 gTwoCircles `shouldEqual` Just (flipStroke outer2)
        getNextEdge (flipStroke inner1) gTwoCircles `shouldEqual` Just (flipStroke inner2)

      it "getNextEdge order outer1" do
        getNextEdge outer1 gTwoCircles `shouldEqual` Just outer2
        getNextEdge (flipStroke outer1) gTwoCircles `shouldEqual` Just inner2

      it "getNextEdge order inner2" do
        getNextEdge inner2 gTwoCircles `shouldEqual` Just (flipStroke outer1)
        getNextEdge (flipStroke inner2) gTwoCircles `shouldEqual` Just (flipStroke inner1)

      it "getNextEdge order outer2" do
        getNextEdge outer2 gTwoCircles `shouldEqual` Just outer1
        getNextEdge (flipStroke outer2) gTwoCircles `shouldEqual` Just inner1

    describe "copyColors" do
      it "should respect stroke order" do
        ( copyColors
          ( insert (Cycle $ (Line (Point 0.0 0.0) (Point 0.0 1.0)) : Nil) black
          $ empty
          )
          ( insert (Cycle $ (Line (Point 1.0 0.0) (Point 0.0 0.0)) : Nil) white
          $ empty
          )
        ) `shouldEqual` (
          insert (Cycle $ (Line (Point 1.0 0.0) (Point 0.0 0.0)) : Nil) white
          $ empty
        )

      it "should match any stroke" do
        ( copyColors
          ( insert (Cycle $ (Line (Point 0.0 0.0) (Point 0.0 1.0)) : Nil) black
          $ empty
          )
          ( insert (Cycle $ ( Line (Point 1.0 0.0) (Point 0.0 0.0))
                            : Line (Point 0.0 0.0) (Point 0.0 1.0)
                            : Nil
                            ) white
          $ empty
          )
        ) `shouldEqual` (
          insert (Cycle $ ( Line (Point 1.0 0.0) (Point 0.0 0.0))
                            : Line (Point 0.0 0.0) (Point 0.0 1.0)
                            : Nil
                            ) black
          $ empty
        )
