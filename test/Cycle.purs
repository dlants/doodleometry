module Test.Cycle where

import Prelude
import Test.Fixtures
import App.Cycle
import App.ColorScheme (ColorScheme(..))
import App.Geometry (Point(..), Stroke(..), flipStroke)
import Data.List (List(..), singleton, (:))
import Data.Map (empty, insert)
import Data.Maybe (Maybe(..))
import Test.Spec (describe, it)
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

      it "hould not depend on edge orientation" do
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

    describe "simplify" do
      it "should prune excess edges from path" do
        simplify ((Line p1 p2) : (Line p2 p3) : (Line p3 p2) : (Line p2 p4) : Nil) `shouldEqual`
          ((Line p1 p2) : (Line p2 p4) : Nil)

        simplify ((Line p1 p2) : (Line p2 p3) : (Line p3 p4) : (Line p4 p3) : Nil) `shouldEqual`
          ((Line p1 p2) : (Line p2 p3) : Nil)

        simplify ((Line p1 p2) : (Line p2 p1) : Nil) `shouldEqual` Nil

        simplify ((Line p1 p2) : (Line p2 p3) : (Line p3 p2) : (Line p2 p1) : Nil) `shouldEqual` Nil

      it "should not simplify a cycle" do
        simplify ((Line p1 p2) : (Line p2 p3) : (Line p3 p1) : Nil) `shouldEqual`
          ((Line p1 p2) : (Line p2 p3) : (Line p3 p1) : Nil)

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

    describe "updateCycles" do
      it "should insert a cycle" do
        updateCycles empty g2 empty (singleton l13) `shouldEqual`
          (insert c123 White $ empty)

        updateCycles empty g2 empty (l13 : l32 : Nil) `shouldEqual`
          (insert c123 White $ empty)

      it "should split a cycle" do
        updateCycles cycles g3 empty (singleton l13) `shouldEqual`
          (insert c123 Red $ insert c134 Red $ empty)
