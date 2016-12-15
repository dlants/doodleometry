module Test.Graph where

import Prelude
import App.Graph
import App.ColorScheme (ColorScheme(..))
import App.Cycle (Cycle(..), cut, joinCycles)
import App.Geometry (Point(..), Stroke(..), flip)
import App.Update (updateCycles)
import Data.List (List(..), singleton, (:))
import Data.Map (empty, insert, keys, lookup, showTree)
import Data.Maybe (Maybe(..))
import Data.Unfoldable (unfoldr)
import Test.Spec (describe, it)
import Test.Spec.Assertions (shouldEqual)

p1 = Point 1.0 0.0
p2 = Point 2.0 0.0
p3 = Point 3.0 0.0
p4 = Point 4.0 0.0

l12 = Line p1 p2
l23 = Line p2 p3
l34 = Line p3 p4
l41 = Line p4 p1

l21 = Line p2 p1
l32 = Line p3 p2
l43 = Line p4 p3
l14 = Line p1 p4

l31 = Line p3 p1
l13 = Line p1 p3
l42 = Line p4 p2
l24 = Line p2 p4

c1234 = Cycle $ l12 : l23 : l34 : l41 : Nil
c123 = Cycle $ l12 : l23 : l31 : Nil
c134 = Cycle $ l13 : l34 : l41 : Nil

g = addStroke l12
  $ addStroke l23
  $ emptyGraph

g2 = addStroke l31 g

g3 = addStroke l12
   $ addStroke l23
   $ addStroke l34
   $ addStroke l41
   $ addStroke l13
   $ emptyGraph

cycles = insert c1234 Red $ empty

spec = do
  describe "App.Graph" do
    describe "addStroke" do
      it "should insert strokes in both directions and in correct order" do
        keys g `shouldEqual` (p1 : p2 : p3 : Nil)
        lookup p2 g `shouldEqual` Just (flip l12 : l23 : Nil)

    describe "getNextEdges" do
      it "should generate the list of next edges to follow" do
        getNextEdges (singleton l12) g `shouldEqual` Nil
        getNextEdges (singleton l23) g `shouldEqual` (singleton $ flip l12)

    describe "unfoldr traverseRight" do
      it "should generate all traversals" do
        (unfoldr traverseRight $ Traversal (singleton $ singleton l23) g)
          `shouldEqual` ((singleton l23) : (l12 : l23 : Nil) : Nil)
        (unfoldr traverseRight $ Traversal (singleton $ singleton l12) g2)
          `shouldEqual` ((l12 : Nil) : (l31 : l12 : Nil) : (l23 : l31 : l12 : Nil) : Nil)

    describe "cutCycle" do
      it "should cut the cycle and orient it the right way" do
        cut c123 l13 `shouldEqual` (l32 : l21 : Nil)
        cut c134 (flip l13) `shouldEqual` (l14 : l43 : Nil)

    describe "joinCycles" do
      it "should join two cycles that share an edge" do
        joinCycles c123 c134 l13 `shouldEqual` c1234

    describe "findCycle" do
      it "should find the cycle!" do
        findCycle g l12 `shouldEqual` Nothing
        findCycle g2 l31 `shouldEqual` Just (Cycle (l12 : l23 : l31 : Nil))

    describe "findCycles" do
      it "should find all cycles!" do
        findCycles g `shouldEqual` Nil
        findCycles g2 `shouldEqual` (Cycle (l12 : l23 : l31 : Nil) : Nil)

    describe "updateCycles" do
      it "should split a cycle" do
        updateCycles cycles g3 empty (singleton l13) `shouldEqual` (insert c123 Red $ insert c134 Red $ empty)
