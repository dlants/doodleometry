module Test.Main where

import Prelude
import App.Graph
import App.Geometry (Point(..), Stroke(..), reverse)
import Data.List (List(..), singleton, (:))
import Data.Map (keys, lookup, showTree)
import Data.Maybe (Maybe(..))
import Test.Spec (describe, it)
import Test.Spec.Assertions (shouldEqual)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner (run)

p1 = Point 1.0 1.0
p2 = Point 2.0 2.0
p3 = Point 2.0 0.0

l1 = Line p1 p2
l2 = Line p2 p3

g = addStroke l1
  $ addStroke l2
  $ emptyGraph

main = run [consoleReporter] do
  describe "addStroke" do
    it "should insert strokes in both directions and in correct order" do
      keys g `shouldEqual` (p1 : p3 : p2 : Nil)
      lookup p2 g `shouldEqual` Just (reverse l1 : l2 : Nil)

  describe "getNextRightEdge" do
    it "should give the next edge" do
      getNextRightEdge l1 g `shouldEqual` Nothing
      getNextRightEdge l2 g `shouldEqual` Just l1
      getNextRightEdge (reverse l1) g `shouldEqual` Just (reverse l2)

  describe "Graph traverseRight" do
    it "should find a path across two edges" do
      traverseRight (singleton l2) g `shouldEqual` (l1 : l2 : Nil)
      traverseRight (singleton l1) g `shouldEqual` (l1 : Nil)
