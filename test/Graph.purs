module Test.Main where

import Prelude
import App.Graph
import App.Geometry (Point(..), Stroke(..), reverse)
import Data.List (List(..), singleton, (:))
import Data.Map (keys, lookup, showTree)
import Data.Maybe (Maybe(..))
import Data.Unfoldable (unfoldr)
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

l3 = Line p3 p1
g2 = addStroke l3 g

main = run [consoleReporter] do
  describe "addStroke" do
    it "should insert strokes in both directions and in correct order" do
      keys g `shouldEqual` (p1 : p3 : p2 : Nil)
      lookup p2 g `shouldEqual` Just (reverse l1 : l2 : Nil)

  describe "getNextEdges" do
    it "should generate the list of next edges to follow" do
      getNextEdges (singleton l1) g `shouldEqual` Nil
      getNextEdges (singleton l2) g `shouldEqual` (singleton $ reverse l1)

  describe "unfoldr traverseRight" do
    it "should generate all traversals" do
      (unfoldr traverseRight $ Traversal (singleton $ singleton l2) g)
        `shouldEqual` ((singleton l2) : (l1 : l2 : Nil) : Nil)
      (unfoldr traverseRight $ Traversal (singleton $ singleton l1) g2)
        `shouldEqual` ((l1 : Nil) : (l3 : l1 : Nil) : (l2 : l3 : l1 : Nil) : Nil)

  describe "findCycle" do
    it "should find the cycle!" do
      findCycle l1 g `shouldEqual` Nothing
      findCycle l3 g2 `shouldEqual` Just (l1: l2 : l3: Nil)

  describe "findCycles" do
    it "should find all cycles!" do
      findCycles g `shouldEqual` Nil
      findCycles g2 `shouldEqual` ((l1: l2 : l3: Nil) : Nil)
