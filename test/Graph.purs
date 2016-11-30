module Test.Main where

import Prelude
import App.Graph
import App.Geometry (Point(..))
import Data.List (List(..), (:))
import Data.Maybe (Maybe(..))
import Test.Spec (describe, it)
import Test.Spec.Assertions (shouldEqual)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner (run)

p1 = Point 1.0 1.0
p2 = Point 2.0 2.0
p3 = Point 2.0 0.0

g = addEdge (Edge p1 p2)
  $ addEdge (Edge p2 p3)
  $ emptyGraph

main = run [consoleReporter] do
  describe "Graph findPath" do
    it "should find a path across two edges" do
      findPath g p1 p3 `shouldEqual` Just (p1 : p2 : p3 : Nil)
