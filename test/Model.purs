module Test.Model where

import Prelude

import App.Events (Event(..), update)
import App.Geometry (Point(..), Stroke(..), unorderedEq)
import App.Graph (allEdges, edges, points)
import App.State (Tool(..), init)
import Data.Foldable (foldl, foldr)
import Data.List (List(..), length, nubBy, sort, (:))
import Data.Map (keys)
import Data.Monoid (mempty)
import Test.Spec (describe, describeOnly, it)
import Test.Spec.Assertions (shouldEqual)

spec = do
  describe "Integration" do
    describe "one circle" do
      let s = foldl (flip update) init [ Select ArcTool
                                       , Draw (Point 100.0 100.0)
                                       , Draw (Point 200.0 100.0)
                                       ]
      let arcs = edges s.graph

      it "has the right number of arcs" do
        (length $ arcs) `shouldEqual` 1
      it "has the right number of points" do
        (length $ points s.graph) `shouldEqual` 1
      it "has the right number of cycles" do
        (length $ keys s.cycles) `shouldEqual` 1

    describe "two circles" do
      let s = foldl (flip update) init [ Select ArcTool
                                       , Draw (Point 100.0 100.0)
                                       , Draw (Point 200.0 100.0)
                                       , Draw (Point 200.0 100.0)
                                       , Draw (Point 100.0 100.0)
                                       ]
      let arcs = edges s.graph

      it "has the right number of arcs" do
        (length $ arcs) `shouldEqual` 6
      it "has the right arcs" do
         (nubBy unorderedEq $ sort $ allEdges s.graph) `shouldEqual`
         (
           (Arc (Point 200.0 100.0) (Point 100.0 100.0) (Point 150.0 13.397459621556138) true) :
           (Arc (Point 200.0 100.0) (Point 100.0 100.0) (Point 150.0 186.60254037844385) false) :
           (Arc (Point 100.0 100.0) (Point 150.0 13.397459621556138) (Point 150.0 186.60254037844385) false) :
           (Arc (Point 200.0 100.0) (Point 150.0 13.397459621556138) (Point 150.0 186.60254037844385) true) :
           (Arc (Point 100.0 100.0) (Point 150.0 13.397459621556138) (Point 200.0 100.0) true) :
           (Arc (Point 100.0 100.0) (Point 150.0 186.60254037844385) (Point 200.0 100.0) false) :
           Nil
         )

      it "has the right number of points" do
        (length $ points s.graph) `shouldEqual` 4
      it "has the right number of cycles" do
        (length $ keys s.cycles) `shouldEqual` 3

    describe "three circles" do
      let s = foldl (flip update) init [ Select ArcTool
                                       , Draw (Point 100.0 100.0) -- c1
                                       , Draw (Point 200.0 100.0) -- c2
                                       , Draw (Point 200.0 100.0) -- c2
                                       , Draw (Point 100.0 100.0) -- c1
                                       , Draw (Point 150.0 186.6) -- c3
                                       , Draw (Point 100.0 100.0) -- c1
                                       ]
      let arcs = edges s.graph

      it "has the right number of arcs" do
        (length $ arcs) `shouldEqual` 12

      it "has the right arcs" do
         (nubBy unorderedEq $ sort $ allEdges s.graph) `shouldEqual` (
           (Arc (Point 100.0 100.0) (Point 50.0 186.6025403784439) (Point 150.0 13.397459621556138) true) :
           (Arc (Point 150.0 186.60254037844385) (Point 50.0 186.6025403784439) (Point 100.0 100.0) true) :
           (Arc (Point 100.0 100.0) (Point 50.0 186.6025403784439) (Point 150.0 186.60254037844385) false) :
           (Arc (Point 150.0 186.60254037844385) (Point 50.0 186.6025403784439) (Point 250.0 186.6025403784439) false) :
           (Arc (Point 200.0 100.0) (Point 100.0 100.0) (Point 150.0 13.397459621556138) true) :
           (Arc (Point 150.0 186.60254037844385) (Point 100.0 100.0) (Point 200.0 100.0) true) :
           (Arc (Point 200.0 100.0) (Point 100.0 100.0) (Point 150.0 186.60254037844385) false) :
           (Arc (Point 200.0 100.0) (Point 150.0 13.397459621556138) (Point 250.0 186.6025403784439) true) :
           (Arc (Point 100.0 100.0) (Point 150.0 13.397459621556138) (Point 200.0 100.0) true) :
           (Arc (Point 100.0 100.0) (Point 150.0 186.60254037844385) (Point 200.0 100.0) false) :
           (Arc (Point 200.0 100.0) (Point 150.0 186.60254037844385) (Point 250.0 186.6025403784439) false) :
           (Arc (Point 150.0 186.60254037844385) (Point 200.0 100.0) (Point 250.0 186.6025403784439) true) :
           Nil
         )

      it "has the right number of points" do
        (length $ points s.graph) `shouldEqual` 6

      it "has the right number of cycles" do
        (length $ keys s.cycles) `shouldEqual` 7
