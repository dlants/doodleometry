module Test.Model where

import Prelude

import App.Events (Event(..), update)
import App.Geometry (Point(..))
import App.Graph (edges, points)
import App.State (Tool(..), init)
import Data.Foldable (foldl, foldr)
import Data.List (List(..), length)
import Data.Map (keys)
import Test.Spec (describe, it)
import Test.Spec.Assertions (shouldEqual)

spec = do
  describe "Drawing" do
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
      it "has the right number of points" do
        (length $ points s.graph) `shouldEqual` 4
      it "has the right number of cycles" do
        (length $ keys s.cycles) `shouldEqual` 3

    describe "draw three circles" do
      let s = foldl (flip update) init [ Select ArcTool
                                       , Draw (Point 100.0 100.0)
                                       , Draw (Point 200.0 100.0)
                                       , Draw (Point 200.0 100.0)
                                       , Draw (Point 100.0 100.0)
                                       , Draw (Point 150.0 186.6)
                                       , Draw (Point 100.0 100.0)
                                       ]
      let arcs = edges s.graph

      it "has the right number of arcs" do
        (length $ arcs) `shouldEqual` 12
      it "has the right number of points" do
        (length $ points s.graph) `shouldEqual` 6
      it "has the right number of cycles" do
        (length $ keys s.cycles) `shouldEqual` 7
