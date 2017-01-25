module Test.Model where

import Prelude
import App.Model
import App.ColorScheme (ColorScheme(..))
import App.Geometry (Point(..))
import App.Graph (edges, points)
import Data.Foldable (foldl, foldr)
import Data.List (List(..), length)
import Data.Map (keys)
import Test.Spec (describe, it)
import Test.Spec.Assertions (shouldEqual)

spec = do
  describe "App.Model" do
    it "three circles" do
      let s = foldl (flip update) init [ Select ArcTool
                                       , Click (Point 100.0 100.0)
                                       , Click (Point 200.0 100.0)
                                       , Click (Point 200.0 100.0)
                                       , Click (Point 100.0 100.0)
                                       , Click (Point 150.0 186.6)
                                       , Click (Point 100.0 100.0)
                                       ]
          arcs = edges s.graph

      (length $ arcs) `shouldEqual` 12
      (length $ points s.graph) `shouldEqual` 6
      (length $ keys s.cycles) `shouldEqual` 7

