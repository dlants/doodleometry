module Test.Graph where

import Prelude
import App.Graph
import Test.Fixtures
import App.ColorScheme (ColorScheme(..))
import App.Cycle (Cycle(..), cut, joinCycles)
import App.Geometry (Point(..), Stroke(..), flipStroke)
import Data.List (List(..), singleton, (:))
import Data.Map (empty, insert, keys, lookup, showTree)
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Data.Unfoldable (unfoldr)
import Test.Spec (describe, it)
import Test.Spec.Assertions (shouldEqual)


spec = do
  describe "App.Graph" do
    describe "addStroke" do
      it "should insert strokes in both directions and in correct order" do
        points g `shouldEqual` (p1 : p2 : p3 : Nil)
        getEdgesForPt p2 g `shouldEqual` Just (l23 : flipStroke l12 : Nil)

    describe "getNextEdge" do
      it "should generate the list of next edges to follow" do
        getNextEdge l12 g `shouldEqual` Just l23
        getNextEdge l23 g `shouldEqual` Just l32

      it "should pick the next edge in ascending radiant order" do
        -- bottom right inbound, top right outbound
        getNextEdge (Line (Point 1.0 0.0) (Point 0.5 0.5)) g4 `shouldEqual`
          Just (Line (Point 0.5 0.5) (Point 1.0 1.0))

        -- bottom left inbound, bottom right outbound
        getNextEdge (Line (Point 0.0 0.0) (Point 0.5 0.5)) g4 `shouldEqual`
          Just (Line (Point 0.5 0.5) (Point 1.0 0.0))
