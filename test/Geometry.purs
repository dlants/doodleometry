module Test.Geometry where

import Prelude
import App.Geometry
import Data.List (List(..), (:))
import Test.Spec (describe, it)
import Test.Spec.Assertions (shouldEqual)

p1 = Point 0.0 0.0
p2 = Point 1.0 1.0
p3 = Point 1.0 0.0
p4 = Point 0.0 1.0

l12 = Line p1 p2
l34 = Line p3 p4


spec = do
  describe "App.Geometry" do
    describe "intersect" do
      it "should find an intersection point" do
        intersect (Line (Point 0.0 0.0) (Point 1.0 1.0)) (Line (Point 0.0 1.0) (Point 1.0 0.0))
          `shouldEqual` ((Point 0.5 0.5) : Nil)

      it "should not find an intersection point for parallel lines" do
        intersect (Line (Point 0.0 0.0) (Point 0.0 1.0)) (Line (Point 1.0 0.0) (Point 1.0 1.0))
          `shouldEqual` Nil

    describe "split" do
      it "should split a line, in order" do
        split (Line (Point 0.0 0.0) (Point 1.0 1.0)) ((Point 0.2 0.2) : (Point 0.7 0.7) : (Point 0.5 0.5) : Nil)
          `shouldEqual`
          ( (Line (Point 0.0 0.0) (Point 0.2 0.2))
          : (Line (Point 0.2 0.2) (Point 0.5 0.5))
          : (Line (Point 0.5 0.5) (Point 0.7 0.7))
          : (Line (Point 0.7 0.7) (Point 1.0 1.0))
          : Nil)
