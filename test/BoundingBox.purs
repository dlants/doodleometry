module Test.BoundingBox where

import App.BoundingBox
import App.Geometry
import Prelude

import App.Cycle as Cycle
import Data.List (List(..), (:))
import Test.Spec (describe, describeOnly, it, itOnly)
import Test.Spec.Assertions (shouldEqual)

spec = do
  let c1 = Point 0.0 5.0
      c2 = Point 8.0 5.0
      p1 = Point 4.0 2.0
      p2 = Point 4.0 8.0
      ai1 = Arc c1 p1 p2 true -- inner arc around center 1
      ao1 = Arc c1 p2 p1 true -- outer ...
      ai2 = Arc c2 p2 p1 true
      ao2 = Arc c2 p1 p2 true

  describe "App.BoundingBox" do
    it "#size" do
      (size $ BoundingBox {
        top: 10.0,
        left: 0.0,
        bottom: -5.0,
        right: 1.0
      }) `shouldEqual` 15.0

    it "#append" do
      ((BoundingBox {
        top: 10.0,
        left: 10.0,
        right: 20.0,
        bottom: 0.0
      }) <> (BoundingBox {
        top: 20.0,
        left: 15.0,
        right: 16.0,
        bottom: -10.0
      })) `shouldEqual` (BoundingBox {
        top: 20.0,
        left: 10.0,
        right: 20.0,
        bottom: -10.0
      })

    describe "stroke#toBoundingBox" do
       it "line" do
         toBoundingBox (Line (Point 10.0 10.0) (Point (-5.0) 20.0)) `shouldEqual` (BoundingBox {
           top: 20.0,
           left: -5.0,
           bottom: 10.0,
           right: 10.0
         })

       it "90 degree arc" do
         toBoundingBox (Arc (Point 0.0 0.0) (Point 10.0 0.0) (Point 0.0 10.0) true) `shouldEqual` (BoundingBox {
           top: 10.0,
           left: 0.0,
           bottom: 0.0,
           right: 10.0
         })

       it "270 degree arc" do
         toBoundingBox (Arc (Point 0.0 0.0) (Point 10.0 0.0) (Point 0.0 10.0) false) `shouldEqual` (BoundingBox {
           top: 10.0,
           left: -10.0,
           bottom: -10.0,
           right: 10.0
         })

       it "inner arc 1" do
         toBoundingBox ai1 `shouldEqual` (BoundingBox {
           top: 8.0,
           left: 4.0,
           bottom: 2.0,
           right: 5.0
         })

       it "inner arc 2" do
         toBoundingBox ai2 `shouldEqual` (BoundingBox {
           top: 8.0,
           left: 3.0,
           bottom: 2.0,
           right: 4.0
         })

       it "outer arc 1" do
         toBoundingBox ao1 `shouldEqual` (BoundingBox {
           top: 10.0,
           left: -5.0,
           bottom: 0.0,
           right: 4.0
         })

       it "outer arc 2" do
         toBoundingBox ao2 `shouldEqual` (BoundingBox {
           top: 10.0,
           left: 4.0,
           bottom: 0.0,
           right: 13.0
         })

    it "cycle#size" do
      Cycle.size (Cycle.Cycle (ai1 : ai2 : Nil)) `shouldEqual` 12.0
      Cycle.size (Cycle.Cycle (ao1 : ao2 : Nil)) `shouldEqual` 180.0
