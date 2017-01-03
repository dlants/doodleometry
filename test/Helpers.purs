module Test.Helpers where

import Prelude
import App.Helpers (rotateList, rotatePast)
import Data.List (List(..), singleton, (:))
import Test.Spec (describe, it)
import Test.Spec.Assertions (shouldEqual)

spec = do
  describe "App.Helpers" do
    describe "rotateList" do
      it "rotates correctly" do
        rotateList 3 (1 : 2 : 3 : Nil) `shouldEqual` (3 : 1 : 2 : Nil)

      it "rotates a single element" do
        rotateList 3 (3 : Nil) `shouldEqual` (3 : Nil)

      it "does nothing when no element" do
        rotateList 4 (1 : 2 : 3 : Nil) `shouldEqual` (1 : 2 : 3 : Nil)

    describe "rotatePast" do
      it "rotates correctly" do
        rotatePast 2 (1 : 2 : 3 : Nil) `shouldEqual` (3 : 1 : 2 : Nil)

      it "rotates a single element" do
        rotatePast 3 (3 : Nil) `shouldEqual` (3 : Nil)

      it "does nothing when no element" do
        rotatePast 4 (1 : 2 : 3 : Nil) `shouldEqual` (1 : 2 : 3 : Nil)
