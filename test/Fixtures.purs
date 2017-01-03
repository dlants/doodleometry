module Test.Fixtures where

import Prelude
import App.Cycle
import App.Geometry
import App.ColorScheme (ColorScheme(..))
import App.Graph (addStroke, addStrokes, emptyGraph)
import Data.List (List(..), (:))
import Data.Map (empty, insert)

p1 = Point 0.0 0.0
p2 = Point 1.0 0.0
p3 = Point 1.0 1.0
p4 = Point 0.0 1.0

l12 = Line p1 p2
l23 = Line p2 p3
l34 = Line p3 p4
l41 = Line p4 p1

l21 = Line p2 p1
l32 = Line p3 p2
l43 = Line p4 p3
l14 = Line p1 p4

l31 = Line p3 p1
l13 = Line p1 p3
l42 = Line p4 p2
l24 = Line p2 p4

c1234 = Cycle $ l12 : l23 : l34 : l41 : Nil
c123 = Cycle $ l12 : l23 : l31 : Nil
c134 = Cycle $ l13 : l34 : l41 : Nil

g = addStroke l12
  $ addStroke l23
  $ emptyGraph

g2 = addStroke l31 g

g3 = addStroke l12
   $ addStroke l23
   $ addStroke l34
   $ addStroke l41
   $ addStroke l13
   $ emptyGraph

g4 = addStrokes (
    Line (Point 0.0 0.0) (Point 1.0 0.0) :
    Line (Point 1.0 0.0) (Point 0.5 0.5) :
    Line (Point 0.5 0.5) (Point 0.0 1.0) :
    Line (Point 0.0 1.0) (Point 1.0 1.0) :
    Line (Point 1.0 1.0) (Point 0.5 0.5) :
    Line (Point 0.5 0.5) (Point 0.0 0.0) :
    Nil
  ) emptyGraph

cycles = insert c1234 Red $ empty
