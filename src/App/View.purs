module App.View where

import Prelude

import App.Background.View (fillBackground)
import App.Background.View (view) as BackgroundView
import App.Cycle (Cycle(..), compareSize)
import App.Events (Event(..))
import App.Geometry (Point(Point), Stroke(Arc, Line), distance, firstPoint, ptX, ptY, secondPoint, sweep)
import App.Graph (Graph, edges)
import App.State (State, Tool(..))
import App.Tool.View (view) as ToolView
import CSS (absolute, bottom, left, position, right, top)
import CSS.Color (Color, toHexString)
import CSS.Geometry (height, width)
import CSS.Overflow (hidden, overflow)
import CSS.Size (px)
import Data.Foldable (for_)
import Data.Function (on)
import Data.Function.Uncurried (runFn2)
import Data.Int (toNumber)
import Data.List (List, foldl, reverse, sortBy, (:))
import Data.Map (Map, toUnfoldable)
import Data.Maybe (Maybe(..))
import Data.Monoid (mempty)
import Data.Tuple (Tuple(..), fst)
import Math (abs, pi)
import Pux.DOM.Events (onClick)
import Pux.DOM.HTML (HTML, memoize)
import Pux.DOM.HTML.Attributes (style)
import Text.Smolder.HTML (div)
import Text.Smolder.HTML.Attributes (className)
import Text.Smolder.Markup (Attribute, attribute, (!), (#!))
import Text.Smolder.SVG (circle, g, line, path, svg)
import Text.Smolder.SVG.Attributes (cx, cy, d, fill, r, stroke, x1, x2, y1, y2)

drawLine :: Attribute -> Point -> Point -> HTML Event
drawLine strokeStyle (Point px1 py1) (Point px2 py2) =
  line ! (x1 $ show px1)
       ! (y1 $ show py1)
       ! (x2 $ show px2)
       ! (y2 $ show py2)
       ! strokeStyle

drawStroke :: Stroke -> HTML Event
drawStroke s =
  let command = (mCmdString $ firstPoint s) <> (lCmdString s)
   in path ! (d command) $ pure unit

drawStrokes :: Graph -> HTML Event
drawStrokes = memoize \graph ->
  let
      strokeStyle :: Attribute
      strokeStyle = stroke "black" <> fill "transparent" <> className "stroke"
   in g ! className "strokes" $ for_ (edges graph) \stroke -> drawStroke stroke ! strokeStyle

pathAttrs :: List Stroke -> Attribute
pathAttrs strokes@(s1 : _) =
  let p = firstPoint s1
   in d (foldl append (mCmdString p) (lCmdString <$> strokes))

pathAttrs _ = mempty

mCmdString :: Point -> String
mCmdString (Point x y) = "M " <> (show $ x) <> " " <> (show $ y) <> " "

lCmdString :: Stroke -> String
lCmdString (Line _ (Point x2 y2)) = "L " <> show x2 <> " " <> show y2 <> " "
lCmdString arc@(Arc c p q ccw) =
  if p == q then let halfPoint = c + c - p
                  in arcCmdString (Arc c p halfPoint ccw) <> arcCmdString (Arc c halfPoint q ccw)
            else arcCmdString arc

arcCmdString :: Stroke -> String
arcCmdString arc@(Arc c p q ccw) =
  let endPoint = secondPoint arc
      r = runFn2 distance c p
      s = sweep arc
   in "A "
      <> show r <> " " -- rx
      <> show r -- ry
      <> " 0 " -- x-axis-rotation
      <> show (if abs s > pi then 1 else 0) -- large sweep flag (sweep > 180 degrees)
      <> " "
      <> show (if ccw then 1 else 0) -- sweep flag (sweep starts positive or negative)
      <> " "
      <> show (ptX endPoint) <> " "
      <> show (ptY endPoint) <> " "

arcCmdString _ = ""

drawCycle :: Cycle -> HTML Event
drawCycle cycle@(Cycle strokes) =
  path ! pathAttrs strokes $ pure unit

drawCycles :: Tool -> Map Cycle Color -> HTML Event
drawCycles tool cycles =
  let handler cycle = case tool of ColorTool newColor -> onClick $ \_ -> ApplyColor cycle newColor
                                   SelectTool -> onClick $ \_ -> SelectCycle cycle
                                   _ -> onClick (const $ NoOp)
      draw cycle color = (drawCycle cycle) ! stroke "black" ! (fill $ toHexString color)
   in g ! className "cycles" $ for_ (reverse $ sortBy (compareSize `on` fst) $ toUnfoldable cycles) $ \(Tuple cycle color) -> (draw cycle color) #! handler cycle

drawPoint :: Point -> Number -> HTML Event
drawPoint (Point x y) size =
  circle ! (cx $ show x)
         ! (cy $ show y)
         ! (r $ show size)

drawSnapPoint :: Maybe Point -> HTML Event
drawSnapPoint Nothing = g $ pure unit
drawSnapPoint (Just p) = drawPoint p 3.0

drawCurrentStroke :: Maybe Stroke -> HTML Event
drawCurrentStroke Nothing = g $ pure unit
drawCurrentStroke (Just s) =
  drawStroke s ! stroke "gray" ! attribute "strokeDasharray" "5 5" ! fill "transparent"

drawSelection :: List Cycle -> HTML Event
drawSelection cycles =
  g ! className "selected" $ for_ cycles \cycle -> drawCycle cycle ! stroke "red" ! fill "transparent"

drawTool :: Tool -> HTML Event
drawTool (EraserTool {pt, size}) =
  drawStroke (Arc pt (pt - (Point size 0.0)) (pt - (Point size 0.0)) true) ! stroke "black" ! fill "transparent"
drawTool _ = pure unit

drawing :: State -> HTML Event
drawing state =
  svg ! style do
          width $ (toNumber state.windowWidth) # px
          height $ (toNumber state.windowHeight) # px
      $ do
        drawCycles state.tool state.drawing.cycles
        fillBackground state.background state.windowWidth state.windowHeight
        drawStrokes state.drawing.graph
        drawSnapPoint state.snapPoint
        drawCurrentStroke state.currentStroke
        drawSelection state.selection
        drawTool state.tool

view :: State -> HTML Event
view state =
  div ! style do
          position absolute
          top $ 0.0 # px
          left $ 0.0 # px
          bottom $ 0.0 # px
          right $ 0.0 # px
          overflow hidden
     $ do
      drawing state
      BackgroundView.view state.background
      ToolView.view state.tool
