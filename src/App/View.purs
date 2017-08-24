module App.View where

import Prelude

import App.Background.View (fillBackground)
import App.Background.View (view) as BackgroundView
import App.ColorScheme (ColorScheme, toColor)
import App.Cycle (Cycle(..))
import App.Events (Event(..))
import App.Geometry (Point(Point), Stroke(Arc, Line), distance, firstPoint, ptX, ptY, secondPoint, sweep)
import App.Graph (edges)
import App.State (State, Tool(..))
import App.Tool.View (view) as ToolView
import CSS.Color (toHexString)
import CSS.Geometry (height, width)
import CSS.Size (px)
import Control.Monad.Except (runExcept)
import DOM.Event.MouseEvent (clientX, clientY, eventToMouseEvent)
import Data.Either (either)
import Data.Foldable (elem, for_)
import Data.Function.Uncurried (runFn2)
import Data.Int (toNumber)
import Data.List (List, foldl, (:))
import Data.Map (Map, toUnfoldable)
import Data.Maybe (Maybe(..))
import Data.Monoid (mempty)
import Data.Tuple (Tuple(..))
import Math (abs, pi)
import Pux.DOM.Events (DOMEvent, onClick, onMouseDown, onMouseMove, onMouseUp)
import Pux.DOM.HTML (HTML)
import Pux.DOM.HTML.Attributes (style)
import Text.Smolder.HTML (div)
import Text.Smolder.HTML.Attributes (className)
import Text.Smolder.Markup (Attribute, (!), (#!))
import Text.Smolder.SVG (circle, g, line, path, svg)
import Text.Smolder.SVG.Attributes (cx, cy, d, fill, r, stroke, strokeDasharray, x1, x2, y1, y2)

drawLine :: Attribute -> Point -> Point -> HTML Event
drawLine strokeStyle (Point px1 py1) (Point px2 py2) =
  line ! (x1 $ show px1)
       ! (y1 $ show py1)
       ! (x2 $ show px2)
       ! (y2 $ show py2)
       ! strokeStyle

drawStroke :: Attribute -> Stroke -> HTML Event
drawStroke strokeStyle s =
  let command = (mCmdString $ firstPoint s) <> (lCmdString s)
   in path ! strokeStyle ! (d command) $ pure unit

drawStrokes :: List Stroke -> HTML Event
drawStrokes strokes =
  let
      strokeStyle :: Attribute
      strokeStyle = stroke "black" <> fill "transparent" <> className "stroke"
   in g ! className "strokes" $ for_ strokes (drawStroke strokeStyle)

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

drawCycle :: Tool -> Tuple Cycle ColorScheme -> HTML Event
drawCycle tool (Tuple cycle@(Cycle strokes) colorScheme) =
  let
    handler = case tool of
                ColorTool newColorScheme -> onClick $ \_ -> Color cycle newColorScheme
                _ -> onClick (const $ NoOp)
  in path ! pathAttrs strokes ! stroke "black" ! (fill $ toHexString $ toColor colorScheme) #! handler $ pure unit

mapToList :: forall k v. Map k v -> List (Tuple k v)
mapToList = toUnfoldable

drawCycles :: Tool -> Map Cycle ColorScheme -> HTML Event
drawCycles tool cycles =
  g ! className "cycles" $ for_ (mapToList cycles) (drawCycle tool)

drawPoint :: Point -> Number -> HTML Event
drawPoint (Point x y) size =
  circle ! (cx $ show x)
         ! (cy $ show y)
         ! (r $ show size)

drawSnapPoint :: Maybe Point -> HTML Event
drawSnapPoint Nothing = g $ pure unit
drawSnapPoint (Just p) = drawPoint p 3.0

currentStrokeAttrs:: Attribute
currentStrokeAttrs = stroke "black" <> strokeDasharray "5 5" <> fill "transparent"

drawCurrentStroke :: Maybe Stroke -> HTML Event
drawCurrentStroke Nothing = g $ pure unit
drawCurrentStroke (Just s) =
  drawStroke currentStrokeAttrs s

readPageXY :: DOMEvent -> {clientX:: Number, clientY:: Number}
readPageXY ev =
  let readXY mouseEv = {clientX: toNumber $ clientX mouseEv, clientY: toNumber $ clientY mouseEv}
   in either (const {clientX: 0.0, clientY: 0.0}) readXY $ runExcept $ eventToMouseEvent ev

withListeners :: Tool -> HTML Event -> HTML Event
withListeners tool html =
  case tool of _ | elem tool [LineTool, ArcTool] -> html #! (onClick (readPageXY >>> \{clientX, clientY} -> Click (Point clientX clientY)))
                                                         #! (onMouseMove (readPageXY >>> \{clientX, clientY} -> Move (Point clientX clientY)))
               EraserTool -> html #! (onMouseDown $ readPageXY >>> \{clientX, clientY} -> EraserDown (Point clientX clientY))
                                  #! (onMouseUp $ readPageXY >>> \{clientX, clientY} -> EraserUp (Point clientX clientY))
                                  #! (onMouseMove $ readPageXY >>> \{clientX, clientY} -> EraserMove (Point clientX clientY))
               _ -> html

drawing :: State -> HTML Event
drawing state =
  withListeners state.tool $
  svg ! style do
          width $ (toNumber state.windowWidth) # px
          height $ (toNumber state.windowHeight) # px
      $ do
        fillBackground state.background state.windowWidth state.windowHeight
        drawStrokes (edges state.graph)
        drawCycles state.tool state.cycles
        drawSnapPoint state.snapPoint
        drawCurrentStroke state.currentStroke

view :: State -> HTML Event
view state =
  div $ do
    BackgroundView.view state.background
    ToolView.view state.tool
    drawing state
