module Mouse where

import Prelude

import App.Geometry (Point(..))
import Control.Monad.Eff (Eff)
import Control.Monad.Except (runExcept)
import DOM (DOM)
import DOM.Event.Event (Event)
import DOM.Event.EventTarget (addEventListener, eventListener)
import DOM.Event.MouseEvent (clientX, clientY, eventToMouseEvent)
import DOM.Event.WheelEvent (deltaY, eventToWheelEvent)
import DOM.HTML.Event.EventTypes (mousedown, mousemove, mouseup, wheel)
import DOM.HTML.Types (Window, windowToEventTarget)
import Data.Either (Either(Right, Left))
import Data.Int (toNumber)
import Signal (Signal)
import Signal.Channel (CHANNEL, channel, send, subscribe)

data MouseData = MouseUp Point | MouseDown Point | MouseMove Point | Wheel Number

evtToPoint :: Event -> Point
evtToPoint evt =
  let res = do
        mouseEv <- runExcept $ eventToMouseEvent evt
        let x = toNumber $ clientX mouseEv
        let y = toNumber $ clientY mouseEv
        pure $ Point x y
   in case res of (Left _ ) -> Point 0.0 0.0
                  (Right pt) -> pt

-- | Returns a signal that fires off MouseData on window
sampleMouse :: forall eff. Window -> Eff (channel :: CHANNEL, dom :: DOM | eff) (Signal MouseData)
sampleMouse win = do
  chan <- channel $ MouseMove (Point 0.0 0.0)
  let listener constructor = eventListener \evt -> send chan $ constructor $ evtToPoint evt

  let wheelListener = eventListener \evt ->
       let res = do
            wheelEv <- runExcept $ eventToWheelEvent evt
            let delta = deltaY wheelEv
            pure $ delta
       in case res of (Left _) -> pure unit
                      (Right delta) -> send chan (Wheel delta)

  addEventListener wheel wheelListener false (windowToEventTarget win)

  pure $ subscribe chan
