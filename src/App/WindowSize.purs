module SampleWindow (sampleWindowSize) where

import Control.Alternative (pure)
import Control.Bind (bind)
import Control.Monad.Eff (Eff)
import DOM (DOM)
import DOM.Event.EventTarget (addEventListener, eventListener)
import DOM.HTML.Event.EventTypes (resize)
import DOM.HTML.Types (Window, windowToEventTarget)
import DOM.HTML.Window (innerHeight, innerWidth)
import Prelude (discard, ($))
import Signal (Signal)
import Signal.Channel (CHANNEL, channel, send, subscribe)

-- | Returns a signal containing the current window size
-- | which is updated on every "resize" event.
sampleWindowSize :: forall eff. Window -> Eff (channel :: CHANNEL, dom :: DOM | eff) (Signal {width:: Int, height:: Int})
sampleWindowSize win = do
  width <- innerWidth win
  height <- innerHeight win
  chan <- channel {width, height}

  let listener = eventListener \ev -> do
        w <- innerWidth win
        h <- innerHeight win
        send chan {width: w, height: h}

  addEventListener resize listener false (windowToEventTarget win)

  pure $ subscribe chan
