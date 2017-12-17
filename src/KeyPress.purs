module KeyPress where

import Control.Alternative (pure)
import Control.Bind (bind)
import Control.Monad.Eff (Eff)
import Control.Monad.Except (runExcept)
import DOM (DOM)
import DOM.Event.EventTarget (addEventListener, eventListener)
import DOM.Event.KeyboardEvent (altKey, code, ctrlKey, eventToKeyboardEvent, metaKey, shiftKey)
import DOM.HTML.Event.EventTypes (keypress)
import DOM.HTML.Types (Window, windowToEventTarget)
import Data.Either (either)
import Data.Maybe (Maybe(..))
import Prelude (const, discard, unit, ($), (<<<))
import Signal (Signal)
import Signal.Channel (CHANNEL, channel, send, subscribe)

type KeyData = {
  code :: String,
  shift :: Boolean,
  ctrl :: Boolean,
  alt :: Boolean,
  meta :: Boolean
}

-- | Returns a signal
sampleKeyPress :: forall eff. Window -> Eff (channel :: CHANNEL, dom :: DOM | eff) (Signal KeyData)
sampleKeyPress win = do
  chan <- channel {code: "init", shift: false, ctrl: false, alt: false, meta: false}
  let listener = eventListener \ev ->
       let readKeyEvt evt = {
             code: code evt,
             shift: shiftKey evt,
             ctrl: ctrlKey evt,
             alt: altKey evt,
             meta: metaKey evt
           }

           readKey = runExcept $ eventToKeyboardEvent ev
           key = either (const Nothing) (Just <<< readKeyEvt) readKey
        in case key of (Just keyData) -> send chan keyData
                       _ -> pure unit

  addEventListener keypress listener false (windowToEventTarget win)
  pure $ subscribe chan
