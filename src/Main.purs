module Main where

import Prelude

import App.Events (Event(..), foldp)
import App.State (State, init)
import App.View (view)
import Control.Monad.Eff (Eff)
import DOM (DOM)
import DOM.HTML (window)
import DOM.HTML.Types (HISTORY)
import KeyDown (sampleKeyDown)
import Pux (CoreEffects, App, start)
import Pux.DOM.Events (DOMEvent)
import Pux.Renderer.React (renderToDOM)
import SampleWindow (sampleWindowSize)
import Signal ((~>))

type WebApp = App (DOMEvent -> Event) Event State

type ClientEffects = CoreEffects (history :: HISTORY, dom :: DOM)

initialState :: State
initialState = init

main :: State -> Eff ClientEffects WebApp
main state = do
  -- | Create a signal of size changes.
  windowSizeSignal <- sampleWindowSize =<< window

  -- | Create a signal of key events
  keySignal <- sampleKeyDown =<< window

  let resizeEventSignal = windowSizeSignal ~> \{width, height} -> WindowResize width height
  let keyEventSignal = keySignal ~> \keyData -> Key keyData

  -- | Start the app.
  app <- start
    { initialState: state
    , view
    , foldp
    , inputs: [resizeEventSignal, keyEventSignal] }

  -- | Render to the DOM
  renderToDOM "#app" app.markup app.input
  pure app
