module Main where

import Prelude
import App.Events (AppEffects, Event(..), foldp)
import App.State (State, init)
import App.View (view)
import Control.Monad.Eff (Eff)
import DOM (DOM)
import DOM.HTML (window)
import DOM.HTML.Types (HISTORY)
import Pux (CoreEffects, App, start)
import Pux.DOM.Events (DOMEvent)
import Pux.Renderer.React (renderToDOM)
import Signal ((~>))
import SampleWindow (sampleWindowSize)

type WebApp = App (DOMEvent -> Event) Event State

type ClientEffects = CoreEffects (AppEffects (history :: HISTORY, dom :: DOM))

main :: Eff ClientEffects WebApp
main = do
  -- | Create a signal of size changes.
  windowSizeSignal <- sampleWindowSize =<< window

  -- | Map a signal of URL changes to PageView actions.
  let resizeSignal = windowSizeSignal ~> \{width, height} -> WindowResize width height

  -- | Start the app.
  app <- start
    { initialState: init
    , view
    , foldp
    , inputs: [resizeSignal] }

  -- | Render to the DOM
  renderToDOM "#app" app.markup app.input
  pure app
