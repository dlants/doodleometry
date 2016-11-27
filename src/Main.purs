module Main where

import App.Model (Action, State, update, init)
import App.View (view)
import Control.Bind ((=<<))
import Control.Monad.Eff (Eff)
import DOM (DOM)
import Prelude (bind, pure)
import Pux (App, Config, CoreEffects, fromSimple, renderToDOM, start)
import Pux.Router (sampleUrl)

type AppEffects = (dom :: DOM)

-- | App configuration
config :: forall eff. State -> Eff (dom :: DOM | eff) (Config State Action AppEffects)
config state = do
  -- | Create a signal of URL changes.
  urlSignal <- sampleUrl

  pure
    { initialState: state
    , update: fromSimple update
    , view: view
    , inputs: [] }

-- | Entry point for the browser.
main :: Eff (CoreEffects AppEffects) (App State Action)
main = do
  app <- start =<< config init
  renderToDOM "#app" app.html
  -- | Used by hot-reloading code in support/index.js
  pure app
