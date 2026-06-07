-- HTML *overlay* controls via anime.js (DOM only — never WebGL). This module
-- renders the data-driven molecule properties panel and wires the bond-formation
-- button + its anime.js animation. Like `Text`, it touches only the DOM and
-- imports only anime.js; it must never import or touch WebGL/GL.
module Controls
  ( renderInfoPanel
  , installBondButton
  , runBondAnimation
  ) where

import Prelude (Unit)
import Effect (Effect)

-- Populate the panel element (by id) with one row per property, then animate the
-- rows in via anime.js. Data-driven: renders whatever array is passed (never
-- hardcodes molecule text). No-op if the element is absent. DOM only.
foreign import renderInfoPanel
  :: String -> Array { label :: String, value :: String } -> Effect Unit

-- Wire a click on `#bond-btn` to run the given effect. No-op if absent.
foreign import installBondButton :: Effect Unit -> Effect Unit

-- Run an anime.js animation on a plain JS object `{ p: 0 }` from 0→1, calling
-- the PureScript callback with the current progress each onUpdate so the renderer
-- can consume it (the callback pushes progress into the FRP input ref). DOM only:
-- this animates a JS value, not WebGL.
foreign import runBondAnimation :: (Number -> Effect Unit) -> Effect Unit
