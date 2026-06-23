-- HTML *overlay* controls via anime.js (DOM only — never WebGL). This module
-- renders the data-driven molecule properties panel and wires the bond-formation
-- button + its anime.js animation. Like `Text`, it touches only the DOM and
-- imports only anime.js; it must never import or touch WebGL/GL.
module Controls
  ( renderInfoPanel
  , installBondButton
  , runBondAnimation
  , animateControlBarIn
  , installPanelToggle
  , installButtonPulse
  , showNuclidePanel
  , showNuclideSectionInDrawer
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

-- Animate the control bar (by element id) IN on load: the panel fades + slides
-- up (opacity 0→1, translateY) and its child buttons stagger in. The panel's
-- initial CSS state is opacity 0, so an early sampled frame reads < 1 mid-flight
-- and settles to ~1. No-op if the element is absent. DOM only — never WebGL.
foreign import animateControlBarIn :: String -> Effect Unit

-- Wire the left-drawer toggle: clicking the icon (first id) slides the panel
-- (second id) in from the left and out again via anime.js (translateX/opacity).
-- The closed drawer keeps pointer-events:none so it never traps canvas events.
-- No-op if either element is absent. DOM only — never WebGL.
foreign import installPanelToggle :: String -> String -> Effect Unit

-- Wire a tasteful click "pulse" (a quick scale bounce) onto a button by id, as
-- add/clear feedback. No-op if absent. DOM only.
foreign import installButtonPulse :: String -> Effect Unit

-- Show or hide the `#nuclide-info` overlay panel. Pass `true` in the Nuclide
-- scene, `false` elsewhere. Mirrors showMaterialsPanel in shape. DOM only.
foreign import showNuclidePanel :: Boolean -> Effect Unit

-- Show or hide the `#nuclide-controls` section inside the left drawer. Pass
-- `true` to show in the Nuclide scene, `false` elsewhere. DOM only.
foreign import showNuclideSectionInDrawer :: Boolean -> Effect Unit
