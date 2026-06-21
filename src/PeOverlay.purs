-- Builder-scene potential-energy (Morse) curve HTML/SVG *overlay*. DOM only —
-- this module must never import or touch WebGL/GL. It draws an inline SVG into
-- the `#pe-overlay` container: a polyline of the sampled curve E(r) (auto-scaled
-- to the panel, with the energy axis inverted so the well dips DOWN), light axis
-- hints, and a small dot per live bond at its current (r, e). Render-only: it
-- consumes the pure `Pe.peCurveSamples` / `Pe.bondCurveMarkers` seam and never
-- mutates the builder model.
module PeOverlay
  ( PeCurve
  , renderPeCurve
  , setPeOverlayVisible
  ) where

import Prelude (Unit)
import Effect (Effect)

-- The render input: the sampled curve (x = r, y = e) and the live bond markers
-- (each a current {r, e} dot on the curve). Both already computed by the caller
-- via the pure Pe seam, so this FFI stays free of any chemistry model.
type PeCurve =
  { samples :: Array { r :: Number, e :: Number }
  , markers :: Array { r :: Number, e :: Number }
  }

-- Draw (replace) the inline SVG inside `#pe-overlay` from the curve + markers.
-- Auto-scales to the panel using the data's own r/e extents. No-op (no crash)
-- if `#pe-overlay` is absent (mirrors the Labels FFI robustness).
foreign import renderPeCurve :: PeCurve -> Effect Unit

-- Toggle the `#pe-overlay` panel display (true → shown, false → hidden). No-op
-- if the container is absent.
foreign import setPeOverlayVisible :: Boolean -> Effect Unit
