-- Per-atom atomic-SYMBOL HTML *overlay* labels for the Builder scene. DOM only —
-- this module must never import or touch WebGL/GL. It maintains a pool of
-- `<div class="atom-label">` nodes inside a `#atom-labels` container, one per
-- placed atom, positioned over the atom in CSS pixels. The renderer (Main draw
-- loop) projects each atom to screen pixels and feeds the positioned labels here.
module Labels
  ( AtomLabel
  , syncAtomLabels
  , clearAtomLabels
  , getCanvasClientSize
  ) where

import Prelude (Unit)
import Effect (Effect)
import Graphics.Canvas (CanvasElement)

-- One positioned label: a stable `id` (the placed-atom id, so a pooled div can
-- be reused/keyed across frames), the CSS-pixel screen position `x`/`y`, the
-- `text` (element symbol, e.g. "H"/"C"/"O"), and an `opacity` in [0,1].
type AtomLabel =
  { id :: Int
  , x :: Number
  , y :: Number
  , text :: String
  , opacity :: Number
  }

-- Reconcile the `#atom-labels` pool against the input: create a div for each new
-- id, update text/position/opacity for every input item, and remove any pooled
-- div whose id is no longer present. No-op (no crash) if `#atom-labels` is absent.
foreign import syncAtomLabels :: Array AtomLabel -> Effect Unit

-- Remove every `.atom-label` child from `#atom-labels` (called when leaving the
-- Builder scene). No-op if the container is absent.
foreign import clearAtomLabels :: Effect Unit

-- The canvas CLIENT size in CSS pixels (`clientWidth`/`clientHeight`), distinct
-- from the backing-store size (`getCanvasWidth`/`getCanvasHeight`). Used to map
-- backing-store projection pixels → CSS pixels so labels align with the rendered
-- atoms under any devicePixelRatio / canvas-scaling.
foreign import getCanvasClientSize
  :: CanvasElement -> Effect { w :: Number, h :: Number }
