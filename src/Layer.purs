-- | Pure CONTINUOUS level-of-detail (LOD) model for the zoom-driven
-- | atom-ball → sub-atomic-detail crossfade. Instead of discrete scale rungs,
-- | a single detail level in [0,1] is derived smoothly from the camera zoom:
-- | fully zoomed OUT renders cheap atomic "balls" (detail 0), fully zoomed IN
-- | reveals sub-atomic detail (detail 1), with a smooth Hermite crossfade in a
-- | zoom band (`detailLo`..`detailHi`). `easeDetail` gives one frame of
-- | exponential smoothing so the detail level eases toward its target rather
-- | than snapping.
-- |
-- | Pure / total / deterministic — imports only Prelude (no Effect / WebGL /
-- | Math.Matrix).
module Layer
  ( smoothstep
  , detailLo
  , detailHi
  , layerBlend
  , easeRate
  , easeDetail
  , detailName
  ) where

import Prelude

-- | Clamp a number into [lo, hi].
clampN :: Number -> Number -> Number -> Number
clampN lo hi x = max lo (min hi x)

-- | Clamped Hermite interpolation: `smoothstep edge0 edge1 x` returns 0 for
-- | x <= edge0, 1 for x >= edge1, and the smooth S-curve 3t²−2t³ in between
-- | (0.5 at the midpoint, monotonic non-decreasing).
smoothstep :: Number -> Number -> Number -> Number
smoothstep edge0 edge1 x =
  let
    t = clampN 0.0 1.0 ((x - edge0) / (edge1 - edge0))
  in
    t * t * (3.0 - 2.0 * t)

-- | Lower edge of the detail crossfade band (in zoom units). Strictly inside
-- | the camera zoom bounds: Camera.minZoom (0.2) < detailLo < detailHi <= 1.0.
detailLo :: Number
detailLo = 0.4

-- | Upper edge of the detail crossfade band (in zoom units).
detailHi :: Number
detailHi = 0.85

-- | Map a camera zoom factor to a detail level in [0,1]: 0 when fully zoomed
-- | out (atomic balls), 1 when zoomed in (sub-atomic detail). Monotonic
-- | non-decreasing, saturating at both ends (layerBlend 1.0 == 1.0 since
-- | 1.0 >= detailHi).
layerBlend :: Number -> Number
layerBlend zoom = smoothstep detailLo detailHi zoom

-- | Exponential-smoothing rate for `easeDetail`: the fraction of the remaining
-- | gap closed each frame. Small and positive ⇒ a smooth, convergent ease.
easeRate :: Number
easeRate = 0.15

-- | One frame of exponential smoothing of `current` toward `target`. A
-- | contraction with a fixed point at current == target; moves strictly toward
-- | the target without overshoot and stays in [0,1] for inputs in [0,1].
easeDetail :: Number -> Number -> Number
easeDetail current target =
  clampN 0.0 1.0 (current + (target - current) * easeRate)

-- | Human-readable name for a detail level, for any overlay/debug text.
detailName :: Number -> String
detailName d = if d < 0.5 then "atomic" else "sub-atomic"
