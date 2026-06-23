-- | Pure, zoom-aware camera projection. This is the single source of truth for
-- | the camera constants (fov / cameraDistance / clip planes) and the
-- | perspective×camera projection matrix used by the renderer.
-- |
-- | The projection mirrors the classic OpenGL perspective matrix composed with a
-- | camera translation along -Z. Zoom is a multiplicative factor on the camera:
-- | the effective camera distance is `cameraDistance / zoom`, so zoom 1.0 is the
-- | unchanged default (camera `cameraDistance` back), zoom 2.0 halves it (closer,
-- | "zoomed in"), and so on. Pure / total / deterministic — no Effect/WebGL.
module Camera
  ( fov
  , cameraDistance
  , clipNear
  , clipFar
  , minZoom
  , maxZoom
  , maxPitch
  , clampPitch
  , projection
  , orbit
  , viewProjection
  , clampZoom
  , applyZoomStep
  , buttonZoomDelta
  , buttonOrbitDelta
  ) where

import Prelude

import Data.Maybe (fromMaybe)
import Data.Number (exp, pi, tan)
import Math.Matrix (Matrix)
import Math.Matrix as M
import Vector as Vector

-- Vertical field of view, in radians.
fov :: Number
fov = pi / 3.0

-- Distance from the camera to the world origin along -Z (at zoom 1.0).
cameraDistance :: Number
cameraDistance = 1000.0

clipNear :: Number
clipNear = 1.0

clipFar :: Number
clipFar = 2000.0

-- Zoom bounds: closest-out (0.05 ⇒ camera 20× farther, ~4× more zoom-out than
-- the old 0.2 floor) and closest-in (5.0 ⇒ camera 5× nearer). The wider
-- zoom-out range lets Builder/Materials pull back far enough to see many atoms
-- (or an entire crystal supercell) in a single view. The clip planes scale with
-- zoom (near=clipNear/zoom, far=clipFar/zoom) so the frustum auto-brackets the
-- scene at any zoom level — no culling occurs. Clamping keeps the camera from
-- flipping through the origin. HARD CONSTRAINT: minZoom must stay strictly
-- below Layer.detailLo (0.10) so layerBlend(minZoom) == 0.0 (fully zoomed out
-- = cheap atom-ball layer = exactly the "many atoms" view).
minZoom :: Number
minZoom = 0.05

maxZoom :: Number
maxZoom = 5.0

-- | Maximum Builder orbit pitch, in radians (≈ 85°). Clamping the pitch just shy
-- | of the poles keeps the orbit from flipping over the top/bottom of the scene.
maxPitch :: Number
maxPitch = 1.4835298641951802

-- | Clamp an orbit pitch (radians) into the supported [-maxPitch, maxPitch] range.
clampPitch :: Number -> Number
clampPitch p = max (negate maxPitch) (min maxPitch p)

-- How aggressively a wheel delta translates into a zoom factor. Small and
-- positive: a typical wheel notch (~±100) changes zoom by exp(±0.15) ≈ ±16%.
zoomSensitivity :: Number
zoomSensitivity = 0.0015

-- | A fixed positive synthetic wheel-delta magnitude for one `applyZoomStep`.
-- | The on-screen zoom buttons were replaced by the #zoom-slider (absolute set)
-- | in M2, so this no longer has a production consumer; it is retained as the
-- | documented per-step zoom magnitude (the reference the orbit-button step
-- | below mirrors) and is exercised by CameraLayerSpec. One step ≈
-- | exp(zoomSensitivity * 120) = exp(0.18) ≈ 1.197, i.e. ~+19.7% zoom-in /
-- | ~−16.5% zoom-out — the same order as one wheel notch (~100). Pure; no Effect.
buttonZoomDelta :: Number
buttonZoomDelta = 120.0

-- | A fixed synthetic input-delta magnitude per on-screen orbit-button click.
-- | The orbit buttons push ±`buttonOrbitDelta` through `Main.applyOrbit`
-- | (mirroring how `buttonZoomDelta = 120.0` is pushed through the zoom path).
-- | With `orbitSens = 0.01` one click yields `17.45 * 0.01 = 0.1745 rad ≈ 10°`
-- | of yaw/pitch per step. Pure; no Effect.
buttonOrbitDelta :: Number
buttonOrbitDelta = 17.45

-- | The perspective×camera projection at a given zoom and canvas size. At
-- | zoom 1.0 the camera translation is `-cameraDistance` (byte-identical to the
-- | historic projection); at zoom 2.0 it is `-cameraDistance / 2`.
projection :: Number -> Number -> Number -> Matrix Number
projection zoom w h =
  let
    aspect = w / h
    f = 1.0 / tan (fov / 2.0)
    -- Scale the clip planes with the effective camera distance so the frustum
    -- always brackets the scene. Without this, zooming OUT pulls the camera
    -- back (effective distance cameraDistance / zoom) while the far plane stays
    -- fixed, so past zoom ≈ 0.5 the world crosses the far plane and is culled
    -- (everything disappears). Dividing both planes by zoom keeps the framing
    -- proportional and is byte-identical at zoom 1.0 (near 1, far 2000).
    near = clipNear / zoom
    far = clipFar / zoom
    p = fromMaybe (M.zeros 4 4) $ M.fromArray 4 4
      [ f / aspect
      , 0.0
      , 0.0
      , 0.0
      , 0.0
      , f
      , 0.0
      , 0.0
      , 0.0
      , 0.0
      , (far + near) / (near - far)
      , (2.0 * far * near) / (near - far)
      , 0.0
      , 0.0
      , -1.0
      , 0.0
      ]
  in
    M.multiply p (M.translate 0.0 0.0 (negate (cameraDistance / zoom)))

-- | The Builder orbit (view) rotation: yaw about Y composed with pitch about X.
-- | `yaw`/`pitch` are in RADIANS (the camera-orbit contract), converted to the
-- | degrees that `Vector.rotateY`/`rotateX` expect. At yaw = pitch = 0 both
-- | factors are the exact 4x4 identity (cos 0 = 1, sin 0 = 0), so `orbit 0 0`
-- | is byte-identical to `M.identity` and `viewProjection {0,0}` collapses to
-- | plain `projection`. Pure / total — no Effect/WebGL.
orbit :: Number -> Number -> Matrix Number
orbit yaw pitch =
  Vector.rotateY (toDeg yaw) `M.multiply` Vector.rotateX (toDeg pitch)
  where
  toDeg r = r * 180.0 / pi

-- | The Builder view×projection: the zoom-aware perspective projection composed
-- | with the orbit rotation. MUST be byte-identical to `projection zoom w h`
-- | when yaw = pitch = 0 (because `orbit 0 0 == identity` and `M * I == M`).
viewProjection
  :: { yaw :: Number, pitch :: Number } -> Number -> Number -> Number -> Matrix Number
viewProjection cam zoom w h =
  projection zoom w h `M.multiply` orbit cam.yaw cam.pitch

-- | Clamp a zoom factor into the supported [minZoom, maxZoom] range.
clampZoom :: Number -> Number
clampZoom z = max minZoom (min maxZoom z)

-- | Apply one mouse-wheel step to the current zoom, multiplicatively and
-- | clamped. NEGATIVE deltaY (wheel up) → exp(positive) → larger zoom (zoom in);
-- | POSITIVE deltaY (wheel down) → smaller zoom (zoom out).
applyZoomStep :: Number -> Number -> Number
applyZoomStep currentZoom wheelDeltaY =
  clampZoom (currentZoom * exp (negate (wheelDeltaY * zoomSensitivity)))
