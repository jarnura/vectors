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
  , projection
  , clampZoom
  , applyZoomStep
  ) where

import Prelude

import Data.Maybe (fromMaybe)
import Data.Number (exp, pi, tan)
import Math.Matrix (Matrix)
import Math.Matrix as M

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

-- Zoom bounds: closest-out (0.2 ⇒ camera 5× farther) and closest-in (5.0 ⇒
-- camera 5× nearer). Clamping keeps the camera from flipping through the origin.
minZoom :: Number
minZoom = 0.2

maxZoom :: Number
maxZoom = 5.0

-- How aggressively a wheel delta translates into a zoom factor. Small and
-- positive: a typical wheel notch (~±100) changes zoom by exp(±0.15) ≈ ±16%.
zoomSensitivity :: Number
zoomSensitivity = 0.0015

-- | The perspective×camera projection at a given zoom and canvas size. At
-- | zoom 1.0 the camera translation is `-cameraDistance` (byte-identical to the
-- | historic projection); at zoom 2.0 it is `-cameraDistance / 2`.
projection :: Number -> Number -> Number -> Matrix Number
projection zoom w h =
  let
    aspect = w / h
    f = 1.0 / tan (fov / 2.0)
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
      , (clipFar + clipNear) / (clipNear - clipFar)
      , (2.0 * clipFar * clipNear) / (clipNear - clipFar)
      , 0.0
      , 0.0
      , -1.0
      , 0.0
      ]
  in
    M.multiply p (M.translate 0.0 0.0 (negate (cameraDistance / zoom)))

-- | Clamp a zoom factor into the supported [minZoom, maxZoom] range.
clampZoom :: Number -> Number
clampZoom z = max minZoom (min maxZoom z)

-- | Apply one mouse-wheel step to the current zoom, multiplicatively and
-- | clamped. NEGATIVE deltaY (wheel up) → exp(positive) → larger zoom (zoom in);
-- | POSITIVE deltaY (wheel down) → smaller zoom (zoom out).
applyZoomStep :: Number -> Number -> Number
applyZoomStep currentZoom wheelDeltaY =
  clampZoom (currentZoom * exp (negate (wheelDeltaY * zoomSensitivity)))
