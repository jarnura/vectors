-- The Builder camera-orbit seam for `window.__builder` (setOrbit / getOrbit).
-- Camera orbit (yaw/pitch in radians) is camera state owned by `Main`, NOT part
-- of the pure `Builder`/`BuilderState` model — so it lives in a small shared Ref
-- in Main that the renderer, the pick/orbit-drag, and this API all read/write. By
-- keeping it here (a sibling to `BuilderApi`) the pure model stays free of camera
-- concerns while the E2E seam still hangs off the same `window.__builder` object.
--
-- The FFI (`OrbitApi.js`) is pure DOM/JS — it never touches WebGL.
module OrbitApi
  ( Orbit
  , installOrbitApi
  ) where

import Prelude

import Camera as Camera
import Effect (Effect)
import Effect.Ref as Ref

-- Builder orbit angles (radians). Pitch is clamped to Camera.maxPitch on write.
type Orbit = { yaw :: Number, pitch :: Number }

-- The bridge handed to the FFI: a clamped setter + a reader over the shared
-- orbit Ref. setOrbit clamps pitch (so the seam can never drive the camera past
-- the poles); getOrbit reads the live angles back.
type Bridge =
  { setOrbit :: Number -> Number -> Effect Unit
  , getOrbit :: Effect Orbit
  }

-- Extend `window.__builder` with setOrbit/getOrbit reading/writing the SAME
-- orbit Ref the renderer and the orbit-drag share (single source of truth). The
-- Ref is created in Main and read into State each frame, mirroring how the
-- builder model Ref is mirrored into State.
installOrbitApi :: Ref.Ref Orbit -> Effect Unit
installOrbitApi ref = do
  let
    bridge :: Bridge
    bridge =
      { setOrbit: \yaw pitch ->
          Ref.write { yaw, pitch: Camera.clampPitch pitch } ref
      , getOrbit:
          Ref.read ref
      }
  installWindowOrbit bridge

-- DOM-only FFI: stash the bridge's effectful closures onto `window.__builder`.
foreign import installWindowOrbit :: Bridge -> Effect Unit
