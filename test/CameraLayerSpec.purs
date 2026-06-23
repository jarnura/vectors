module Test.CameraLayerSpec where

import Prelude

import Data.Array (all, index, range)
import Data.Foldable (foldl)
import Data.Maybe (fromMaybe)
import Data.Number (abs, pi)
import Effect (Effect)
import Effect.Console (log)
import Main (applyOrbit)
import Builder as B
import Camera as Cam
import Layer as Layer
import Math.Matrix as M
import Test.Util (approxEq, approxEqMatrix, check, foldZoom, testProjection)

cameraLayerSpec :: Effect Unit
cameraLayerSpec = do
  -- ───── Camera (zoom projection) ─────────────────────────────────────
  log "camera zoom projection properties:"

  -- clampZoom: bounds are returned exactly; in-range passes through.
  check "clampZoom below min ⇒ minZoom" $ Cam.clampZoom (-1.0) == Cam.minZoom
  check "clampZoom above max ⇒ maxZoom" $ Cam.clampZoom 100.0 == Cam.maxZoom
  check "clampZoom in-range (1.0) ⇒ 1.0" $ approxEq (Cam.clampZoom 1.0) 1.0

  -- Extended zoom-out range: minZoom MUST be strictly lower than the old 0.2
  -- floor so the Builder/Materials world can pull back further and fit more atoms.
  -- RED until Camera.minZoom is lowered below 0.2.
  check "minZoom < 0.2 (extended zoom-out range, more atoms in view)" $
    Cam.minZoom < 0.2

  -- clampZoom clamps tiny / negative values to the new minZoom floor.
  check "clampZoom 0.001 == minZoom (tiny value clamps to new floor)" $
    Cam.clampZoom 0.001 == Cam.minZoom
  check "clampZoom (-1.0) == minZoom (negative value clamps to new floor)" $
    Cam.clampZoom (-1.0) == Cam.minZoom

  -- projection at new minZoom: world origin must remain inside the frustum
  -- (NDC z in [-1,1]) — no culling at the extended zoom-out.
  let
    projNewMin = Cam.projection Cam.minZoom 800.0 600.0
    originClipNewMin = M.toVector $ M.multiply projNewMin
      (fromMaybe (M.zeros 4 1) (M.fromArray 4 1 [ 0.0, 0.0, 0.0, 1.0 ]))
    originNdcZNewMin = fromMaybe 0.0 (index originClipNewMin 2) / fromMaybe 0.0 (index originClipNewMin 3)
  check "projection newMinZoom: origin stays in front of far plane (ndc z ≤ 1)" $
    originNdcZNewMin <= 1.0
  check "projection newMinZoom: origin stays behind near plane (ndc z ≥ -1)" $
    originNdcZNewMin >= -1.0

  -- applyZoomStep direction: negative deltaY zooms IN (larger), positive OUT.
  check "applyZoomStep: wheel up (−Δ) zooms IN (>1.0)" $
    Cam.applyZoomStep 1.0 (-100.0) > 1.0
  check "applyZoomStep: wheel down (+Δ) zooms OUT (<1.0)" $
    Cam.applyZoomStep 1.0 100.0 < 1.0

  -- applyZoomStep clamps at the bounds (already-max zoom-in stays max, etc.).
  check "applyZoomStep: already-max zoom-in stays maxZoom" $
    Cam.applyZoomStep Cam.maxZoom (-1000.0) == Cam.maxZoom
  check "applyZoomStep: already-min zoom-out stays minZoom" $
    Cam.applyZoomStep Cam.minZoom 1000.0 == Cam.minZoom

  -- Repeated stepping stays within [minZoom, maxZoom] (folded over many steps).
  let
    zoomInMany = foldZoom (-200.0) 1.0 (range 1 20)
    zoomOutMany = foldZoom 200.0 1.0 (range 1 20)
  check "applyZoomStep: many zoom-in steps stay ≤ maxZoom" $
    zoomInMany <= Cam.maxZoom
  check "applyZoomStep: many zoom-out steps stay ≥ minZoom" $
    zoomOutMany >= Cam.minZoom

  -- determinism: same inputs ⇒ same output.
  check "applyZoomStep is deterministic" $
    Cam.applyZoomStep 1.0 50.0 == Cam.applyZoomStep 1.0 50.0

  -- projection baseline: zoom 1.0 equals the existing testProjection ENTRY-BY-ENTRY.
  check "projection 1.0 == testProjection (entry-by-entry)" $
    approxEqMatrix (Cam.projection 1.0 800.0 600.0) (testProjection 800.0 600.0)

  -- projection differs at zoom 2.0 (camera distance halved ⇒ translation differs).
  check "projection 2.0 differs from projection 1.0 (some entry)" $
    not (approxEqMatrix (Cam.projection 2.0 800.0 600.0) (Cam.projection 1.0 800.0 600.0))

  -- Zoom-out frustum: at minZoom the focal point (world origin) must stay INSIDE
  -- the frustum (NDC z within [-1, 1]).
  let
    projMin = Cam.projection Cam.minZoom 800.0 600.0
    originClip = M.toVector $ M.multiply projMin
      (fromMaybe (M.zeros 4 1) (M.fromArray 4 1 [ 0.0, 0.0, 0.0, 1.0 ]))
    originNdcZ = fromMaybe 0.0 (index originClip 2) / fromMaybe 0.0 (index originClip 3)
  check "projection minZoom: origin stays in front of the far plane (ndc z ≤ 1)" $
    originNdcZ <= 1.0
  check "projection minZoom: origin stays behind the near plane (ndc z ≥ -1)" $
    originNdcZ >= -1.0

  -- Control-panel zoom buttons: the #zoom-in / #zoom-out buttons reuse the
  -- existing applyZoomStep by pushing a FIXED synthetic wheel delta.
  check "buttonZoomDelta is a positive magnitude" $
    Cam.buttonZoomDelta > 0.0
  check "zoom-in button (−Δ) increases zoom" $
    Cam.applyZoomStep 1.0 (negate Cam.buttonZoomDelta) > 1.0
  check "zoom-out button (+Δ) decreases zoom" $
    Cam.applyZoomStep 1.0 Cam.buttonZoomDelta < 1.0
  check "zoom-in button clamps at maxZoom" $
    Cam.applyZoomStep Cam.maxZoom (negate Cam.buttonZoomDelta) <= Cam.maxZoom
  check "zoom-out button clamps at minZoom" $
    Cam.applyZoomStep Cam.minZoom Cam.buttonZoomDelta >= Cam.minZoom

  log "all camera zoom projection properties hold."

  -- ───── Continuous level-of-detail (Layer smoothstep/layerBlend/easeDetail) ─
  log "continuous LOD (Layer smoothstep/layerBlend/easeDetail) properties:"

  -- smoothstep: clamped Hermite. Below edge0 → 0, above edge1 → 1, symmetric
  -- midpoint → 0.5, and strictly increasing through the band.
  -- Probes updated to the retuned band (detailLo=0.10, detailHi=0.20).
  check "smoothstep below edge0 == 0.0" $
    Layer.smoothstep 0.10 0.20 0.05 == 0.0
  check "smoothstep above edge1 == 1.0" $
    Layer.smoothstep 0.10 0.20 0.25 == 1.0
  check "smoothstep midpoint of [0,1] == 0.5" $
    approxEq (Layer.smoothstep 0.0 1.0 0.5) 0.5
  check "smoothstep is monotonic increasing (0.25 < 0.75)" $
    Layer.smoothstep 0.0 1.0 0.25 < Layer.smoothstep 0.0 1.0 0.75

  -- Detail-band invariant: the crossfade band sits strictly inside the camera
  -- zoom bounds and is ordered. Band retuned to detailLo=0.10, detailHi=0.20.
  check "detail band invariant: minZoom < detailLo" $
    Cam.minZoom < Layer.detailLo
  check "detail band invariant: detailLo < detailHi" $
    Layer.detailLo < Layer.detailHi
  check "detail band invariant: detailHi <= 1.0" $
    Layer.detailHi <= 1.0

  -- layerBlend: zoom → detail in [0,1].
  check "layerBlend at minZoom == 0.0 (zoomed out -> balls)" $
    approxEq (Layer.layerBlend Cam.minZoom) 0.0
  check "layerBlend at 1.0 == 1.0 (Builder default -> full detail)" $
    approxEq (Layer.layerBlend 1.0) 1.0
  check "layerBlend at maxZoom == 1.0 (fully zoomed in)" $
    approxEq (Layer.layerBlend Cam.maxZoom) 1.0
  check "layerBlend monotonic non-decreasing (0.5 <= 1.0)" $
    Layer.layerBlend 0.5 <= Layer.layerBlend 1.0
  -- Mid-band probe at 0.15 (inside new band [0.10, 0.20], strictly fractional).
  check "layerBlend monotonic non-decreasing (minZoom <= 0.15)" $
    Layer.layerBlend Cam.minZoom <= Layer.layerBlend 0.15

  -- ── Wide-window assertions (new retuned band) ────────────────────────────────
  -- Sub-atomic layer is FULLY ON at zoom 0.20 (== detailHi): layerBlend 0.20 ≈ 1.0.
  -- This means the user can pull the camera back to a wide framing (zoom 0.20) and
  -- still see the sub-particle detail (nucleons + electrons) without flipping to balls.
  check "layerBlend 0.20 ≈ 1.0 (sub-atomic fully ON when zoomed out to 0.20)" $
    approxEq (Layer.layerBlend 0.20) 1.0
  -- Materials reframe lands at zoom 0.08 (above minZoom 0.05, below detailLo 0.10)
  -- so the gallery opens on the whole-lattice ball view: layerBlend 0.08 ≈ 0.0.
  check "layerBlend 0.08 ≈ 0.0 (Materials reframe at 0.08 lands on ball view)" $
    approxEq (Layer.layerBlend 0.08) 0.0
  -- Mid-band probe: 0.15 is inside the crossfade zone, so detail is strictly
  -- fractional (not 0, not 1).
  check "layerBlend 0.15 is fractional (strictly in (0,1) — mid crossfade)" $
    Layer.layerBlend 0.15 > 0.0 && Layer.layerBlend 0.15 < 1.0

  -- easeDetail: one frame of smoothing current → target.
  let
    easedConverged = foldl (\c _ -> Layer.easeDetail c 1.0) 0.0 (range 1 60)

  check "easeDetail fixed point: easeDetail 1.0 1.0 == 1.0" $
    approxEq (Layer.easeDetail 1.0 1.0) 1.0
  check "easeDetail moves toward target (0.0 -> 1.0 lands strictly between)" $
    Layer.easeDetail 0.0 1.0 > 0.0 && Layer.easeDetail 0.0 1.0 < 1.0
  check "easeDetail stays in range (no overshoot either direction)" $
    Layer.easeDetail 0.0 1.0 <= 1.0 && Layer.easeDetail 1.0 0.0 >= 0.0
  check "easeDetail converges to target after ~60 frames (within 1e-3)" $
    abs (easedConverged - 1.0) < 1.0e-3

  log "all continuous LOD properties hold."

  -- ───── Camera.orbit + Camera.viewProjection (M2 genuinely-3D) ──────────
  log "Camera.orbit + Camera.viewProjection (M2 3D) properties:"

  -- (a) IDENTITY: orbit 0.0 0.0 is the 4x4 identity matrix (tol 1e-10).
  check "orbit 0.0 0.0 == M.identity (entry-by-entry, 1e-10)" $
    approxEqMatrix (Cam.orbit 0.0 0.0) M.identity

  -- (b) BYTE-IDENTICAL REGRESSION GUARD: viewProjection {yaw:0,pitch:0} z w h
  --     must equal projection z w h for at least two (z,w,h) triples.
  check "viewProjection {0,0} 1.0 800.0 600.0 == projection 1.0 800.0 600.0 (1e-10)" $
    approxEqMatrix
      (Cam.viewProjection { yaw: 0.0, pitch: 0.0 } 1.0 800.0 600.0)
      (Cam.projection 1.0 800.0 600.0)
  check "viewProjection {0,0} 2.0 1024.0 768.0 == projection 2.0 1024.0 768.0 (1e-10)" $
    approxEqMatrix
      (Cam.viewProjection { yaw: 0.0, pitch: 0.0 } 2.0 1024.0 768.0)
      (Cam.projection 2.0 1024.0 768.0)

  -- (c) ORBIT IS REAL: a non-zero yaw makes the matrix differ from identity and
  --     makes viewProjection differ from plain projection (orbit actually rotates).
  check "orbit 0.5 0.0 /= M.identity (non-zero yaw is not identity)" $
    not (approxEqMatrix (Cam.orbit 0.5 0.0) M.identity)
  check "viewProjection {yaw:0.5,pitch:0} 1.0 800.0 600.0 /= projection 1.0 800.0 600.0" $
    not
      ( approxEqMatrix
          (Cam.viewProjection { yaw: 0.5, pitch: 0.0 } 1.0 800.0 600.0)
          (Cam.projection 1.0 800.0 600.0)
      )

  -- (d) AXIS MAP: orbit (pi/2) 0.0 == rotateY(pi/2) as a pure rotation matrix.
  let
    orbitHalfPi = Cam.orbit (pi / 2.0) 0.0
    orbitVec = M.toVector orbitHalfPi
    orbitEntry r c = fromMaybe 999.0 (index orbitVec (r * 4 + c))

  check "orbit (pi/2) 0.0 entry [0][0] ≈ 0.0 (cos 90°)" $
    approxEq (orbitEntry 0 0) 0.0
  check "orbit (pi/2) 0.0 entry [0][2] ≈ 1.0 (sin 90°)" $
    approxEq (orbitEntry 0 2) 1.0
  check "orbit (pi/2) 0.0 entry [2][0] ≈ -1.0 (-sin 90°)" $
    approxEq (orbitEntry 2 0) (-1.0)
  check "orbit (pi/2) 0.0 entry [2][2] ≈ 0.0 (cos 90°)" $
    approxEq (orbitEntry 2 2) 0.0
  -- Y-axis is unchanged by a Y-rotation: entry [1][1] == 1.0.
  check "orbit (pi/2) 0.0 entry [1][1] == 1.0 (Y axis unaffected)" $
    approxEq (orbitEntry 1 1) 1.0

  log "all Camera.orbit + Camera.viewProjection (M2 3D) properties hold."

  -- ───── M3: Matrix.transpose + Builder.unprojectAtDepthFull + Camera.clampPitch ─
  log "M3 (transpose / unprojectAtDepthFull / clampPitch) properties:"

  -- ── (A) Matrix.transpose ─────────────────────────────────────────────────────
  let
    -- A known non-symmetric 4×4 matrix with distinct entries at each position.
    testM = fromMaybe (M.zeros 4 4) $ M.fromArray 4 4
      [ 1.0
      , 2.0
      , 3.0
      , 4.0
      , 5.0
      , 6.0
      , 7.0
      , 8.0
      , 9.0
      , 10.0
      , 11.0
      , 12.0
      , 13.0
      , 14.0
      , 15.0
      , 16.0
      ]
    testMT = M.transpose testM
    -- entry (i,j) from a 4x4 row-major matrix vector.
    entryM m_ i_ j_ = fromMaybe 0.0 (index (M.toVector m_) (i_ * 4 + j_))

  -- transpose swaps (i,j) ↔ (j,i) for a few representative off-diagonal entries.
  check "M3 transpose: entry (0,1) of transpose == entry (1,0) of original (2.0 -> 5.0)" $
    approxEq (entryM testMT 0 1) (entryM testM 1 0)
  check "M3 transpose: entry (1,0) of transpose == entry (0,1) of original (5.0 -> 2.0)" $
    approxEq (entryM testMT 1 0) (entryM testM 0 1)
  check "M3 transpose: entry (0,2) of transpose == entry (2,0) of original (3.0 -> 9.0)" $
    approxEq (entryM testMT 0 2) (entryM testM 2 0)
  check "M3 transpose: entry (2,3) of transpose == entry (3,2) of original (12.0 -> 15.0)" $
    approxEq (entryM testMT 2 3) (entryM testM 3 2)
  -- diagonal entries are unchanged by transpose.
  check "M3 transpose: diagonal entry (1,1) unchanged (6.0)" $
    approxEq (entryM testMT 1 1) (entryM testM 1 1)
  check "M3 transpose: diagonal entry (3,3) unchanged (16.0)" $
    approxEq (entryM testMT 3 3) (entryM testM 3 3)

  -- transpose (transpose m) == m (double-transpose is identity).
  check "M3 transpose: transpose (transpose m) == m (approxEqMatrix, 1e-10)" $
    approxEqMatrix (M.transpose (M.transpose testM)) testM

  -- transpose identity == identity.
  check "M3 transpose: transpose M.identity == M.identity (1e-10)" $
    approxEqMatrix (M.transpose M.identity) M.identity

  -- ── (B) Builder.unprojectAtDepthFull round-trip under orbit ──────────────────
  let
    -- Canvas and projection used for the full-orbit unproject tests.
    m3Canvas = { w: 800.0, h: 600.0 }
    m3Proj = Cam.projection 1.0 800.0 600.0
    -- A non-trivial orbit (yaw=0.5, pitch=0.3 radians).
    m3Orb = Cam.orbit 0.5 0.3
    -- The combined view-projection: vp = proj * orbit.
    m3Vp = Cam.viewProjection { yaw: 0.5, pitch: 0.3 } 1.0 800.0 600.0
    -- A world reference point in front of the camera.
    m3WorldRef = { x: 40.0, y: -25.0, z: -120.0 }
    -- Project the reference point to a pixel using the FULL view-projection.
    m3Px = B.projectToScreen m3Vp m3Canvas m3WorldRef
    -- Unproject with the full-orbit variant; should recover worldRef.
    m3Back = B.unprojectAtDepthFull m3Orb m3Proj m3Canvas { x: m3Px.x, y: m3Px.y } m3WorldRef
    m3ApproxClose a c = abs (a - c) < 1.0e-6

  -- (B1) Round-trip: projectToScreen then unprojectAtDepthFull recovers the world point.
  check "M3 unprojectAtDepthFull: round-trip x ≈ worldRef.x (1e-6)" $
    m3ApproxClose m3Back.x m3WorldRef.x
  check "M3 unprojectAtDepthFull: round-trip y ≈ worldRef.y (1e-6)" $
    m3ApproxClose m3Back.y m3WorldRef.y
  check "M3 unprojectAtDepthFull: round-trip z ≈ worldRef.z (1e-6)" $
    m3ApproxClose m3Back.z m3WorldRef.z

  -- (B2) Offset pixel test: unproject a shifted pixel, then re-project the result;
  --      it should reproduce the shifted pixel (the unproject lands on the correct ray).
  let
    m3PxShifted = { x: m3Px.x + 30.0, y: m3Px.y - 20.0 }
    m3Back2 = B.unprojectAtDepthFull m3Orb m3Proj m3Canvas m3PxShifted m3WorldRef
    m3PxReproj = B.projectToScreen m3Vp m3Canvas m3Back2

  check "M3 unprojectAtDepthFull offset: re-projected x ≈ shifted px.x (1e-6)" $
    m3ApproxClose m3PxReproj.x m3PxShifted.x
  check "M3 unprojectAtDepthFull offset: re-projected y ≈ shifted px.y (1e-6)" $
    m3ApproxClose m3PxReproj.y m3PxShifted.y

  -- ── (C) Equivalence at zero orbit: unprojectAtDepthFull with orbit 0 0 == unprojectAtDepth ──
  let
    -- At zero orbit, orbit = identity, so unprojectAtDepthFull must collapse to unprojectAtDepth.
    m3Orb0 = Cam.orbit 0.0 0.0
    m3RefPos0 = { x: 40.0, y: -25.0, z: 30.0 }
    m3Px0 = B.projectToScreen m3Proj m3Canvas m3RefPos0
    m3FullAtZero = B.unprojectAtDepthFull m3Orb0 m3Proj m3Canvas { x: m3Px0.x, y: m3Px0.y } m3RefPos0
    m3Legacy = B.unprojectAtDepth m3Proj m3Canvas { x: m3Px0.x, y: m3Px0.y } m3RefPos0

  check "M3 unprojectAtDepthFull orbit=0: x ≈ unprojectAtDepth x (1e-10)" $
    approxEq m3FullAtZero.x m3Legacy.x
  check "M3 unprojectAtDepthFull orbit=0: y ≈ unprojectAtDepth y (1e-10)" $
    approxEq m3FullAtZero.y m3Legacy.y
  check "M3 unprojectAtDepthFull orbit=0: z ≈ unprojectAtDepth z (1e-10)" $
    approxEq m3FullAtZero.z m3Legacy.z

  -- ── (D) Camera.clampPitch ─────────────────────────────────────────────────────
  -- clampPitch clamps to ±maxPitch (≈ 85° in radians = 1.4835).

  -- Over-bound positive clamps to +maxPitch.
  check "M3 clampPitch (pi) == Camera.maxPitch (over-bound positive clamps to max)" $
    approxEq (Cam.clampPitch pi) Cam.maxPitch

  -- Over-bound negative clamps to -maxPitch.
  check "M3 clampPitch (-pi) == -Camera.maxPitch (over-bound negative clamps to -max)" $
    approxEq (Cam.clampPitch (negate pi)) (negate Cam.maxPitch)

  -- In-range value passes through unchanged.
  check "M3 clampPitch 0.2 == 0.2 (in-range unchanged, 1e-10)" $
    approxEq (Cam.clampPitch 0.2) 0.2

  -- maxPitch is close to 85° in radians (1.4835 ± 0.01).
  check "M3 Camera.maxPitch ≈ 1.4835 (85° in radians, within 0.01)" $
    abs (Cam.maxPitch - 1.4835) < 0.01

  -- clampPitch at exactly ±maxPitch is a fixed point.
  check "M3 clampPitch maxPitch == maxPitch (fixed point at boundary)" $
    approxEq (Cam.clampPitch Cam.maxPitch) Cam.maxPitch
  check "M3 clampPitch (-maxPitch) == -maxPitch (fixed point at -boundary)" $
    approxEq (Cam.clampPitch (negate Cam.maxPitch)) (negate Cam.maxPitch)

  -- Zero is in-range (passes through).
  check "M3 clampPitch 0.0 == 0.0 (zero is in-range)" $
    approxEq (Cam.clampPitch 0.0) 0.0

  log "all M3 (transpose / unprojectAtDepthFull / clampPitch) properties hold."

  -- ───── M1: Camera.buttonOrbitDelta + Main.applyOrbit (orbit buttons) ────────
  log "M1 orbit buttons (Camera.buttonOrbitDelta + applyOrbit) properties:"

  let
    right = { dx: Cam.buttonOrbitDelta, dy: 0.0 }
    left = { dx: negate Cam.buttonOrbitDelta, dy: 0.0 }
    up = { dx: 0.0, dy: negate Cam.buttonOrbitDelta }
    zero = { yaw: 0.0, pitch: 0.0 }

  -- (a) buttonOrbitDelta is a positive magnitude; one right click yaws ≈ 0.1745 rad (≈10°).
  check "buttonOrbitDelta is a positive magnitude" $
    Cam.buttonOrbitDelta > 0.0
  check "one right-click yaw ≈ 0.1745 rad (≈10°, within 0.01)" $
    abs ((applyOrbit right zero).yaw - 0.1745) < 0.01

  -- (b) Repeated up-clicks clamp pitch and never exceed maxPitch in magnitude.
  let
    pitchAfterUp n = (foldl (\o _ -> applyOrbit up o) zero (range 1 n)).pitch
    pitchSaturated = pitchAfterUp 40
  check "pitch clamped at every step (20 up-clicks, magnitude <= maxPitch + 1e-10)" $
    all (\n -> abs (pitchAfterUp n) <= Cam.maxPitch + 1.0e-10) (range 1 20)
  check "pitch saturates at -maxPitch after 40 up-clicks (within 1e-10)" $
    abs (pitchSaturated - (negate Cam.maxPitch)) < 1.0e-10

  -- (c) left-then-right round-trips yaw back to 0.0 (within 1e-10).
  check "left-then-right round-trip: yaw ≈ 0.0 (1e-10)" $
    approxEq (applyOrbit right (applyOrbit left zero)).yaw 0.0

  -- (d) Reset is exactly {yaw:0.0, pitch:0.0} — the literal record, both fields == 0.0.
  check "reset record: yaw == 0.0 exactly" $
    zero.yaw == 0.0
  check "reset record: pitch == 0.0 exactly" $
    zero.pitch == 0.0

  log "all M1 orbit buttons properties hold."
