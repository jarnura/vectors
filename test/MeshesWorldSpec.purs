module Test.MeshesWorldSpec where

import Prelude

import Data.Array (all, length, range)
import Data.Foldable (maximum, minimum)
import Data.Maybe (fromMaybe, isNothing)
import Data.Number (abs, sqrt)
import Effect (Effect)
import Effect.Console (log)
import FRP.Loop (emptyInput)
import Meshes (groundPlane, gridFloor, sphere)
import World (groundTransform, groundY, groundExtent, gridDivisions, skyColor)
import Math.Matrix as M
import Test.Util (approxEq, approxEqMatrix, check, epsilon, everyNth, nth)

meshesWorldSpec :: Effect Unit
meshesWorldSpec = do
  -- ───── World backdrop geometry (M1) ─────────────────────────────────
  log "world geometry properties:"

  let
    gp = groundPlane 800.0
    gpYs = everyNth 3 1 gp.vertices
    gpXs = everyNth 3 0 gp.vertices
    gpZs = everyNth 3 2 gp.vertices

  -- A ground plane is one quad: 4 vertices (12 floats), 4 normals, 6 indices.
  check "groundPlane has 4 vertices (12 floats)" $ length gp.vertices == 12
  check "groundPlane has 4 normals (12 floats)" $ length gp.normals == 12
  check "groundPlane has 6 indices (2 triangles)" $ length gp.indices == 6

  -- The quad is flat at local Y = 0 (the model matrix places it in the world).
  check "groundPlane is flat at Y=0" $ all (\y -> approxEq y 0.0) gpYs

  -- All normals point straight up (+Y).
  check "groundPlane normals all (0,1,0)" $
    all (\i -> let n = nth gp.normals i in approxEq n.x 0.0 && approxEq n.y 1.0 && approxEq n.z 0.0)
      [ 0, 1, 2, 3 ]

  -- Corners span exactly [-e, +e] in X and Z.
  check "groundPlane X spans ±extent" $
    approxEq (fromMaybe 0.0 (minimum gpXs)) (-800.0) && approxEq (fromMaybe 0.0 (maximum gpXs)) 800.0
  check "groundPlane Z spans ±extent" $
    approxEq (fromMaybe 0.0 (minimum gpZs)) (-800.0) && approxEq (fromMaybe 0.0 (maximum gpZs)) 800.0

  -- Winding is CCW seen from above: cross((v1-v0),(v2-v0)) points +Y.
  check "groundPlane first triangle wound CCW (normal +Y)" $
    let
      v0 = nth gp.vertices 0
      v1 = nth gp.vertices 1
      v2 = nth gp.vertices 2
      ax = v1.x - v0.x
      az = v1.z - v0.z
      bx = v2.x - v0.x
      bz = v2.z - v0.z
      ny = az * bx - ax * bz -- +Y component of (a × b)
    in
      ny > 0.0

  -- Grid floor: n divisions ⇒ 2*(n+1) line segments ⇒ 4*(n+1) index entries.
  let
    gf = gridFloor 800.0 8
    gfYs = everyNth 3 1 gf.vertices
  check "gridFloor 8 has 2*(n+1)=18 segments (36 indices)" $ length gf.indices == 36
  check "gridFloor vertices flat at Y=0" $ all (\y -> approxEq y 0.0) gfYs
  check "gridFloor stays within ±extent" $
    all (\v -> abs v <= 800.0 + epsilon) gf.vertices

  log "all world geometry properties hold."

  -- ───── World placement (M2) ─────────────────────────────────────────
  log "world placement properties:"

  -- The ground's model matrix is a constant translation (no State input),
  -- so it never moves when the cube rotates.
  check "groundTransform = translate(0, groundY, 0)" $
    approxEqMatrix groundTransform (M.translate 0.0 groundY 0.0)

  -- The plane sits at the main cube's base (half-extent 100 ⇒ base at Y=-100).
  check "groundY places plane at cube base (-100)" $ approxEq groundY (-100.0)

  -- Extent stays within the far-plane budget (camera 1000 back, far 2000 ⇒ ≤ ~900).
  check "groundExtent within far-plane budget (≤ 900)" $ groundExtent <= 900.0

  log "all world placement properties hold."

  -- ───── Grid floor wiring (M3) ───────────────────────────────────────
  log "grid wiring properties:"

  -- The grid the app actually renders (extent × divisions).
  let
    appGrid = gridFloor groundExtent gridDivisions
    appGridVerts = (length appGrid.vertices) / 3

  -- n divisions ⇒ 2*(n+1) line segments ⇒ 4*(n+1) index entries.
  check "app grid has 2*(n+1) segments" $
    length appGrid.indices == 4 * (gridDivisions + 1)

  -- Every index addresses a real vertex (no out-of-range index).
  check "app grid indices in range" $
    all (\i -> i >= 0 && i < appGridVerts) appGrid.indices

  -- Vertex count stays under the Uint16 index-buffer cap (65536).
  check "app grid within Uint16 cap" $ appGridVerts < 65536

  log "all grid wiring properties hold."

  -- ───── Sky backdrop (M4) ────────────────────────────────────────────
  log "sky backdrop properties:"

  let inUnit v = v >= 0.0 && v <= 1.0

  -- Every channel is a valid normalized color component.
  check "skyColor channels in [0,1]" $
    inUnit skyColor.r && inUnit skyColor.g && inUnit skyColor.b && inUnit skyColor.a

  -- Regression guard: the backdrop is no longer plain white.
  check "skyColor is not white" $
    not (approxEq skyColor.r 1.0 && approxEq skyColor.g 1.0 && approxEq skyColor.b 1.0)

  -- A sky reads blue-dominant (blue ≥ red and ≥ green).
  check "skyColor is blue-dominant" $
    skyColor.b >= skyColor.r && skyColor.b >= skyColor.g

  log "all sky backdrop properties hold."

  -- ───── Shear input wiring (shear-button M2) ─────────────────────────
  log "shear input properties:"

  -- The extended Input has a shear channel, empty by default (no click yet).
  check "emptyInput.shear is Nothing" $ isNothing emptyInput.shear

  log "all shear input properties hold."

  -- ───── UV sphere mesh (atomos M1) ───────────────────────────────────
  log "sphere mesh properties:"

  let
    latSeg = 12
    longSeg = 12
    sp = sphere latSeg longSeg 50.0
    spVerts = length sp.vertices / 3
    mag v = sqrt (v.x * v.x + v.y * v.y + v.z * v.z)

  -- A UV sphere has (lat+1)*(long+1) vertices, equally many normal floats.
  check "sphere vertex count = (lat+1)(long+1)" $
    length sp.vertices == (latSeg + 1) * (longSeg + 1) * 3
  check "sphere normals count matches vertices" $
    length sp.normals == length sp.vertices

  -- Two triangles per quad ⇒ lat*long*6 indices.
  check "sphere index count = lat*long*6" $
    length sp.indices == latSeg * longSeg * 6

  -- Every vertex lies on the sphere of the given radius.
  check "sphere vertices lie on radius" $
    all (\vi -> approxEq (mag (nth sp.vertices vi)) 50.0) (range 0 (spVerts - 1))

  -- Every normal is unit length.
  check "sphere normals are unit length" $
    all (\vi -> approxEq (mag (nth sp.normals vi)) 1.0) (range 0 (spVerts - 1))

  -- Every index addresses a real vertex.
  check "sphere indices in range" $
    all (\i -> i >= 0 && i < spVerts) sp.indices

  log "all sphere mesh properties hold."
