module Test.Main where

import Prelude

import Data.Array (all, filter, index, length, mapWithIndex, range, zipWith)
import Data.Foldable (maximum, minimum)
import Data.Maybe (fromMaybe, isNothing)
import FRP.Loop (emptyInput)
import Data.Number (abs, sqrt)
import Effect (Effect)
import Effect.Console (log)
import Effect.Exception (throw)
import Math.Matrix as M

import Meshes (groundPlane, gridFloor, sphere)
import Vector (rotateX, rotateY, rotateZ)
import World (groundTransform, groundY, groundExtent, gridDivisions, skyColor)

-- Tolerance for floating-point matrix equality.
-- sin/cos roundtrip at 360° produces error on the order of 1e-15;
-- 1e-10 leaves comfortable headroom for chained multiplications.
epsilon :: Number
epsilon = 1.0e-10

approxEq :: Number -> Number -> Boolean
approxEq a b = abs (a - b) < epsilon

approxEqMatrix :: M.Matrix Number -> M.Matrix Number -> Boolean
approxEqMatrix m1 m2 =
  let
    v1 = M.toVector m1
    v2 = M.toVector m2
  in
    all identity (zipWith approxEq v1 v2)

identity4 :: M.Matrix Number
identity4 = fromMaybe (M.zeros 4 4) $ M.fromArray 4 4
  [ 1.0
  , 0.0
  , 0.0
  , 0.0
  , 0.0
  , 1.0
  , 0.0
  , 0.0
  , 0.0
  , 0.0
  , 1.0
  , 0.0
  , 0.0
  , 0.0
  , 0.0
  , 1.0
  ]

check :: String -> Boolean -> Effect Unit
check name cond =
  if cond then log $ "  ok  " <> name
  else throw $ "FAIL " <> name

main :: Effect Unit
main = do
  log "rotation matrix properties:"

  -- Zero rotation is identity
  check "rotateX 0° = I" $ approxEqMatrix (rotateX 0.0) identity4
  check "rotateY 0° = I" $ approxEqMatrix (rotateY 0.0) identity4
  check "rotateZ 0° = I" $ approxEqMatrix (rotateZ 0.0) identity4

  -- Full rotation returns to identity
  check "rotateX 360° ≈ I" $ approxEqMatrix (rotateX 360.0) identity4
  check "rotateY 360° ≈ I" $ approxEqMatrix (rotateY 360.0) identity4
  check "rotateZ 360° ≈ I" $ approxEqMatrix (rotateZ 360.0) identity4

  -- Inverse rotation: rotate by θ then by -θ ≈ identity
  check "rotateX θ · rotateX -θ ≈ I" $ approxEqMatrix
    (M.multiply (rotateX 47.5) (rotateX (-47.5)))
    identity4
  check "rotateY θ · rotateY -θ ≈ I" $ approxEqMatrix
    (M.multiply (rotateY 47.5) (rotateY (-47.5)))
    identity4
  check "rotateZ θ · rotateZ -θ ≈ I" $ approxEqMatrix
    (M.multiply (rotateZ 47.5) (rotateZ (-47.5)))
    identity4

  -- Composition by angle addition: rotateX a · rotateX b ≈ rotateX (a+b)
  check "rotateX 90° · rotateX 90° ≈ rotateX 180°" $ approxEqMatrix
    (M.multiply (rotateX 90.0) (rotateX 90.0))
    (rotateX 180.0)
  check "rotateY 30° · rotateY 60° ≈ rotateY 90°" $ approxEqMatrix
    (M.multiply (rotateY 30.0) (rotateY 60.0))
    (rotateY 90.0)
  check "rotateZ 45° · rotateZ 45° ≈ rotateZ 90°" $ approxEqMatrix
    (M.multiply (rotateZ 45.0) (rotateZ 45.0))
    (rotateZ 90.0)

  -- 180° + 180° round-trip
  check "rotateX 180° · rotateX 180° ≈ I" $ approxEqMatrix
    (M.multiply (rotateX 180.0) (rotateX 180.0))
    identity4
  check "rotateY 180° · rotateY 180° ≈ I" $ approxEqMatrix
    (M.multiply (rotateY 180.0) (rotateY 180.0))
    identity4
  check "rotateZ 180° · rotateZ 180° ≈ I" $ approxEqMatrix
    (M.multiply (rotateZ 180.0) (rotateZ 180.0))
    identity4

  log "all rotation properties hold."

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

  -- ───── Shear matrix (shear-button M1) ───────────────────────────────
  log "shear matrix properties:"

  -- Zero shear is the identity.
  check "shear 0 = I" $ approxEqMatrix (M.shear 0.0) identity4

  -- The shear matrix differs from identity only at entry [0][1] = k.
  check "shear k has entry [0][1] = k" $
    approxEq (fromMaybe 0.0 (index (M.toVector (M.shear 0.7)) 1)) 0.7

  -- Shearing the +Y basis vector moves it by k in +X: (0,1,0,1) ↦ (k,1,0,1).
  check "shear k shears +Y into +X" $
    let
      sheared = M.toVector (M.multiply (M.shear 0.7) (M.fromColumn [ 0.0, 1.0, 0.0, 1.0 ]))
    in
      approxEq (fromMaybe 0.0 (index sheared 0)) 0.7

  log "all shear matrix properties hold."

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

-- Extract every Nth element starting at `start` (used to pluck x/y/z columns).
everyNth :: Int -> Int -> Array Number -> Array Number
everyNth step start xs =
  map (\t -> t.v)
    $ filter (\t -> (t.i - start) `mod` step == 0 && t.i >= start)
    $ mapWithIndex (\i v -> { i, v }) xs

-- Read a vec3 at vertex index `vi` (positions are packed [x,y,z, x,y,z, ...]).
nth :: Array Number -> Int -> { x :: Number, y :: Number, z :: Number }
nth xs vi =
  { x: at (vi * 3)
  , y: at (vi * 3 + 1)
  , z: at (vi * 3 + 2)
  }
  where
  at i = fromMaybe 0.0 (index xs i)
