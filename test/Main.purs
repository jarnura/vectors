module Test.Main where

import Prelude

import Data.Array (all, filter, index, length, mapWithIndex, zipWith)
import Data.Foldable (maximum, minimum)
import Data.Maybe (fromMaybe)
import Data.Number (abs)
import Effect (Effect)
import Effect.Console (log)
import Effect.Exception (throw)
import Math.Matrix as M

import Meshes (groundPlane, gridFloor)
import Vector (rotateX, rotateY, rotateZ)

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
