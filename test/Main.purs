module Test.Main where

import Prelude

import Data.Array (all, zipWith)
import Data.Maybe (fromMaybe)
import Data.Number (abs)
import Effect (Effect)
import Effect.Console (log)
import Effect.Exception (throw)
import Math.Matrix as M

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
  let v1 = M.toVector m1
      v2 = M.toVector m2
  in all identity (zipWith approxEq v1 v2)

identity4 :: M.Matrix Number
identity4 = fromMaybe (M.zeros 4 4) $ M.fromArray 4 4
  [ 1.0, 0.0, 0.0, 0.0
  , 0.0, 1.0, 0.0, 0.0
  , 0.0, 0.0, 1.0, 0.0
  , 0.0, 0.0, 0.0, 1.0
  ]

check :: String -> Boolean -> Effect Unit
check name cond =
  if cond
    then log $ "  ok  " <> name
    else throw $ "FAIL " <> name

main :: Effect Unit
main = do
  log "rotation matrix properties:"

  -- Zero rotation is identity
  check "rotateX 0° = I"  $ approxEqMatrix (rotateX 0.0) identity4
  check "rotateY 0° = I"  $ approxEqMatrix (rotateY 0.0) identity4
  check "rotateZ 0° = I"  $ approxEqMatrix (rotateZ 0.0) identity4

  -- Full rotation returns to identity
  check "rotateX 360° ≈ I" $ approxEqMatrix (rotateX 360.0) identity4
  check "rotateY 360° ≈ I" $ approxEqMatrix (rotateY 360.0) identity4
  check "rotateZ 360° ≈ I" $ approxEqMatrix (rotateZ 360.0) identity4

  -- Inverse rotation: rotate by θ then by -θ ≈ identity
  check "rotateX θ · rotateX -θ ≈ I" $ approxEqMatrix
    (M.multiply (rotateX 47.5) (rotateX (-47.5))) identity4
  check "rotateY θ · rotateY -θ ≈ I" $ approxEqMatrix
    (M.multiply (rotateY 47.5) (rotateY (-47.5))) identity4
  check "rotateZ θ · rotateZ -θ ≈ I" $ approxEqMatrix
    (M.multiply (rotateZ 47.5) (rotateZ (-47.5))) identity4

  -- Composition by angle addition: rotateX a · rotateX b ≈ rotateX (a+b)
  check "rotateX 90° · rotateX 90° ≈ rotateX 180°" $ approxEqMatrix
    (M.multiply (rotateX 90.0) (rotateX 90.0)) (rotateX 180.0)
  check "rotateY 30° · rotateY 60° ≈ rotateY 90°" $ approxEqMatrix
    (M.multiply (rotateY 30.0) (rotateY 60.0)) (rotateY 90.0)
  check "rotateZ 45° · rotateZ 45° ≈ rotateZ 90°" $ approxEqMatrix
    (M.multiply (rotateZ 45.0) (rotateZ 45.0)) (rotateZ 90.0)

  -- 180° + 180° round-trip
  check "rotateX 180° · rotateX 180° ≈ I" $ approxEqMatrix
    (M.multiply (rotateX 180.0) (rotateX 180.0)) identity4
  check "rotateY 180° · rotateY 180° ≈ I" $ approxEqMatrix
    (M.multiply (rotateY 180.0) (rotateY 180.0)) identity4
  check "rotateZ 180° · rotateZ 180° ≈ I" $ approxEqMatrix
    (M.multiply (rotateZ 180.0) (rotateZ 180.0)) identity4

  log "all rotation properties hold."
