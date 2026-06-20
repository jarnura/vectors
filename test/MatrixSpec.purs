module Test.MatrixSpec where

import Prelude

import Data.Maybe (fromMaybe)
import Effect (Effect)
import Effect.Console (log)
import Math.Matrix as M
import Vector (rotateX, rotateY, rotateZ)
import Test.Util (approxEq, approxEqMatrix, check, identity4)
import Data.Array (index)

matrixSpec :: Effect Unit
matrixSpec = do
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
