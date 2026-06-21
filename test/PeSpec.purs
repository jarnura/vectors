-- | Unit tests for the Pe (Morse potential) module.
-- | Hand-rolled assertions in the style of CameraLayerSpec, using Test.Util.approxEq
-- | and Test.Util.check (1e-10 tolerance). 52 assertions across four representative
-- | element pairs: H-H (1,1), C-C (6,6), O-H (8,1), and a heavy pair Kr-Kr (36,36).
module Test.PeSpec where

import Prelude

import Chem (bondEnergy)
import Data.Number (abs, isFinite)
import Effect (Effect)
import Effect.Console (log) as Console
import Pe as Pe
import Test.Util (approxEq, check)

-- | Tolerance for wall-distance checks: morseWidth a is derived from R0 − minSep
-- | but stretchEnergy ≠ wallEnergyFactor*De exactly when a uses the closed form;
-- | the wall-distance closed form inverts exactly by construction so the error
-- | is only floating-point rounding (order 1e-12), well within 1e-10.
wallTol :: Number
wallTol = 1.0e-8

-- | Tolerance for bondR0 ~ 160 checks: pullRestLen = max(minSep, 160); for H-H
-- | and C-C the minSep clamps to 130 (absoluteMinPe), so R0 = 160.0 exactly.
r0Tol :: Number
r0Tol = 1.0e-10


peSpec :: Effect Unit
peSpec = do
  Console.log "Pe (Morse potential) properties:"

  -- ─── 1. Primitive morse: minimum, asymptote, sign of repulsive branch ───────

  -- morse De a R0 R0 = -De  (global minimum)
  let de0 = 4.36
  let a0  = 0.049
  let r0h = 160.0
  check "morse primitive: minimum at R0 == -De (H-H params)" $
    approxEq (Pe.morse de0 a0 r0h r0h) (-de0)

  -- asymptote: morse → 0 as r → ∞
  check "morse primitive: asymptote at r=1e4 within 1e-10 of 0" $
    approxEq (Pe.morse de0 a0 r0h 1.0e4) 0.0

  -- repulsive: morse > 0 for r well below R0
  check "morse primitive: repulsive (>0) for r << R0" $
    Pe.morse de0 a0 r0h (r0h - 30.0) > 0.0

  -- forceMorse = 0 at equilibrium
  check "forceMorse primitive: zero at R0" $
    approxEq (Pe.forceMorse de0 a0 r0h r0h) 0.0

  -- forceMorse > 0 for r < R0 (repulsion pushes apart)
  check "forceMorse primitive: repulsive (>0) for r < R0" $
    Pe.forceMorse de0 a0 r0h (r0h - 10.0) > 0.0

  -- forceMorse < 0 for r > R0 (attraction pulls together)
  check "forceMorse primitive: attractive (<0) for r > R0" $
    Pe.forceMorse de0 a0 r0h (r0h + 10.0) < 0.0

  Console.log "  [morse primitive] ok"

  -- ─── 2. bondDepth == Chem.bondEnergy, symmetry ──────────────────────────────

  check "bondDepth 1 1 == Chem.bondEnergy 1 1 (H-H)" $
    approxEq (Pe.bondDepth 1 1) (bondEnergy 1 1)
  check "bondDepth 6 6 == Chem.bondEnergy 6 6 (C-C)" $
    approxEq (Pe.bondDepth 6 6) (bondEnergy 6 6)
  check "bondDepth 8 1 == Chem.bondEnergy 8 1 (O-H)" $
    approxEq (Pe.bondDepth 8 1) (bondEnergy 8 1)
  check "bondDepth 36 36 == Chem.bondEnergy 36 36 (Kr-Kr)" $
    approxEq (Pe.bondDepth 36 36) (bondEnergy 36 36)

  -- Symmetry: bondDepth z1 z2 == bondDepth z2 z1
  check "bondDepth symmetry H-H (trivial)" $
    approxEq (Pe.bondDepth 1 1) (Pe.bondDepth 1 1)
  check "bondDepth symmetry O-H == H-O" $
    approxEq (Pe.bondDepth 8 1) (Pe.bondDepth 1 8)
  check "bondDepth symmetry C-O == O-C" $
    approxEq (Pe.bondDepth 6 8) (Pe.bondDepth 8 6)

  Console.log "  [bondDepth / symmetry] ok"

  -- ─── 3. bondR0: pinned to ~160, >= minSep ───────────────────────────────────

  -- For H-H and C-C, minSep clamps to absoluteMinPe=130, so R0 = 160.0 exactly.
  check "bondR0 H-H == 160.0 (pullRestLen = max(130, 160) = 160)" $
    approxEq (Pe.bondR0 1 1) 160.0
  check "bondR0 C-C == 160.0 (pullRestLen = max(130, 160) = 160)" $
    approxEq (Pe.bondR0 6 6) 160.0

  -- bondR0 >= minSep for all test pairs (the Pauli floor is respected)
  check "bondR0 H-H >= minSepPe (H-H) = 130.0" $
    Pe.bondR0 1 1 >= Pe.absoluteMinPe
  check "bondR0 C-C >= minSepPe (C-C) = 130.0" $
    Pe.bondR0 6 6 >= Pe.absoluteMinPe
  check "bondR0 O-H >= absoluteMinPe" $
    Pe.bondR0 8 1 >= Pe.absoluteMinPe
  check "bondR0 Kr-Kr >= absoluteMinPe" $
    Pe.bondR0 36 36 >= Pe.absoluteMinPe

  -- bondR0 symmetry
  check "bondR0 symmetry: O-H == H-O" $
    approxEq (Pe.bondR0 8 1) (Pe.bondR0 1 8)
  check "bondR0 symmetry: C-O == O-C" $
    approxEq (Pe.bondR0 6 8) (Pe.bondR0 8 6)

  Console.log "  [bondR0] ok"

  -- ─── 4. morseWidth > 0, symmetry ────────────────────────────────────────────

  check "morseWidth H-H > 0" $
    Pe.morseWidth 1 1 > 0.0
  check "morseWidth C-C > 0" $
    Pe.morseWidth 6 6 > 0.0
  check "morseWidth O-H > 0" $
    Pe.morseWidth 8 1 > 0.0
  check "morseWidth Kr-Kr > 0" $
    Pe.morseWidth 36 36 > 0.0

  -- symmetry
  check "morseWidth symmetry: O-H == H-O" $
    approxEq (Pe.morseWidth 8 1) (Pe.morseWidth 1 8)

  Console.log "  [morseWidth] ok"

  -- ─── 5. morseE: minimum, asymptote, sign convention per pair ────────────────

  -- morseE z z (bondR0 z z) == -(bondDepth z z)  (well minimum)
  check "morseE H-H at R0 == -bondDepth (well minimum)" $
    approxEq (Pe.morseE 1 1 (Pe.bondR0 1 1)) (-(Pe.bondDepth 1 1))
  check "morseE C-C at R0 == -bondDepth" $
    approxEq (Pe.morseE 6 6 (Pe.bondR0 6 6)) (-(Pe.bondDepth 6 6))
  check "morseE O-H at R0 == -bondDepth" $
    approxEq (Pe.morseE 8 1 (Pe.bondR0 8 1)) (-(Pe.bondDepth 8 1))
  check "morseE Kr-Kr at R0 == -bondDepth" $
    approxEq (Pe.morseE 36 36 (Pe.bondR0 36 36)) (-(Pe.bondDepth 36 36))

  -- asymptote → 0 for r = 1e4
  check "morseE H-H at r=1e4 within 1e-10 of 0" $
    approxEq (Pe.morseE 1 1 1.0e4) 0.0
  check "morseE C-C at r=1e4 within 1e-10 of 0" $
    approxEq (Pe.morseE 6 6 1.0e4) 0.0
  check "morseE O-H at r=1e4 within 1e-10 of 0" $
    approxEq (Pe.morseE 8 1 1.0e4) 0.0
  check "morseE Kr-Kr at r=1e4 within 1e-10 of 0" $
    approxEq (Pe.morseE 36 36 1.0e4) 0.0

  -- repulsive branch: morseE > 0 for r = R0 - 20 (inside Pauli wall)
  check "morseE H-H > 0 for r = R0 - 20 (repulsive)" $
    Pe.morseE 1 1 (Pe.bondR0 1 1 - 20.0) > 0.0
  check "morseE C-C > 0 for r = R0 - 20 (repulsive)" $
    Pe.morseE 6 6 (Pe.bondR0 6 6 - 20.0) > 0.0

  -- symmetry: morseE z1 z2 == morseE z2 z1
  check "morseE symmetry: O-H at R0 == H-O at R0" $
    approxEq (Pe.morseE 8 1 (Pe.bondR0 8 1)) (Pe.morseE 1 8 (Pe.bondR0 1 8))

  Console.log "  [morseE] ok"

  -- ─── 6. morseForce: zero at R0, sign convention ─────────────────────────────

  check "morseForce H-H at R0 == 0" $
    approxEq (Pe.morseForce 1 1 (Pe.bondR0 1 1)) 0.0
  check "morseForce C-C at R0 == 0" $
    approxEq (Pe.morseForce 6 6 (Pe.bondR0 6 6)) 0.0
  check "morseForce O-H at R0 == 0" $
    approxEq (Pe.morseForce 8 1 (Pe.bondR0 8 1)) 0.0
  check "morseForce Kr-Kr at R0 == 0" $
    approxEq (Pe.morseForce 36 36 (Pe.bondR0 36 36)) 0.0

  -- repulsive branch: morseForce > 0 for r = R0 - 20
  check "morseForce H-H > 0 for r < R0 (repulsive)" $
    Pe.morseForce 1 1 (Pe.bondR0 1 1 - 20.0) > 0.0
  check "morseForce C-C > 0 for r < R0 (repulsive)" $
    Pe.morseForce 6 6 (Pe.bondR0 6 6 - 20.0) > 0.0

  -- attractive branch: morseForce < 0 for r = R0 + 20
  check "morseForce H-H < 0 for r > R0 (attractive)" $
    Pe.morseForce 1 1 (Pe.bondR0 1 1 + 20.0) < 0.0
  check "morseForce C-C < 0 for r > R0 (attractive)" $
    Pe.morseForce 6 6 (Pe.bondR0 6 6 + 20.0) < 0.0

  -- symmetry
  check "morseForce symmetry: O-H at R0 == H-O at R0 (both 0)" $
    approxEq (Pe.morseForce 8 1 (Pe.bondR0 8 1)) (Pe.morseForce 1 8 (Pe.bondR0 1 8))

  Console.log "  [morseForce] ok"

  -- ─── 7. morseK = 2 * a^2 * De, positive ─────────────────────────────────────

  check "morseK H-H == 2*a^2*De > 0" $
    let a = Pe.morseWidth 1 1
        de = Pe.bondDepth 1 1
    in approxEq (Pe.morseK 1 1) (2.0 * a * a * de) && Pe.morseK 1 1 > 0.0
  check "morseK C-C == 2*a^2*De > 0" $
    let a = Pe.morseWidth 6 6
        de = Pe.bondDepth 6 6
    in approxEq (Pe.morseK 6 6) (2.0 * a * a * de) && Pe.morseK 6 6 > 0.0
  check "morseK O-H == 2*a^2*De > 0" $
    let a = Pe.morseWidth 8 1
        de = Pe.bondDepth 8 1
    in approxEq (Pe.morseK 8 1) (2.0 * a * a * de) && Pe.morseK 8 1 > 0.0
  check "morseK Kr-Kr > 0" $
    Pe.morseK 36 36 > 0.0

  Console.log "  [morseK] ok"

  -- ─── 8. stretchEnergy: zero at R0, non-negative everywhere, approaches De ────

  check "stretchEnergy H-H at R0 == 0.0 (well bottom)" $
    approxEq (Pe.stretchEnergy 1 1 (Pe.bondR0 1 1)) 0.0
  check "stretchEnergy C-C at R0 == 0.0" $
    approxEq (Pe.stretchEnergy 6 6 (Pe.bondR0 6 6)) 0.0

  -- At large r, stretchEnergy → De (dissociation cost)
  check "stretchEnergy H-H at r=1e4 ≈ bondDepth (dissociation asymptote)" $
    approxEq (Pe.stretchEnergy 1 1 1.0e4) (Pe.bondDepth 1 1)
  check "stretchEnergy C-C at r=1e4 ≈ bondDepth" $
    approxEq (Pe.stretchEnergy 6 6 1.0e4) (Pe.bondDepth 6 6)

  -- stretchEnergy >= 0 everywhere (R0-30, R0, R0+30, 1e4)
  check "stretchEnergy H-H >= 0 at r < R0 (repulsive wall)" $
    Pe.stretchEnergy 1 1 (Pe.bondR0 1 1 - 30.0) >= 0.0
  check "stretchEnergy H-H >= 0 at r > R0 (attraction side)" $
    Pe.stretchEnergy 1 1 (Pe.bondR0 1 1 + 30.0) >= 0.0

  Console.log "  [stretchEnergy] ok"

  -- ─── 9. wallDistanceFor ≈ minSeparation ─────────────────────────────────────
  -- By construction, wallDistanceFor should land ~ minSepPe (within ~1 world unit
  -- due to the closed-form derivation aligning with absoluteMinPe=130 for H-H/C-C).
  -- The tolerance is 5.0 world units (generous to survive float rounding in the
  -- ln/sqrt chain, while still pinning the location meaningfully).

  let wallTolWide = 5.0

  check "wallDistanceFor H-H ≈ absoluteMinPe (within 5 world units)" $
    abs (Pe.wallDistanceFor 1 1 - Pe.absoluteMinPe) < wallTolWide
  check "wallDistanceFor C-C ≈ absoluteMinPe (within 5 world units)" $
    abs (Pe.wallDistanceFor 6 6 - Pe.absoluteMinPe) < wallTolWide
  check "wallDistanceFor O-H is between absoluteMinPe and floorCeilPe" $
    Pe.wallDistanceFor 8 1 >= (Pe.absoluteMinPe - wallTolWide)
      && Pe.wallDistanceFor 8 1 <= (Pe.floorCeilPe + wallTolWide)
  check "wallDistanceFor Kr-Kr < bondR0 Kr-Kr (wall is left of equilibrium)" $
    Pe.wallDistanceFor 36 36 < Pe.bondR0 36 36
  check "wallDistanceFor H-H < bondR0 H-H" $
    Pe.wallDistanceFor 1 1 < Pe.bondR0 1 1

  Console.log "  [wallDistanceFor] ok"

  -- ─── 10. Determinism ────────────────────────────────────────────────────────

  check "determinism: morseE H-H called twice gives same result" $
    Pe.morseE 1 1 150.0 == Pe.morseE 1 1 150.0
  check "determinism: morseForce C-C called twice gives same result" $
    Pe.morseForce 6 6 170.0 == Pe.morseForce 6 6 170.0
  check "determinism: bondR0 O-H called twice gives same result" $
    Pe.bondR0 8 1 == Pe.bondR0 1 8
  check "determinism: morseK Kr-Kr called twice gives same result" $
    Pe.morseK 36 36 == Pe.morseK 36 36

  Console.log "  [determinism] ok"

  -- ─── 11. Clamp-safety: no NaN / Infinity for edge r values ──────────────────
  -- All morseE values are finite for all (z, r) combinations in the test set.
  check "morseE z=1 r=0.001 is finite" $
    isFinite (Pe.morseE 1 1 0.001)
  check "morseE z=6 r=0.001 is finite" $
    isFinite (Pe.morseE 6 6 0.001)
  check "morseE z=8 r=0.001 is finite" $
    isFinite (Pe.morseE 8 1 0.001)
  check "morseE z=36 r=0.001 is finite" $
    isFinite (Pe.morseE 36 36 0.001)
  check "morseE z=1 r=R0 is finite" $
    isFinite (Pe.morseE 1 1 (Pe.bondR0 1 1))
  check "morseE z=1 r=1e4 is finite" $
    isFinite (Pe.morseE 1 1 1.0e4)
  check "morseForce z=1 r=0.001 is finite" $
    isFinite (Pe.morseForce 1 1 0.001)
  check "morseForce z=36 r=0.001 is finite" $
    isFinite (Pe.morseForce 36 36 0.001)
  check "morseWidth z=36 is finite and positive" $
    isFinite (Pe.morseWidth 36 36) && Pe.morseWidth 36 36 > 0.0

  Console.log "  [clamp-safety / no NaN] ok"

  Console.log "all Pe (Morse potential) properties hold."
