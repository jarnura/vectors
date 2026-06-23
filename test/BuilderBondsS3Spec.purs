-- S3 per-order geometry/energy assertions for the Builder bond-pull pipeline.
-- Verifies that bond ORDER affects equilibrium distance (R0') and well depth (De')
-- via Pe.bondR0'/Pe.bondDepth', and that the Builder drag-strength pipeline
-- threads order through correctly (order=2 O-O converges to R0'(order=2),
-- order=3 N-N's stretchEnergy' is 0 at R0'(order=3), etc.).
-- Order=1 continuity assertions bridge to the pre-S3 M1 golden values.
-- Pure, total, deterministic — no Effect/WebGL.
module Test.BuilderBondsS3Spec where

import Prelude

import Data.Array (index, length)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Number (abs, sqrt)
import Effect (Effect)
import Effect.Console (log)
import Builder as B
import Pe as Pe
import Test.Util (approxEq, check)

-- Convergence-band tolerance (world units): same as BuilderBondsSpec.bondR0Tol.
bondR0Tol :: Number
bondR0Tol = 5.0

builderBondsS3Spec :: Effect Unit
builderBondsS3Spec = do
  log "S3 per-order geometry/energy (bond order affects R0 and De) properties:"

  -- Helper: get the bond from a two-atom state.
  let
    getBond st = index st.bonds 0

    -- Build isolated O-O (order=2): two O atoms within bondThreshold.
    s3_oo = B.addAtom 8 { x: 150.0, y: 0.0, z: 0.0 }
      (B.addAtom 8 { x: 0.0, y: 0.0, z: 0.0 } B.emptyBuilder)
    s3_ooBond = getBond s3_oo

    -- Build isolated N-N (order=3): two N atoms within bondThreshold.
    s3_nn = B.addAtom 7 { x: 150.0, y: 0.0, z: 0.0 }
      (B.addAtom 7 { x: 0.0, y: 0.0, z: 0.0 } B.emptyBuilder)
    s3_nnBond = getBond s3_nn

    -- Build H-H (order=1 due to valence cap).
    s3_hh = B.addAtom 1 { x: 150.0, y: 0.0, z: 0.0 }
      (B.addAtom 1 { x: 0.0, y: 0.0, z: 0.0 } B.emptyBuilder)
    s3_hhBond = getBond s3_hh

  -- Sanity: verify bond orders formed correctly.
  check "S3 fixture O-O has order 2" $
    case s3_ooBond of
      Just bd -> bd.order == 2
      Nothing -> false
  check "S3 fixture N-N has order 3" $
    case s3_nnBond of
      Just bd -> bd.order == 3
      Nothing -> false
  check "S3 fixture H-H has order 1" $
    case s3_hhBond of
      Just bd -> bd.order == 1
      Nothing -> false

  -- Pe.bondR0' order=1 == Pe.bondR0 (continuity for the O-H pair).
  check "S3 continuity: Pe.bondR0' O-H order=1 == Pe.bondR0 O-H" $
    approxEq (Pe.bondR0' 8 1 1) (Pe.bondR0 8 1)

  -- Pe.bondDepth' order=1 == Pe.bondDepth (continuity for the O-O pair).
  check "S3 continuity: Pe.bondDepth' O-O order=1 == Pe.bondDepth O-O" $
    approxEq (Pe.bondDepth' 8 8 1) (Pe.bondDepth 8 8)

  -- O-O order=2 rests shorter than order=1 reference.
  check "S3 O-O: bondR0' order=2 < bondR0' order=1" $
    Pe.bondR0' 8 8 2 < Pe.bondR0' 8 8 1

  -- O-O order=2 has deeper De than order=1.
  check "S3 O-O: bondDepth' order=2 > bondDepth' order=1" $
    Pe.bondDepth' 8 8 2 > Pe.bondDepth' 8 8 1

  -- N-N order=3 rests shorter than order=2 and order=1.
  check "S3 N-N: bondR0' order=3 < bondR0' order=2" $
    Pe.bondR0' 7 7 3 < Pe.bondR0' 7 7 2
  check "S3 N-N: bondR0' order=3 < bondR0' order=1" $
    Pe.bondR0' 7 7 3 < Pe.bondR0' 7 7 1

  -- N-N order=3 De < 3 * order=1 De (diminishing pi).
  check "S3 N-N: bondDepth' order=3 < 3 * bondDepth' order=1 (diminishing pi)" $
    Pe.bondDepth' 7 7 3 < 3.0 * Pe.bondDepth' 7 7 1

  -- A double bond (O-O, order=2) formed in the builder uses R0'(order=2).
  -- After moveAtomWith with strength=0 (all bonds hold), the partner should
  -- converge toward R0'(order=2) < R0'(order=1).
  let
    s3_ooId0 = fromMaybe (-1) (map _.id (index s3_oo.atoms 0))
    s3_ooId1 = fromMaybe (-1) (map _.id (index s3_oo.atoms 1))
    s3_ooTarget = { x: 600.0, y: 0.0, z: 0.0 }
    s3_ooMoved = B.moveAtomWith 0.0 s3_ooId0 s3_ooTarget s3_oo
    s3_ooDraggedPos = fromMaybe { x: -1.0, y: -1.0, z: -1.0 }
      (map _.pos (B.atomById s3_ooMoved s3_ooId0))
    s3_ooPartnerPos = fromMaybe { x: -1.0, y: -1.0, z: -1.0 }
      (map _.pos (B.atomById s3_ooMoved s3_ooId1))
    s3_ooDist = sqrt
      ( (s3_ooDraggedPos.x - s3_ooPartnerPos.x) * (s3_ooDraggedPos.x - s3_ooPartnerPos.x)
          + (s3_ooDraggedPos.y - s3_ooPartnerPos.y) * (s3_ooDraggedPos.y - s3_ooPartnerPos.y)
          + (s3_ooDraggedPos.z - s3_ooPartnerPos.z) * (s3_ooDraggedPos.z - s3_ooPartnerPos.z)
      )

  -- The O-O bond (order=2) must survive the strength-0 drag.
  check "S3 O-O order=2 bond survives strength-0 drag to 600" $
    length s3_ooMoved.bonds == 1

  -- The partner should converge to near the order-2 rest length Pe.bondR0' 8 8 2.
  -- Convergence within bondR0Tol of the order-2 R0 (not the order-1 R0).
  check "S3 O-O order=2: partner distance converges within bondR0Tol of R0'(order=2)" $
    abs (s3_ooDist - Pe.bondR0' 8 8 2) <= bondR0Tol

  -- A triple bond (N-N, order=3) must have zero stretch energy at equilibrium.
  check "S3 N-N order=3: stretchEnergy' at R0'(order=3) == 0" $
    approxEq (Pe.stretchEnergy' 7 7 3 (Pe.bondR0' 7 7 3)) 0.0

  -- S4 update: O-O order=2 now emits 2*order = 4 bond electrons (1 sigma + 1 PI pair).
  -- S3 only affects geometry/energy, not the count formula; count changed in S4.
  check "S3 O-O order=2: bond electron count == 2*order (4 electrons, sigma+PI pair)" $
    length (B.bondElectronPositions s3_oo 0.0) == 2 *
      ( case index s3_oo.bonds 0 of
          Just bd -> bd.order
          Nothing -> 1
      )

  log "all S3 per-order geometry/energy properties hold."
