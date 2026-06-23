-- Unit tests for the constant-density nucleus packing (nucleusPackScale fix).
--
-- ROOT CAUSE: clusterPositions used a fixed outer radius
-- (rad = nucleusRadius * 0.85 * ((i+0.5)/N)^(1/3) → outer ≈ 51 model, count-independent),
-- so light nuclei showed large gaps while heavy nuclei became severely under-spaced
-- (Fe/Kr world-NN / mesh-diameter ≈ 0.27–0.34, extreme overlap).
--
-- FIX: replace frac/rad with rad = nucleusPackScale * cbrt(i+0.5), making the
-- outer radius grow as cbrt(N) — constant packing density per nucleon.
-- nucleusPackScale = 13.0 (calibrated for mean NN ≈ 1 mesh-diameter, contiguous packing).
--
-- INVARIANT GUARDS (mean/max NN, catches under-calibration at S=19.8):
--   C-12:  mean-NN/d ∈ [0.70, 1.05]  AND  max-NN/d < 1.15
--   O-16:  mean-NN/d ∈ [0.60, 0.95]  AND  max-NN/d < 1.10
--   Fe-56: mean-NN/d ∈ [0.60, 0.95]  AND  max-NN/d < 1.10
--   Kr-84: mean-NN/d ∈ [0.60, 0.95]  AND  max-NN/d < 1.10  AND  min-NN/d > 0.25
--   He-4:  mean-NN/d < 1.10 (light no longer split)
--   density band (max-min of means over C/O/Fe/Kr) < 0.30
--   outer-radius ratio Kr-84 / C-12 ∈ (1.50, 2.20)   (≈cbrt(84/12) ≈ 1.913)
--
-- At S=19.8 (old): He-4 mean≈1.47, C-12 max≈1.69, Kr max≈1.39 — all FAIL.
-- At S=13.0 (new): all guards GREEN.
--
-- Builder-world scale: worldPos = model.pos × 1.408
--   (= builderNucleusCompress 0.4 × builderScale 2.2 × intraAtomLayerSpace 1.6)
-- Mesh diameter = 31.68 = 2 × (nucleonRadius 22 × 0.72)
module Test.NucleusPackingSpec where

import Prelude

import Atom (elementOf, nucleons)
import Data.Array (foldl, index, length, mapWithIndex)
import Data.Foldable (maximum, minimum, sum)
import Data.Int (toNumber)
import Data.Maybe (fromMaybe)
import Data.Number (sqrt)
import Effect (Effect)
import Effect.Console (log)
import Test.Util (check)

-- ─── world-scale constants (must match Scene.Entities) ─────────────────────

-- builderNucleusCompress × builderScale × intraAtomLayerSpace = 0.4 × 2.2 × 1.6
worldScale :: Number
worldScale = 1.408

-- 2 × (nucleonRadius × 0.72) = 2 × (22 × 0.72) = 2 × 15.84
meshDiameter :: Number
meshDiameter = 31.68

-- ─── helpers ───────────────────────────────────────────────────────────────

dist3
  :: { x :: Number, y :: Number, z :: Number }
  -> { x :: Number, y :: Number, z :: Number }
  -> Number
dist3 a b =
  let
    dx = a.x - b.x
    dy = a.y - b.y
    dz = a.z - b.z
  in
    sqrt (dx * dx + dy * dy + dz * dz)

-- Convert model positions to world-space (apply worldScale).
toWorld
  :: Array { x :: Number, y :: Number, z :: Number }
  -> Array { x :: Number, y :: Number, z :: Number }
toWorld = map (\p -> { x: p.x * worldScale, y: p.y * worldScale, z: p.z * worldScale })

-- For each point i, compute the distance to its nearest neighbour j ≠ i.
-- Returns an array of per-point NN distances (length = n).
-- Returns [] for fewer than 2 points.
allNnDists
  :: Array { x :: Number, y :: Number, z :: Number }
  -> Array Number
allNnDists positions =
  let
    n = length positions
    getPos i = fromMaybe { x: 0.0, y: 0.0, z: 0.0 } (index positions i)
    nnForI i =
      foldl
        ( \acc j ->
            if j == i then acc
            else
              let
                d = dist3 (getPos i) (getPos j)
              in
                if d < acc then d else acc
        )
        99999.0
        (mapWithIndex (\k _ -> k) positions)
  in
    if n < 2 then []
    else map nnForI (mapWithIndex (\i _ -> i) positions)

-- Global minimum NN distance across all points (the classical min-NN).
globalMinNn
  :: Array { x :: Number, y :: Number, z :: Number }
  -> Number
globalMinNn positions =
  fromMaybe 99999.0 (minimum (allNnDists positions))

-- Arithmetic mean of the per-point NN distances.
meanNn
  :: Array { x :: Number, y :: Number, z :: Number }
  -> Number
meanNn positions =
  let
    ds = allNnDists positions
    n = length ds
  in
    if n == 0 then 99999.0
    else sum ds / toNumber n

-- Maximum of the per-point NN distances (the widest gap in the cluster).
maxNn
  :: Array { x :: Number, y :: Number, z :: Number }
  -> Number
maxNn positions =
  fromMaybe 0.0 (maximum (allNnDists positions))

-- World-space minimum NN ratio (worldMinNN / meshDiameter).
minNnRatio
  :: Array { x :: Number, y :: Number, z :: Number }
  -> Number
minNnRatio pos = globalMinNn (toWorld pos) / meshDiameter

-- World-space mean NN ratio (worldMeanNN / meshDiameter).
meanNnRatio
  :: Array { x :: Number, y :: Number, z :: Number }
  -> Number
meanNnRatio pos = meanNn (toWorld pos) / meshDiameter

-- World-space max NN ratio (worldMaxNN / meshDiameter).
maxNnRatio
  :: Array { x :: Number, y :: Number, z :: Number }
  -> Number
maxNnRatio pos = maxNn (toWorld pos) / meshDiameter

-- Maximum distance from origin in model space.
maxRadius
  :: Array { x :: Number, y :: Number, z :: Number }
  -> Number
maxRadius pos =
  fromMaybe 0.0
    ( maximum
        (map (\p -> sqrt (p.x * p.x + p.y * p.y + p.z * p.z)) pos)
    )

-- ─── nucleon clusters under test ───────────────────────────────────────────

-- Retrieve the nucleon position cluster for a given element Z.
-- elementOf uses standard neutron table so A = protons + neutrons is realistic.
nucleonPositions :: Int -> Array { x :: Number, y :: Number, z :: Number }
nucleonPositions z = map _.pos (nucleons (elementOf z))

-- He-4: Z=2 → 2p+2n = 4 nucleons
he4Pos :: Array { x :: Number, y :: Number, z :: Number }
he4Pos = nucleonPositions 2

-- C-12: Z=6 → 6p+6n = 12 nucleons
c12Pos :: Array { x :: Number, y :: Number, z :: Number }
c12Pos = nucleonPositions 6

-- O-16: Z=8 → 8p+8n = 16 nucleons
o16Pos :: Array { x :: Number, y :: Number, z :: Number }
o16Pos = nucleonPositions 8

-- Fe-56: Z=26 → 26p+30n = 56 nucleons
fe56Pos :: Array { x :: Number, y :: Number, z :: Number }
fe56Pos = nucleonPositions 26

-- Kr-84: Z=36 → 36p+48n = 84 nucleons
kr84Pos :: Array { x :: Number, y :: Number, z :: Number }
kr84Pos = nucleonPositions 36

-- ─── spec ──────────────────────────────────────────────────────────────────

nucleusPackingSpec :: Effect Unit
nucleusPackingSpec = do
  log "nucleus constant-density packing properties (mean/max NN guards):"

  let
    heMean = meanNnRatio he4Pos
    c12Mean = meanNnRatio c12Pos
    c12Max = maxNnRatio c12Pos
    o16Mean = meanNnRatio o16Pos
    o16Max = maxNnRatio o16Pos
    fe56Mean = meanNnRatio fe56Pos
    fe56Max = maxNnRatio fe56Pos
    kr84Min = minNnRatio kr84Pos
    kr84Mean = meanNnRatio kr84Pos
    kr84Max = maxNnRatio kr84Pos

  -- ── Guard 1: C-12 mean and max NN ─────────────────────────────────────────
  -- S=13: mean≈0.875, max≈1.112.  S=19.8: mean≈1.332, max≈1.694 → FAIL.
  log "  C-12 mean/max NN:"

  check "C-12 mean-NN/d >= 0.70 (contiguous, not degenerate)" $
    c12Mean >= 0.70

  check "C-12 mean-NN/d <= 1.05 (not excessively gapped)" $
    c12Mean <= 1.05

  check "C-12 max-NN/d < 1.15 (no large interior gaps)" $
    c12Max < 1.15

  -- ── Guard 2: O-16 mean and max NN ─────────────────────────────────────────
  -- S=13: mean≈0.847, max≈1.099.  S=19.8: mean≈1.289, max≈1.674 → FAIL.
  log "  O-16 mean/max NN:"

  check "O-16 mean-NN/d >= 0.60 (contiguous)" $
    o16Mean >= 0.60

  check "O-16 mean-NN/d <= 0.95 (not excessively gapped)" $
    o16Mean <= 0.95

  check "O-16 max-NN/d < 1.10 (no large interior gaps)" $
    o16Max < 1.10

  -- ── Guard 3: Fe-56 mean and max NN ────────────────────────────────────────
  -- S=13: mean≈0.737, max≈0.961.  S=19.8: mean≈1.122, max≈1.464 → FAIL.
  log "  Fe-56 mean/max NN:"

  check "Fe-56 mean-NN/d >= 0.60 (contiguous)" $
    fe56Mean >= 0.60

  check "Fe-56 mean-NN/d <= 0.95 (not excessively gapped)" $
    fe56Mean <= 0.95

  check "Fe-56 max-NN/d < 1.10 (no large interior gaps)" $
    fe56Max < 1.10

  -- ── Guard 4: Kr-84 mean, max, and min NN ──────────────────────────────────
  -- S=13: min≈0.297, mean≈0.710, max≈0.911.
  -- S=19.8: mean≈1.081, max≈1.387 → max guard FAILS (1.387 > 1.10).
  log "  Kr-84 mean/max/min NN:"

  check "Kr-84 mean-NN/d >= 0.60 (contiguous)" $
    kr84Mean >= 0.60

  check "Kr-84 mean-NN/d <= 0.95 (not excessively gapped)" $
    kr84Mean <= 0.95

  check "Kr-84 max-NN/d < 1.10 (no large interior gaps)" $
    kr84Max < 1.10

  check "Kr-84 min-NN/d > 0.25 (heavy nuclei still distinct, not catastrophically merged)" $
    kr84Min > 0.25

  -- ── Guard 5: He-4 mean NN (light nucleus no longer split) ─────────────────
  -- S=13: mean≈0.969, max≈1.104.  S=19.8: mean≈1.475 → FAIL (1.475 > 1.10).
  log "  He-4 light nucleus packing:"

  check "He-4 mean-NN/d < 1.10 (light nucleus is contiguous, not split)" $
    heMean < 1.10

  -- ── Guard 6: density band (max-min of means over C/O/Fe/Kr) < 0.30 ────────
  -- S=13: max=0.875, min=0.710, range=0.165.
  -- S=19.8: max=1.332, min=1.081, range=0.251 (< 0.30 but max guard fails).
  log "  density constant-ness (mean-NN band):"

  let
    means = [ c12Mean, o16Mean, fe56Mean, kr84Mean ]
    maxMean = fromMaybe 0.0 (maximum means)
    minMean = fromMaybe 99999.0 (minimum means)
    densityBand = maxMean - minMean

  check "mean-NN band (max-min over C/O/Fe/Kr) < 0.30 (constant density)" $
    densityBand < 0.30

  -- ── Guard 7: outer-radius grows as cbrt(A) — the core cbrt fix ────────────
  -- Both S=13 and S=19.8 pass this (ratio ≈ 1.936 for both).
  -- Retains the fundamental cbrt(A) scaling invariant.
  log "  outer-radius scales with cbrt(A):"

  let
    mrKr = maxRadius kr84Pos
    mrC = maxRadius c12Pos
    radiusRatio = mrKr / (mrC + 1.0e-10)

  check "maxRadius(Kr-84) / maxRadius(C-12) > 1.50 (outer radius grows with A)" $
    radiusRatio > 1.50

  check "maxRadius(Kr-84) / maxRadius(C-12) < 2.20 (outer radius growth is cbrt, not linear)" $
    radiusRatio < 2.20

  -- ── Guard 8: single-nucleon guard (H-1 has only 1 nucleon, pos = origin) ──
  log "  single-nucleon guard:"

  let
    hPos = nucleonPositions 1

  check "H-1 nucleus has exactly 1 nucleon" $
    length hPos == 1

  check "H-1 single nucleon is at the origin" $
    let
      p = fromMaybe { x: 9999.0, y: 0.0, z: 0.0 } (index hPos 0)
    in
      p.x == 0.0 && p.y == 0.0 && p.z == 0.0

  log "all nucleus constant-density packing properties hold."
