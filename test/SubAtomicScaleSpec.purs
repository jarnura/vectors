-- Unit tests for the sub-atomic scale-invariance fix (builderDetailPlaceWith).
--
-- ROOT CAUSE: builderDetailPlaceWith multiplied the ENTIRE model position by
-- S = builderScale × ls, so intra-atom offsets (nucleon cluster, electron rings)
-- ballooned with layerSpace while fixed mesh radii don't.
--
-- FIX: the atom CENTRE scales by S = builderScale × ls; the LOCAL offset
-- (full − center) scales by the FIXED builderScale only.
--
-- These guards verify the INVARIANT (RED on broken code, GREEN after fix):
--   1. Nucleon world-positions relative to the atom centre must be ls-independent.
--      nnSep(ls=1.6) ≈ nnSep(ls=1.0) within 15%; nnSep(ls=4.0) ≈ nnSep(ls=1.0).
--      BROKEN: nnSep scales proportionally with ls (nnSep_16 ≈ 1.6 × nnSep_10).
--
--   2. Electron ring world radius must equal loneOrbitRadius × builderScale × intraAtomLayerSpace = 295.68
--      for all ls ∈ {1.0, 1.6, 4.0} (ls-independent).
--      BROKEN: world ring radius = loneOrbitRadius × builderScale × ls.
module Test.SubAtomicScaleSpec where

import Prelude

import Atom as Atom
import Data.Array (foldl, length, index, mapWithIndex)
import Data.Foldable (maximum)
import Data.Maybe (fromMaybe)
import Data.Number (abs, sqrt)
import Effect (Effect)
import Effect.Console (log)
import Math.Matrix as M
import Scene.Entities
  ( builderDetailPlaceWith
  , builderNucleusCompress
  , builderScale
  , intraAtomLayerSpace
  )
import Test.Util (check)

-- ──────────────────────────────────────────────────────────────────────────────
-- Helpers
-- ──────────────────────────────────────────────────────────────────────────────

-- Extract the translation column (world position) from a 4×4 model matrix.
-- builderDetailPlaceWith returns translate(pos) × scale(sc), so in the row-major
-- flat array the translation is at indices 3 (x), 7 (y), 11 (z).
extractTranslation :: M.Matrix Number -> { x :: Number, y :: Number, z :: Number }
extractTranslation m =
  let
    v = M.toVector m
    at i = fromMaybe 0.0 (index v i)
  in
    { x: at 3, y: at 7, z: at 11 }

-- Euclidean distance between two vec3s.
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

-- Nearest-neighbour separation: the minimum distance between any two distinct
-- points in the array. Returns a large sentinel (99999) for fewer than 2 points.
-- Uses a fold with a running accumulator to avoid generating the full pair list.
nnSepOf :: Array { x :: Number, y :: Number, z :: Number } -> Number
nnSepOf positions =
  let
    n = length positions
    getPos i = fromMaybe { x: 0.0, y: 0.0, z: 0.0 } (index positions i)

    -- For each point i, find the minimum distance to all points j>i.
    minFromI i =
      if i + 1 >= n then 99999.0
      else
        let
          pi_ = getPos i
          -- Fold over all j > i, tracking the running minimum.
          go j acc =
            if j >= n then acc
            else
              let
                d = dist3 pi_ (getPos j)
              in
                go (j + 1) (if d < acc then d else acc)
        in
          go (i + 1) 99999.0

    -- Fold over all i, picking the global minimum across all i.
    result = foldl (\acc i -> let m = minFromI i in if m < acc then m else acc) 99999.0 (mapWithIndex (\i _ -> i) positions)
  in
    result

-- ──────────────────────────────────────────────────────────────────────────────
-- Nucleon world positions at d=1.0 for a Carbon atom at the origin.
-- ──────────────────────────────────────────────────────────────────────────────

-- Carbon Z=6: 6 protons + 6 neutrons = 12 nucleons.
carbonNucleons :: Array Atom.Particle
carbonNucleons = Atom.nucleons (Atom.elementOf 6)

-- World position of a nucleon using builderDetailPlaceWith at d=1.0.
-- center = {0,0,0}; full = n.pos × builderNucleusCompress.
-- FIXED:  world = n.pos × builderNucleusCompress × builderScale   (ls-independent)
-- BROKEN: world = n.pos × builderNucleusCompress × builderScale × ls
nucleonWorldPos :: Number -> Atom.Particle -> { x :: Number, y :: Number, z :: Number }
nucleonWorldPos ls nucleon =
  let
    center = { x: 0.0, y: 0.0, z: 0.0 }
    full =
      { x: center.x + nucleon.pos.x * builderNucleusCompress
      , y: center.y + nucleon.pos.y * builderNucleusCompress
      , z: center.z + nucleon.pos.z * builderNucleusCompress
      }
    m = builderDetailPlaceWith ls 1.0 center full
  in
    extractTranslation m

-- Nearest-neighbour separation of the Carbon nucleon cluster at a given ls.
nnSepAtLs :: Number -> Number
nnSepAtLs ls = nnSepOf (map (nucleonWorldPos ls) carbonNucleons)

-- ──────────────────────────────────────────────────────────────────────────────
-- Electron ring world radius at d=1.0 for an atom at the origin.
-- ──────────────────────────────────────────────────────────────────────────────

-- loneOrbitRadius = nucleusRadius × 1.4 = 60 × 1.4 = 84.
loneOrbitRadius :: Number
loneOrbitRadius = Atom.nucleusRadius * 1.4

-- Expected ls-independent world ring radius = 84 × 2.2 × 1.6 = 295.68.
-- The offset is frozen at builderScale × intraAtomLayerSpace so the DEFAULT view
-- (ls=1.6) is byte-identical to the historic render.
expectedWorldRingRadius :: Number
expectedWorldRingRadius = loneOrbitRadius * builderScale * intraAtomLayerSpace

-- World +Y radius of an electron placed at model offset loneOrbitRadius along +Y.
-- full = {0, 84, 0}; center = {0, 0, 0}.
-- FIXED:  world y = 84 × builderScale × intraAtomLayerSpace = 295.68  (regardless of ls)
-- BROKEN: world y = 84 × builderScale × ls                            (varies with ls)
electronWorldRingRadius :: Number -> Number
electronWorldRingRadius ls =
  let
    center = { x: 0.0, y: 0.0, z: 0.0 }
    full = { x: 0.0, y: loneOrbitRadius, z: 0.0 }
    m = builderDetailPlaceWith ls 1.0 center full
    wp = extractTranslation m
  in
    abs wp.y

-- Max nucleon world distance from the atom center (used for clearance check).
maxNucleonWorldDist :: Number -> Number
maxNucleonWorldDist ls =
  let
    positions = map (nucleonWorldPos ls) carbonNucleons
    dists = map (dist3 { x: 0.0, y: 0.0, z: 0.0 }) positions
  in
    fromMaybe 0.0 (maximum dists)

-- ──────────────────────────────────────────────────────────────────────────────
-- Scale-invariance tolerance: 15% relative.
-- ──────────────────────────────────────────────────────────────────────────────

-- True if |a - b| / |b| < 15%.
withinRel :: Number -> Number -> Boolean
withinRel a b = abs (a - b) / (abs b + 1.0e-10) < 0.15

-- ──────────────────────────────────────────────────────────────────────────────
-- The spec
-- ──────────────────────────────────────────────────────────────────────────────

subAtomicScaleSpec :: Effect Unit
subAtomicScaleSpec = do
  log "sub-atomic scale-invariance (builderDetailPlaceWith fix) properties:"

  let
    nnSep10 = nnSepAtLs 1.0
    nnSep16 = nnSepAtLs 1.6
    nnSep40 = nnSepAtLs 4.0

    ringR10 = electronWorldRingRadius 1.0
    ringR16 = electronWorldRingRadius 1.6
    ringR40 = electronWorldRingRadius 4.0

  -- ── Guard 1: Nucleon world-size invariance ─────────────────────────────────
  log "  nucleon nearest-neighbour separation invariance:"

  -- Baseline: Carbon nucleus must produce a non-degenerate cluster (nnSep > 0).
  check "nnSep(ls=1.0) > 0 (non-degenerate Carbon nucleus cluster)" $
    nnSep10 > 0.0

  -- nnSep must be ls-independent within 15% relative.
  -- RED today (broken): nnSep_16 ≈ 1.6 × nnSep_10 (≈60% deviation from ls=1).
  check "nnSep(ls=1.6) ≈ nnSep(ls=1.0) within 15% (scale-invariant)" $
    withinRel nnSep16 nnSep10

  check "nnSep(ls=4.0) ≈ nnSep(ls=1.0) within 15% (scale-invariant)" $
    withinRel nnSep40 nnSep10

  -- ── Guard 2: Inner electron-ring world-radius invariance ──────────────────
  log "  electron ring world-radius invariance:"

  -- Ring radius must be ≈ expectedWorldRingRadius (295.68) within 15%.
  -- Frozen at builderScale × intraAtomLayerSpace so ls-independent and
  -- DEFAULT view (ls=1.6) is byte-identical to the historic render.
  check "world ring radius at ls=1.0 ≈ 295.68 (loneOrbitRadius × builderScale × intraAtomLayerSpace)" $
    withinRel ringR10 expectedWorldRingRadius

  check "world ring radius at ls=1.6 ≈ 295.68 (ls-independent, default view)" $
    withinRel ringR16 expectedWorldRingRadius

  check "world ring radius at ls=4.0 ≈ 295.68 (ls-independent after fix)" $
    withinRel ringR40 expectedWorldRingRadius

  -- Clearance: ring radius > max nucleon world distance from center.
  -- This ensures electrons orbit OUTSIDE the nucleus at every ls.
  check "ring clearance at ls=1.0: electron ring > max nucleon dist" $
    ringR10 > maxNucleonWorldDist 1.0

  check "ring clearance at ls=1.6: electron ring > max nucleon dist" $
    ringR16 > maxNucleonWorldDist 1.6

  check "ring clearance at ls=4.0: electron ring > max nucleon dist" $
    ringR40 > maxNucleonWorldDist 4.0

  log "all sub-atomic scale-invariance properties verified."
