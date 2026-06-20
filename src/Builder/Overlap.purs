-- Pauli-exclusion minimum-separation constraint model for the Builder.
--
-- Physics: the Pauli exclusion principle forbids the FILLED electron shells of
-- two atoms from interpenetrating — two atoms can never collapse onto each
-- other, so their centres always keep a hard minimum distance. The covalent
-- shared bonding pair is the one ALLOWED overlap: bonded atoms may sit close
-- (anywhere below bondThreshold), just never below the contact floor. The
-- solver below only moves atom CENTRES; the shared electron-pair rendering and
-- all bond logic are untouched. Pure, total, deterministic — no Effect/WebGL.
module Builder.Overlap
  ( contactFactor
  , absoluteMin
  , floorMargin
  , floorCeil
  , relaxPasses
  , minSeparation
  , resolveOverlaps
  ) where

import Prelude

import Atom (atomicRadius)
import Builder.Geom
  ( alongFrom
  , atomById
  , bondThreshold
  , distance
  , idPairs
  , negV3
  , separationDir
  , setAtomPos
  )
import Builder.Types (BuilderState)
import Data.Array (foldl, range)
import Data.Foldable (elem)
import Data.Maybe (Maybe(..))

-- Scale from the summed normalised covalent radii (Atom.atomicRadius) of a
-- pair to its world-unit contact floor.
contactFactor :: Number
contactFactor = 55.0

-- Hard non-collapse floor (world units): whatever the elements, nuclei /
-- filled shells can never interpenetrate closer than this.
absoluteMin :: Number
absoluteMin = 130.0

-- Safety margin keeping every contact floor strictly below bondThreshold.
floorMargin :: Number
floorMargin = 15.0

-- Ceiling on the contact floor (= 165.0 = bondThreshold − floorMargin): EVERY
-- pair's floor stays strictly below bondThreshold (180), so valence
-- auto-bonding keeps working for any element pair, however large.
floorCeil :: Number
floorCeil = bondThreshold - floorMargin

-- Clamp a raw radius-derived floor into [absoluteMin, floorCeil].
clampFloor :: Number -> Number
clampFloor x = max absoluteMin (min floorCeil x)

-- Minimum allowed centre distance between atoms of atomic numbers z1/z2:
-- proportional to the sum of their normalised covalent radii, clamped so it
-- never drops below the hard floor nor reaches bondThreshold. Symmetric.
minSeparation :: Int -> Int -> Number
minSeparation z1 z2 =
  clampFloor (contactFactor * (atomicRadius z1 + atomicRadius z2))

-- FIXED number of relaxation passes: the solver is bounded (no convergence
-- loop), so it always terminates; 10 passes settle small clusters.
relaxPasses :: Int
relaxPasses = 10

-- Anchor-aware bounded relaxation enforcing `minSeparation` between every atom
-- pair. Runs exactly `relaxPasses` Gauss-Seidel passes; each pass walks all
-- unordered id pairs in ascending (a, b) order (the same `idPairs` walk
-- `recomputeBonds` uses) and projects each violating pair back onto its floor:
--   * d >= floor          → no change (valid states are fixed points, so the
--                           solver is idempotent — no drag jitter);
--   * both ids anchored   → SKIP (documented limitation: an intra-rigid-
--                           component overlap is left to the component itself);
--   * exactly one anchor  → push ONLY the non-anchor along the centre line,
--                           away from the anchor, landing exactly at the floor;
--   * neither anchored    → push BOTH apart symmetrically by (floor − d)/2
--                           each along the centre line.
-- Atom array order and ids are preserved (only `pos` changes); bonds are NOT
-- recomputed here (M1 is the pure model only). Pure, total, deterministic.
resolveOverlaps :: Array Int -> BuilderState -> BuilderState
resolveOverlaps anchors st0 = foldl (\s _ -> relaxPass s) st0 (range 1 relaxPasses)
  where
  relaxPass s = foldl (separatePair anchors) s (idPairs s)

-- Project one id pair back onto its separation floor (one Gauss-Seidel step).
-- Pairs that already satisfy the floor, fully-anchored pairs, and pairs with a
-- missing endpoint are returned unchanged.
separatePair :: Array Int -> BuilderState -> { a :: Int, b :: Int } -> BuilderState
separatePair anchors s p =
  case atomById s p.a, atomById s p.b of
    Just pa, Just pb ->
      let
        floorDist = minSeparation pa.z pb.z
        d = distance pa.pos pb.pos
        aFixed = elem p.a anchors
        bFixed = elem p.b anchors
      in
        if d >= floorDist || (aFixed && bFixed) then s
        else
          let
            -- Unit direction from atom a toward atom b (id-derived tie-break
            -- when coincident, so no division by zero / NaN is possible).
            dir = separationDir pa pb d
          in
            if aFixed then setAtomPos p.b (alongFrom pa.pos dir floorDist) s
            else if bFixed then setAtomPos p.a (alongFrom pb.pos (negV3 dir) floorDist) s
            else
              let
                push = (floorDist - d) / 2.0
              in
                setAtomPos p.b (alongFrom pb.pos dir push)
                  (setAtomPos p.a (alongFrom pa.pos (negV3 dir) push) s)
    _, _ -> s
