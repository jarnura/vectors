-- Pure curated-structure registry for the Materials scene.
-- Emits deterministic all-carbon `BuilderState` values for known crystal
-- structures (Diamond sp³, Graphene sp²). No Effect, no WebGL.
--
-- Coordinate convention: MODEL units (the renderer applies builderScale = 2.2
-- after the fact — generators must NOT pre-scale). Chosen C–C nearest-neighbour
-- distance: **cc = 165** model units, satisfying:
--   minSeparation(6,6) = 130 < cc = 165 < bondThreshold = 180
-- 2nd-neighbour distances all exceed bondThreshold (no spurious bonds):
--   Diamond  2nd-nbr ≈ cc * √(8/3) ≈ 269  > 180  ✓
--   Graphene 2nd-nbr = cc * √3       ≈ 286  > 180  ✓
module Lattice
  ( CuratedStructure
  , structures
  , structureOf
  ) where

import Prelude

import Builder.Types (BBond, BuilderState, PlacedAtom, emptyBuilder)
import Data.Array (concatMap, foldl, index, length, mapWithIndex, range)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Number (sqrt)

-- ─── Public type ─────────────────────────────────────────────────────────────

-- A curated crystal-structure entry: human metadata plus a ready-made
-- BuilderState the renderer can consume directly.
type CuratedStructure =
  { name :: String
  , formula :: String
  , hybridization :: String
  , properties :: Array { label :: String, value :: String }
  , build :: BuilderState
  }

-- ─── Constants ───────────────────────────────────────────────────────────────

-- Chosen C–C nearest-neighbour distance (model units). Must satisfy:
--   minSeparation(6,6) = 130 < cc < bondThreshold = 180
-- giving a safe margin on both sides.
cc :: Number
cc = 165.0

-- Tolerance for the proximity-bond algorithm: bond a pair iff
--   |distance − cc| ≤ bondTol
-- At 2.0 this is tight enough to exclude 2nd neighbours (≥269 for diamond,
-- ≥286 for graphene) while tolerating floating-point drift (actual drift < 1e-10).
bondTol :: Number
bondTol = 2.0

-- ─── Shared helpers ──────────────────────────────────────────────────────────

type V3 = { x :: Number, y :: Number, z :: Number }

-- Euclidean distance between two raw positions.
dist3 :: V3 -> V3 -> Number
dist3 p q =
  let
    dx = p.x - q.x
    dy = p.y - q.y
    dz = p.z - q.z
  in
    sqrt (dx * dx + dy * dy + dz * dz)

absNum :: Number -> Number
absNum x = if x < 0.0 then -x else x

mkPlacedAtom :: Int -> Int -> V3 -> PlacedAtom
mkPlacedAtom aid z p = { id: aid, z, pos: p }

mkBond :: Int -> Int -> BBond
mkBond a b = { a, b, order: 1 }

-- Given a list of positions, assign sequential ids and produce PlacedAtoms
-- (all carbon, z=6) plus proximity bonds: bond iff |dist−cc| ≤ bondTol.
-- The proximity-bond approach replaces all hand-listed bonds and scales to
-- any supercell size automatically.
proximityBuild :: Array V3 -> BuilderState
proximityBuild positions =
  let
    atoms :: Array PlacedAtom
    atoms = mapWithIndex (\i p -> mkPlacedAtom i 6 p) positions

    n = length atoms

    -- Generate bonds for every unordered pair (i<j) where |dist-cc| ≤ bondTol.
    -- Using range indices for clarity and determinism.
    bonds :: Array BBond
    bonds = concatMap
      ( \i -> concatMap
          ( \j ->
              if j <= i then []
              else
                case index atoms i, index atoms j of
                  Just ai, Just aj ->
                    if absNum (dist3 ai.pos aj.pos - cc) <= bondTol
                    then [ mkBond ai.id aj.id ]
                    else []
                  _, _ -> []
          )
          (range 0 (n - 1))
      )
      (range 0 (n - 1))
  in
    emptyBuilder
      { atoms = atoms
      , bonds = bonds
      , nextId = n
      , picked = Nothing
      }

-- ─── Diamond generator ───────────────────────────────────────────────────────
-- A 2×2×2 supercell of the diamond-cubic conventional unit cell (64 atoms),
-- centered at the origin. The conventional cell has lattice constant
--   a = 4·cc/√3  ≈ 381 model units
-- and contains 8 atoms: 4 FCC A-sites + 4 FCC B-sites (offset by a/4).
--
-- Supercell: ix,iy,iz ∈ {0,1} → 8 cells × 8 atoms = 64 atoms.
-- Center: shift all positions by (a, a, a) so the structure straddles origin.
-- Bounding radius ≈ a·√3 ≈ 660 model units.
--
-- Interior (degree-4) atoms: every B-site of type (a/4,a/4,a/4) has all four
-- A-site partners within the SAME conventional cell (independent of ix,iy,iz),
-- so all 8 such B-sites are degree 4. A-sites at interior cells (ix=1,iy=1,iz=1)
-- also achieve degree 4. This gives ≥ 27 interior atoms with degree 4.
--
-- Diamond count (64) differs from Graphene count (32), enabling structural
-- distinctness checks in tests and the E2E gallery.
--
-- Proximity bonds: bond iff |distance − cc| ≤ bondTol.
-- Diamond 2nd-neighbour ≈ cc·√(8/3) ≈ 269 >> 167 → no spurious bonds.

diamondPositions :: Array V3
diamondPositions =
  let
    a = 4.0 * cc / sqrt 3.0
    half = a / 2.0
    quarter = a / 4.0
    threeQ = 3.0 * a / 4.0

    -- 8 basis atoms per conventional cell:
    --   FCC A-sites (4): (0,0,0) (a/2,a/2,0) (a/2,0,a/2) (0,a/2,a/2)
    --   FCC B-sites (4): (a/4,a/4,a/4) (3a/4,3a/4,a/4) (3a/4,a/4,3a/4) (a/4,3a/4,3a/4)
    basis :: Array V3
    basis =
      [ { x: 0.0,     y: 0.0,     z: 0.0     }  -- A
      , { x: half,    y: half,    z: 0.0     }  -- A
      , { x: half,    y: 0.0,     z: half    }  -- A
      , { x: 0.0,     y: half,    z: half    }  -- A
      , { x: quarter, y: quarter, z: quarter }  -- B (always degree-4 in own cell)
      , { x: threeQ,  y: threeQ,  z: quarter }  -- B
      , { x: threeQ,  y: quarter, z: threeQ  }  -- B
      , { x: quarter, y: threeQ,  z: threeQ  }  -- B
      ]

    -- 2×2×2 supercell: ix,iy,iz ∈ {0,1} → 8 cells
    cellOffsets :: Array { ox :: Number, oy :: Number, oz :: Number }
    cellOffsets =
      [ { ox: 0.0, oy: 0.0, oz: 0.0 }
      , { ox: 0.0, oy: 0.0, oz: a   }
      , { ox: 0.0, oy: a,   oz: 0.0 }
      , { ox: 0.0, oy: a,   oz: a   }
      , { ox: a,   oy: 0.0, oz: 0.0 }
      , { ox: a,   oy: 0.0, oz: a   }
      , { ox: a,   oy: a,   oz: 0.0 }
      , { ox: a,   oy: a,   oz: a   }
      ]

    -- Center: the 2×2×2 block spans [0..2a] in each dimension; center at (a,a,a).
    cx = a
    cy = a
    cz = a
  in
    concatMap
      ( \cell ->
          map
            ( \b ->
                { x: cell.ox + b.x - cx
                , y: cell.oy + b.y - cy
                , z: cell.oz + b.z - cz
                }
            )
            basis
      )
      cellOffsets

diamondBuild :: BuilderState
diamondBuild = proximityBuild diamondPositions

diamond :: CuratedStructure
diamond =
  { name: "Diamond"
  , formula: "C"
  , hybridization: "sp³"
  , properties:
      [ { label: "Coordination", value: "4 (interior)" }
      , { label: "Bond", value: "C–C ~154 pm" }
      , { label: "Atoms", value: "64 (2×2×2 supercell)" }
      , { label: "Note", value: "Hardest natural material; each interior C bonded to 4 others" }
      ]
  , build: diamondBuild
  }

-- ─── Graphene generator ──────────────────────────────────────────────────────
-- A 4×4 supercell of the 2-atom hexagonal unit cell (32 atoms total, all z=0),
-- centered at the origin. Interior atoms are fully sp²-coordinated (degree 3).
--
-- Hexagonal lattice vectors (armchair orientation, C–C = cc):
--   a1 = (cc·√3, 0)
--   a2 = (cc·√3/2, 3·cc/2)
-- Two-atom basis:
--   A at (0, 0)
--   B at (0, cc)   [B is directly above A along the armchair axis]
--
-- 4×4 supercell: n ∈ {0,1,2,3}, m ∈ {0,1,2,3} → 16 cells × 2 atoms = 32 atoms.
-- Center: subtract the centroid of all positions (pure arithmetic, exact).
--
-- Interior atoms: those whose 3 bonded neighbours are all within the sheet.
-- In a 4×4 sheet ≥ 6 interior atoms have degree 3 (sp² fully coordinated).
--
-- Proximity bonds: bond iff |distance − cc| ≤ bondTol.
-- Graphene 2nd-neighbour = cc·√3 ≈ 286 >> 167 → no spurious bonds.

graphenePositions :: Array V3
graphenePositions =
  let
    sq3 = sqrt 3.0
    a1x = cc * sq3         -- lattice vector a1: (a1x, 0)
    a2x = cc * sq3 / 2.0   -- lattice vector a2: (a2x, a2y)
    a2y = 3.0 * cc / 2.0

    -- 4×4 supercell: n,m ∈ {0,1,2,3}
    ns = [ 0, 1, 2, 3 ]
    ms = [ 0, 1, 2, 3 ]

    toN :: Int -> Number
    toN k = case k of
      0 -> 0.0
      1 -> 1.0
      2 -> 2.0
      3 -> 3.0
      _ -> 0.0

    -- All 32 raw positions (before centering).
    rawPositions :: Array V3
    rawPositions = concatMap
      ( \n ->
          concatMap
            ( \m ->
                let
                  fn = toN n
                  fm = toN m
                  ax = fn * a1x + fm * a2x
                  ay = fm * a2y
                in
                  [ { x: ax,        y: ay,      z: 0.0 }  -- A-site
                  , { x: ax,        y: ay + cc, z: 0.0 }  -- B-site
                  ]
            )
            ms
      )
      ns

    -- Compute centroid and center the sheet.
    nNum = foldl (\acc _ -> acc + 1.0) 0.0 rawPositions
    sumX = foldl (\acc p -> acc + p.x) 0.0 rawPositions
    sumY = foldl (\acc p -> acc + p.y) 0.0 rawPositions
    cx = sumX / nNum
    cy = sumY / nNum
  in
    map (\p -> { x: p.x - cx, y: p.y - cy, z: 0.0 }) rawPositions

grapheneBuild :: BuilderState
grapheneBuild = proximityBuild graphenePositions

graphene :: CuratedStructure
graphene =
  { name: "Graphene"
  , formula: "C"
  , hybridization: "sp²"
  , properties:
      [ { label: "Coordination", value: "3 (interior)" }
      , { label: "Bond", value: "C–C ~142 pm" }
      , { label: "Atoms", value: "32 (4×4 supercell)" }
      , { label: "Note", value: "Single-atom-thick carbon sheet; each interior C bonded to 3 others" }
      ]
  , build: grapheneBuild
  }

-- ─── Registry ────────────────────────────────────────────────────────────────

-- The ordered registry of curated structures. Diamond is index 0, Graphene
-- index 1. Adding a 3rd entry requires only a new record here — no renderer or
-- FFI changes needed.
structures :: Array CuratedStructure
structures = [ diamond, graphene ]

-- Clamp-safe structure lookup: out-of-range indices are clamped into
-- [0, length structures − 1] so callers never get a crash or a Maybe.
structureOf :: Int -> CuratedStructure
structureOf raw =
  let
    n = length structures
    i
      | raw < 0 = 0
      | raw >= n = n - 1
      | otherwise = raw
  in
    fromMaybe diamond (index structures i)
