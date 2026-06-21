-- Shared geometry primitives and threshold constants for the Builder.* model:
-- distances, atom lookup, the deterministic candidate-pair walk, separation
-- directions, and small vector/matrix helpers used across Spawn / Overlap /
-- Bonds / Electrons and the facade. Pure, total, deterministic — no Effect/WebGL.
module Builder.Geom
  ( bondThreshold
  , breakThreshold
  , coincidentEps
  , distance
  , atomById
  , distById
  , degreeIn
  , idPairs
  , separationDir
  , tieBreakDir
  , alongFrom
  , negV3
  , setAtomPos
  , clampUnit
  , mulVec
  , entry
  ) where

import Prelude

import Atom (V3)
import Builder.Types (BBond, BuilderState, PlacedAtom)
import Data.Array (filter, foldl, index, length, range, sortBy, (!!))
import Data.Int (toNumber)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Number (cos, sin, sqrt)
import Math.Matrix (Matrix, multiply, fromColumn, toVector)

-- Distance below which a fresh (unbonded) pair will FORM a bond.
bondThreshold :: Number
bondThreshold = 180.0

-- Distance above which an EXISTING bond breaks. Strictly greater than
-- bondThreshold, giving a hysteresis band where bonds persist but don't form.
breakThreshold :: Number
breakThreshold = 230.0

-- Below this pair distance the separation direction is degenerate (coincident
-- atoms) and the deterministic id-derived tie-break direction is used instead.
coincidentEps :: Number
coincidentEps = 1.0e-9

-- Euclidean distance between two world points.
distance :: V3 -> V3 -> Number
distance a b =
  let
    dx = a.x - b.x
    dy = a.y - b.y
    dz = a.z - b.z
  in
    sqrt (dx * dx + dy * dy + dz * dz)

-- Look up an atom by id.
atomById :: BuilderState -> Int -> Maybe PlacedAtom
atomById st aid = index (filter (\a -> a.id == aid) st.atoms) 0

-- Distance between two atoms (by id); large sentinel if either is missing.
distById :: BuilderState -> Int -> Int -> Number
distById st x y =
  case atomById st x, atomById st y of
    Just a, Just b -> distance a.pos b.pos
    _, _ -> 1.0e18

-- Current degree of `aid` in a bond set: the SUM of incident bond orders.
-- At order=1 (all bonds in this step) this equals the old incident-bond count,
-- so freeValence and lone-electron counts are unchanged. When higher orders are
-- introduced in future M2 steps, degree will correctly exceed bond count.
degreeIn :: Array BBond -> Int -> Int
degreeIn bonds aid =
  foldl (\acc bd -> if bd.a == aid || bd.b == aid then acc + bd.order else acc) 0 bonds

-- All unordered atom-id pairs, in ascending (a, b) id order for determinism.
-- The single candidate-pair walk shared by `recomputeBonds` (bond formation)
-- and `resolveOverlaps` (separation constraints).
idPairs :: BuilderState -> Array { a :: Int, b :: Int }
idPairs st = do
  i <- range 0 (length ids - 1)
  j <- range 0 (length ids - 1)
  if j <= i then []
  else case ids !! i, ids !! j of
    Just a, Just b -> [ { a, b } ]
    _, _ -> []
  where
  ids = sortBy compare (map _.id st.atoms)

-- Unit separation direction from `pa` toward `pb` given their distance `d`
-- (passed in so it is computed once). A coincident pair (d < coincidentEps)
-- has no direction, so a deterministic tie-break direction is derived from the
-- pair ids instead — reproducible run-to-run, never NaN, never zero-length.
separationDir :: PlacedAtom -> PlacedAtom -> Number -> V3
separationDir pa pb d
  | d < coincidentEps = tieBreakDir pa.id pb.id
  | otherwise =
      { x: (pb.pos.x - pa.pos.x) / d
      , y: (pb.pos.y - pa.pos.y) / d
      , z: (pb.pos.z - pa.pos.z) / d
      }

-- Deterministic unit direction for a coincident pair: +x rotated in the XY
-- plane by an id-derived angle, so distinct coincident pairs fan out in
-- distinct directions while every run produces identical results. cos/sin of
-- a finite number is always finite, so the result is NaN-free by construction.
tieBreakDir :: Int -> Int -> V3
tieBreakDir a b =
  let
    theta = toNumber (a * 7 + b * 13) * 0.61803398875
  in
    { x: cos theta, y: sin theta, z: 0.0 }

-- The point `dist` world units from `origin` along the unit direction `dir`.
alongFrom :: V3 -> V3 -> Number -> V3
alongFrom origin dir dist =
  { x: origin.x + dir.x * dist
  , y: origin.y + dir.y * dist
  , z: origin.z + dir.z * dist
  }

-- Negate a vector (flip a direction).
negV3 :: V3 -> V3
negV3 v = { x: -v.x, y: -v.y, z: -v.z }

-- Replace ONLY the position of atom `aid` (immutably; array order, ids and all
-- other atoms untouched).
setAtomPos :: Int -> V3 -> BuilderState -> BuilderState
setAtomPos aid pos s =
  s { atoms = map (\a -> if a.id == aid then a { pos = pos } else a) s.atoms }

-- Clamp a value into [-1, 1] so acos/sqrt latitude maths can never go NaN.
clampUnit :: Number -> Number
clampUnit x = max (-1.0) (min 1.0 x)

-- Multiply a 4x4 matrix by a homogeneous vector (w = 1): M * [x, y, z, 1]ᵀ.
mulVec :: Matrix Number -> V3 -> { x :: Number, y :: Number, z :: Number, w :: Number }
mulVec m v =
  let
    out = toVector (multiply m (fromColumn [ v.x, v.y, v.z, 1.0 ]))
  in
    { x: fromMaybe 0.0 (out !! 0)
    , y: fromMaybe 0.0 (out !! 1)
    , z: fromMaybe 0.0 (out !! 2)
    , w: fromMaybe 0.0 (out !! 3)
    }

-- Read entry (i, j) of a 4x4 row-major matrix.
entry :: Matrix Number -> Int -> Int -> Number
entry m i j = fromMaybe 0.0 (toVector m !! (i * 4 + j))
