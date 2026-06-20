-- Deterministic 3D spawn positions for the Builder: a growing golden-angle /
-- Fibonacci shell layout so successive atoms differ in x, y AND z. Pure, total,
-- NaN-free — no Effect/WebGL.
module Builder.Spawn
  ( spawnPos
  ) where

import Prelude

import Atom (V3)
import Builder.Geom (bondThreshold, clampUnit)
import Data.Int (toNumber)
import Data.Number (cos, pi, sin, sqrt)

-- A deterministic 3D spawn position for the atom at insertion index `i`, laid
-- out on a growing golden-angle / Fibonacci shell so successive atoms differ in
-- x, y AND z (never the old collinear y = z = 0 line). The azimuth advances by
-- the golden angle, the latitude sweeps a band so points stay well-spread, and
-- the radius grows slowly (∝ sqrt i) — tied to bondThreshold so near atoms can
-- still auto-bond while the Pauli contact floor survives resolveOverlaps. Pure,
-- total, NaN-free (sqrt domains guarded with max).
spawnPos :: Int -> V3
spawnPos i =
  let
    fi = toNumber i
    azimuth = fi * goldenAngle
    -- Cycle the latitude band every `spawnLatK` atoms (radius keeps growing with
    -- the true index) so the shell never collapses to a pole/axis at high counts.
    -- `i mod spawnLatK` is the identity for i < spawnLatK, so the first
    -- `spawnLatK` atoms are byte-unchanged.
    latIndex = toNumber (i `mod` spawnLatK)
    yUnit = clampUnit (1.0 - 2.0 * ((latIndex + 0.5) / toNumber spawnLatK))
    r = sqrt (max 0.0 (1.0 - yUnit * yUnit))
    radius = bondThreshold * 0.5 + bondThreshold * 0.28 * sqrt (max 0.0 fi)
  in
    { x: radius * r * cos azimuth
    , y: radius * yUnit
    , z: radius * r * sin azimuth
    }

-- The golden angle, pi * (3 - sqrt 5) ≈ 2.399963 rad — the irrational turn that
-- keeps successive Fibonacci-shell azimuths maximally spread.
goldenAngle :: Number
goldenAngle = pi * (3.0 - sqrt 5.0)

-- Latitude-band size for spawnPos: the band cycles every `spawnLatK` atoms
-- (radius still grows with the true index), so the shell spreads in 3D at any
-- count instead of collapsing to a pole/axis past the band edge.
spawnLatK :: Int
spawnLatK = 12
