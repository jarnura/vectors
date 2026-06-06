-- Pure quantum-orbital model for atomos: enumerate the real orbitals (s, p, d)
-- of an element, fill them by Aufbau + Hund + Pauli, and give each a physical
-- radius from Slater's-rules effective nuclear charge. No rendering, no effects.
--
-- Only s/p/d are needed: the supported range is Z = 1..36 (Krypton), whose
-- highest azimuthal quantum number is ℓ = 2 (d).
module Orbital
  ( OrbShape(..)
  , Orbital
  , orbitalsFor
  , zEff
  , meanRadius
  , rScale
  , orbitalViewportRadius
  , occBrightness
  ) where

import Prelude

import Atom (elementOf, fillSubshells)
import Data.Array (concatMap, length, range, zipWith)
import Data.Foldable (maximum, sum)
import Data.Int (toNumber)
import Data.Maybe (fromMaybe)

-- The real (cubic-harmonic) orbital shapes reached for Z ≤ 36.
data OrbShape
  = S
  | Px
  | Py
  | Pz
  | Dz2
  | Dxz
  | Dyz
  | Dx2y2
  | Dxy

derive instance eqOrbShape :: Eq OrbShape
derive instance ordOrbShape :: Ord OrbShape

-- A single real orbital: principal n, azimuthal l, its shape, and how many
-- electrons (0, 1, or 2) occupy it.
type Orbital = { n :: Int, l :: Int, kind :: OrbShape, occ :: Int }

-- The real orbital shapes of sub-shell l, in a fixed canonical order.
shapesFor :: Int -> Array OrbShape
shapesFor 0 = [ S ]
shapesFor 1 = [ Px, Py, Pz ]
shapesFor 2 = [ Dz2, Dxz, Dyz, Dx2y2, Dxy ]
shapesFor _ = []

-- Distribute `count` electrons across `numOrb` degenerate orbitals by Hund's
-- rule: one electron in each orbital before any pairing (Pauli ≤ 2).
hundOccupancies :: Int -> Int -> Array Int
hundOccupancies count numOrb = map occAt (range 0 (numOrb - 1))
  where
  singles = min count numOrb
  paired = count - numOrb -- > 0 once every orbital is singly occupied
  occAt i =
    (if i < singles then 1 else 0)
      + (if paired > 0 && i < paired then 1 else 0)

-- Every real orbital of element Z with its Hund/Pauli occupancy. Built from the
-- Madelung sub-shell fill (Atom.fillSubshells), so it inherits the correct
-- Aufbau order and clamping.
orbitalsFor :: Int -> Array Orbital
orbitalsFor z =
  concatMap subOrbitals (fillSubshells (elementOf z).electrons)
  where
  subOrbitals ss =
    let
      shapes = shapesFor ss.l
      occs = hundOccupancies ss.count (length shapes)
    in
      zipWith (\kind occ -> { n: ss.n, l: ss.l, kind, occ }) shapes occs

-- ───── Slater's rules: effective nuclear charge & physical radii ─────

-- Slater shielding groups: [ns,np] share a group; nd and nf are separate. The
-- group is keyed by (n, isHigherL) where isHigherL distinguishes d/f from s/p.
groupKey :: Int -> Int -> { n :: Int, hi :: Boolean }
groupKey n l = { n, hi: l >= 2 }

-- Effective nuclear charge seen by an electron in sub-shell (tn, tl) of element
-- Z, via Slater's rules. Z_eff = Z − σ.
zEff :: Int -> Int -> Int -> Number
zEff z tn tl =
  let
    el = elementOf z
    subs = fillSubshells el.electrons
    target = groupKey tn tl
    sameGroupConst = if tn == 1 && tl == 0 then 0.30 else 0.35
    contribution ss =
      let
        c = toNumber ss.count
        g = groupKey ss.n ss.l
        sameGroup = g.n == target.n && g.hi == target.hi
      in
        if sameGroup then c * sameGroupConst
        else if tl <= 1 then
          -- target electron is in an [ns,np] group
          if ss.n == tn - 1 then c * 0.85
          else if ss.n <= tn - 2 then c * 1.0
          else 0.0
        else
          -- target electron is in an nd/nf group: every inner electron screens
          -- fully (all lower shells, plus the ns,np of the same shell).
          if ss.n < tn then c * 1.0
        else if ss.n == tn && ss.l < tl then c * 1.0
        else 0.0
    -- Sum all contributions, then remove the target electron's own same-group
    -- self-contribution.
    sigma = sum (map contribution subs) - sameGroupConst
  in
    max 0.001 (toNumber z - sigma)

-- Unnormalized Bohr-like mean radius of sub-shell (n, l) in element Z: ∝ n²/Z_eff.
meanRadius :: Int -> Int -> Int -> Number
meanRadius z n l = toNumber (n * n) / zEff z n l

-- World-space radius the outermost occupied orbital maps to (keeps Krypton's 4p
-- inside the camera frustum while leaving inner orbitals visible).
orbitalViewportRadius :: Number
orbitalViewportRadius = 380.0

-- Physical render radius of sub-shell (n, l) in element Z: the mean radius,
-- normalized so the element's outermost occupied orbital sits at the viewport
-- radius. Grows ~n² within an atom and contracts as Z increases.
rScale :: Int -> Int -> Int -> Number
rScale z n l =
  let
    el = elementOf z
    subs = fillSubshells el.electrons
    radii = map (\ss -> meanRadius z ss.n ss.l) subs
    maxR = fromMaybe 1.0 (maximum radii)
  in
    orbitalViewportRadius * meanRadius z n l / maxR

-- Render brightness for an orbital's occupancy: empty (0) is dark/unrendered, a
-- singly-occupied orbital (1, Hund) is dimmer, a paired orbital (2) is full —
-- so Hund's rule reads visually.
occBrightness :: Int -> Number
occBrightness occ
  | occ <= 0 = 0.0
  | occ == 1 = 0.5
  | otherwise = 1.0
