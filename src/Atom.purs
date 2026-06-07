-- Pure atomic model for atomos: an element table (Z = 1..36, H..Kr), Madelung
-- sub-shell electron filling, and a deterministic nucleon cluster layout. No
-- rendering or effects.
module Atom
  ( V3
  , Element
  , Nucleon(..)
  , Particle
  , Subshell
  , elementOf
  , elementName
  , electronShells
  , subshellCap
  , fillSubshells
  , configString
  , nucleons
  , nucleusRadius
  , nucleonRadius
  , maxElectron
  , shellRadius
  , subshellRadius
  , subshellInclination
  , electronRadius
  , electronPositions
  , electronPositionsBySubshell
  , electronPositionsBySubshell2D
  ) where

import Prelude

import Data.Array (concat, filter, mapWithIndex, range, sortBy, uncons, (!!))
import Data.Foldable (intercalate, maximum, sum)
import Data.Int (toNumber)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Number (cos, pi, pow, sin, sqrt)

type V3 = { x :: Number, y :: Number, z :: Number }

type Element =
  { z :: Int
  , symbol :: String
  , protons :: Int
  , neutrons :: Int
  , electrons :: Int
  }

data Nucleon
  = Proton
  | Neutron

derive instance eqNucleon :: Eq Nucleon

type Particle = { pos :: V3, kind :: Nucleon }

-- Radius of the whole nucleus cluster, and of each individual nucleon sphere.
nucleusRadius :: Number
nucleusRadius = 60.0

nucleonRadius :: Number
nucleonRadius = 22.0

-- Neutron count (most-abundant isotope, N = A − Z) for Z = 1..36 (index Z−1).
neutronTable :: Array Int
neutronTable =
  [ 0
  , 2
  , 4
  , 5
  , 6
  , 6
  , 7
  , 8
  , 10
  , 10 -- H..Ne
  , 12
  , 12
  , 14
  , 14
  , 16
  , 16
  , 18
  , 22
  , 20
  , 20 -- Na..Ca
  , 24
  , 26
  , 28
  , 28
  , 30
  , 30
  , 32
  , 30
  , 34
  , 34 -- Sc..Zn
  , 38
  , 42
  , 42
  , 46
  , 44
  , 48 -- Ga..Kr
  ]

symbolTable :: Array String
symbolTable =
  [ "H"
  , "He"
  , "Li"
  , "Be"
  , "B"
  , "C"
  , "N"
  , "O"
  , "F"
  , "Ne"
  , "Na"
  , "Mg"
  , "Al"
  , "Si"
  , "P"
  , "S"
  , "Cl"
  , "Ar"
  , "K"
  , "Ca"
  , "Sc"
  , "Ti"
  , "V"
  , "Cr"
  , "Mn"
  , "Fe"
  , "Co"
  , "Ni"
  , "Cu"
  , "Zn"
  , "Ga"
  , "Ge"
  , "As"
  , "Se"
  , "Br"
  , "Kr"
  ]

nameTable :: Array String
nameTable =
  [ "Hydrogen"
  , "Helium"
  , "Lithium"
  , "Beryllium"
  , "Boron"
  , "Carbon"
  , "Nitrogen"
  , "Oxygen"
  , "Fluorine"
  , "Neon"
  , "Sodium"
  , "Magnesium"
  , "Aluminium"
  , "Silicon"
  , "Phosphorus"
  , "Sulfur"
  , "Chlorine"
  , "Argon"
  , "Potassium"
  , "Calcium"
  , "Scandium"
  , "Titanium"
  , "Vanadium"
  , "Chromium"
  , "Manganese"
  , "Iron"
  , "Cobalt"
  , "Nickel"
  , "Copper"
  , "Zinc"
  , "Gallium"
  , "Germanium"
  , "Arsenic"
  , "Selenium"
  , "Bromine"
  , "Krypton"
  ]

-- Full element name for an atomic number (clamped to the supported table).
elementName :: Int -> String
elementName raw = fromMaybe "?" (nameTable !! (clampZ raw - 1))

-- Clamp an atomic number into the supported range (H..Kr) so bad input can't
-- crash. Mirrors maxElectron so the table and the electron model agree.
clampZ :: Int -> Int
clampZ z
  | z < 1 = 1
  | z > maxElectron = maxElectron
  | otherwise = z

elementOf :: Int -> Element
elementOf raw =
  let
    z = clampZ raw
    n = fromMaybe 0 (neutronTable !! (z - 1))
    sym = fromMaybe "?" (symbolTable !! (z - 1))
  in
    { z, symbol: sym, protons: z, neutrons: n, electrons: z }

-- ───── Sub-shell (orbital) electron model, Madelung/Aufbau filling ───

-- A filled subshell: principal quantum number n, azimuthal l (s=0,p=1,d=2,…),
-- a human label (e.g. "3d"), and how many electrons occupy it.
type Subshell = { n :: Int, l :: Int, label :: String, count :: Int }

-- Highest atomic number the model supports (Krypton).
maxElectron :: Int
maxElectron = 36

-- Maximum electrons a subshell of azimuthal quantum number l can hold: 4ℓ+2
-- (s=2, p=6, d=10, f=14, g=18).
subshellCap :: Int -> Int
subshellCap l = 4 * l + 2

-- Subshell letters for l = 0..4.
subshellLetter :: Int -> String
subshellLetter l = fromMaybe "?" ([ "s", "p", "d", "f", "g" ] !! l)

-- Madelung / Aufbau fill order as (n, l) pairs, sorted by (n+ℓ, then lower n).
-- The list runs past Krypton for headroom; only subshells reached by Z fill.
madelungOrder :: Array { n :: Int, l :: Int }
madelungOrder =
  [ { n: 1, l: 0 } -- 1s
  , { n: 2, l: 0 } -- 2s
  , { n: 2, l: 1 } -- 2p
  , { n: 3, l: 0 } -- 3s
  , { n: 3, l: 1 } -- 3p
  , { n: 4, l: 0 } -- 4s
  , { n: 3, l: 2 } -- 3d
  , { n: 4, l: 1 } -- 4p
  , { n: 5, l: 0 } -- 5s
  , { n: 4, l: 2 } -- 4d
  , { n: 5, l: 1 } -- 5p
  ]

-- Fill subshells in Madelung order, capping each at 4ℓ+2, until `total`
-- electrons are placed. Pure, total, deterministic; emits only filled (>0)
-- subshells. A non-positive count yields no subshells.
fillSubshells :: Int -> Array Subshell
fillSubshells total = go madelungOrder (max 0 total)
  where
  go orbitals remaining
    | remaining <= 0 = []
    | otherwise = case uncons orbitals of
        Nothing -> []
        Just { head: o, tail } ->
          let
            here = min remaining (subshellCap o.l)
          in
            [ { n: o.n, l: o.l, label: show o.n <> subshellLetter o.l, count: here } ]
              <> go tail (remaining - here)

-- Clamp an electron count into the supported [1, maxElectron] range.
clampElectron :: Int -> Int
clampElectron e
  | e < 1 = 1
  | e > maxElectron = maxElectron
  | otherwise = e

-- Per-principal-shell electron totals, DERIVED from the subshell fill by
-- grouping subshells by n and summing. Because 4s fills before 3d (Madelung)
-- but 3d's electrons belong to shell n=3, this yields the correct transition
-- metal configs (e.g. Scandium → [2,8,9,2], Krypton → [2,8,18,8]).
electronShells :: Int -> Array Int
electronShells z =
  let
    subs = fillSubshells (clampElectron z)
    maxN = fromMaybe 0 (maximum (map _.n subs))
  in
    filter (_ > 0)
      (map (\nn -> sum (map _.count (filter (\s -> s.n == nn) subs))) (range 1 maxN))

-- Electron configuration string in standard (n, l)-sorted order, e.g. Krypton →
-- "1s2 2s2 2p6 3s2 3p6 3d10 4s2 4p6".
configString :: Int -> String
configString z =
  intercalate " " (map (\s -> s.label <> show s.count) sorted)
  where
  sorted = sortBy (\a b -> compare a.n b.n <> compare a.l b.l) (fillSubshells (clampElectron z))

-- The nucleus: protons + neutrons packed into a small ball (Fibonacci-sphere
-- directions, cube-root radial spacing so they fill rather than shell). The
-- first `protons` particles are protons; the rest neutrons.
nucleons :: Element -> Array Particle
nucleons el =
  mapWithIndex
    (\i pos -> { pos, kind: if i < el.protons then Proton else Neutron })
    (clusterPositions (el.protons + el.neutrons))

clusterPositions :: Int -> Array V3
clusterPositions total = map place (range 0 (total - 1))
  where
  goldenAngle = pi * (3.0 - sqrt 5.0)
  n = toNumber total
  place i =
    let
      fi = toNumber i
      -- Fill the ball: radius grows with the cube root of the index.
      frac = if total <= 1 then 0.0 else pow ((fi + 0.5) / n) (1.0 / 3.0)
      rad = nucleusRadius * 0.85 * frac
      y = 1.0 - 2.0 * (fi + 0.5) / n
      rr = sqrt (max 0.0 (1.0 - y * y))
      theta = goldenAngle * fi
    in
      { x: rad * rr * cos theta, y: rad * y, z: rad * rr * sin theta }

-- ───── Electrons: animated circular Bohr orbits ──────────────────────

-- Radius of each electron sphere.
electronRadius :: Number
electronRadius = 11.0

-- Radius of electron shell `s` (0-based), outside the nucleus and increasing.
-- Kept as the per-principal-shell base radius that subshellRadius builds on.
shellRadius :: Int -> Number
shellRadius s = 140.0 + toNumber s * 80.0

-- Extra ring spacing between subshells of the same shell (s/p/d/f), so distinct
-- orbitals are visually separated. Kept well under the 80-unit shell spacing so
-- subshells never cross into the next shell's band.
subshellGap :: Number
subshellGap = 18.0

-- Radius of the subshell with principal number `n` (1-based) and azimuthal `l`:
-- the shell base radius plus a per-ℓ offset. Each (n, l) gets a distinct radius.
subshellRadius :: Int -> Int -> Number
subshellRadius n l = shellRadius (n - 1) + toNumber l * subshellGap

-- Tilt of the subshell's orbital plane, a pure function of (n, l). Shared by the
-- electrons AND the orbital ring line so the ring traces the electron path exactly.
subshellInclination :: Int -> Int -> Number
subshellInclination n l = toNumber n * (pi / 5.0) + toNumber l * (pi / 9.0)

-- Electron world positions for an element at animation time `frame`. Each filled
-- SUBSHELL holds its electrons on its own ring (radius from subshellRadius),
-- spread evenly; rings are tilted by (n, l) so orbitals don't all lie in one
-- plane, and inner shells orbit faster. Every electron stays exactly on its
-- subshell radius.
electronPositions :: Element -> Number -> Array V3
electronPositions el frame = concat (electronPositionsBySubshell el frame)

-- Electron world positions GROUPED by filled sub-shell (Madelung order), so each
-- group can be coloured by its sub-shell. `concat` of the groups is exactly
-- `electronPositions`.
electronPositionsBySubshell :: Element -> Number -> Array (Array V3)
electronPositionsBySubshell el frame =
  map subshellElectrons (fillSubshells el.electrons)
  where
  subshellElectrons ss =
    let
      r = subshellRadius ss.n ss.l
      incl = subshellInclination ss.n ss.l
      speed = 0.02 / (toNumber ss.n + 1.0)
    in
      map
        ( \k ->
            let
              theta = 2.0 * pi * toNumber k / toNumber ss.count + frame * speed
            in
              { x: r * cos theta
              , y: -r * sin theta * sin incl
              , z: r * sin theta * cos incl
              }
        )
        (range 0 (ss.count - 1))

-- Like `electronPositionsBySubshell`, but FLAT in the XY plane (facing the
-- camera): z is always 0 and electrons orbit on a circle in screen space.
electronPositionsBySubshell2D :: Element -> Number -> Array (Array V3)
electronPositionsBySubshell2D el frame =
  map subshellElectrons (fillSubshells el.electrons)
  where
  subshellElectrons ss =
    let
      r = subshellRadius ss.n ss.l
      speed = 0.02 / (toNumber ss.n + 1.0)
    in
      map
        ( \k ->
            let
              theta = 2.0 * pi * toNumber k / toNumber ss.count + frame * speed
            in
              { x: r * cos theta
              , y: r * sin theta
              , z: 0.0
              }
        )
        (range 0 (ss.count - 1))
