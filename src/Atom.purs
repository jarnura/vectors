-- Pure atomic model for atomos: a small element table (Z = 1..8), electron-shell
-- filling, and a deterministic nucleon cluster layout. No rendering or effects.
module Atom
  ( V3
  , Element
  , Nucleon(..)
  , Particle
  , elementOf
  , elementName
  , electronShells
  , nucleons
  , nucleusRadius
  , nucleonRadius
  , shellRadius
  , electronRadius
  , electronPositions
  ) where

import Prelude

import Data.Array (concat, mapWithIndex, range, uncons, (!!))
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

-- Neutron count (common isotope) and symbol for Z = 1..8 (index Z-1).
neutronTable :: Array Int
neutronTable = [ 0, 2, 4, 5, 6, 6, 7, 8 ]

symbolTable :: Array String
symbolTable = [ "H", "He", "Li", "Be", "B", "C", "N", "O" ]

nameTable :: Array String
nameTable =
  [ "Hydrogen", "Helium", "Lithium", "Beryllium", "Boron", "Carbon", "Nitrogen", "Oxygen" ]

-- Full element name for an atomic number (clamped to the supported table).
elementName :: Int -> String
elementName raw = fromMaybe "?" (nameTable !! (clampZ raw - 1))

-- Clamp an atomic number into the supported range so bad input can't crash.
clampZ :: Int -> Int
clampZ z
  | z < 1 = 1
  | z > 8 = 8
  | otherwise = z

elementOf :: Int -> Element
elementOf raw =
  let
    z = clampZ raw
    n = fromMaybe 0 (neutronTable !! (z - 1))
    sym = fromMaybe "?" (symbolTable !! (z - 1))
  in
    { z, symbol: sym, protons: z, neutrons: n, electrons: z }

-- Greedily fill electron shells with capacities 2, 8, 8, 18, … until all
-- electrons are placed.
electronShells :: Int -> Array Int
electronShells = go shellCaps
  where
  shellCaps = [ 2, 8, 8, 18, 18 ]
  go caps remaining
    | remaining <= 0 = []
    | otherwise = case uncons caps of
        Nothing -> [ remaining ]
        Just { head: cap, tail } ->
          let
            here = min remaining cap
          in
            [ here ] <> go tail (remaining - here)

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
shellRadius :: Int -> Number
shellRadius s = 140.0 + toNumber s * 80.0

-- Electron world positions for an element at animation time `frame`. Each shell
-- holds its quota of electrons, spread evenly around a circle; successive shells
-- are tilted so their orbits don't all lie in one plane, and inner shells orbit
-- faster. Every electron stays exactly on its shell radius.
electronPositions :: Element -> Number -> Array V3
electronPositions el frame =
  concat (mapWithIndex shellElectrons (electronShells el.electrons))
  where
  shellElectrons s count =
    let
      r = shellRadius s
      incl = toNumber s * (pi / 5.0)
      speed = 0.02 / (toNumber s + 1.0)
    in
      map
        ( \k ->
            let
              theta = 2.0 * pi * toNumber k / toNumber count + frame * speed
            in
              { x: r * cos theta
              , y: -r * sin theta * sin incl
              , z: r * sin theta * cos incl
              }
        )
        (range 0 (count - 1))
