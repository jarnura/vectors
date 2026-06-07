-- Pure molecule model for the (future) molecule scene. A small registry of
-- molecules built on the atomic model in `Atom`: each molecule is a set of
-- atoms (positioned in scene units), the covalent bonds between them, and a
-- data-driven property list for the info overlay. No rendering or effects.
--
-- The first entry is the diatomic hydrogen slice (H₂): two hydrogen nuclei
-- separated along the x axis, sharing a pair of electrons in the internuclear
-- overlap region. Shared electrons and nucleon placement reuse `Atom`.
module Molecule
  ( Bond
  , MolAtom
  , Property
  , Molecule
  , bondLength
  , molecules
  , moleculeOf
  , sharedElectronPositions
  , moleculeNucleons
  ) where

import Prelude

import Atom (Particle, V3, elementOf, nucleons)
import Data.Array (concatMap, length, mapWithIndex, range, (!!))
import Data.Int (toNumber)
import Data.Maybe (fromMaybe)
import Data.Number (cos, pi, sin)

-- A covalent bond between atoms `a` and `b` (indices into the molecule's atom
-- list), with a bond `order` (1 = single, 2 = double, …) and the number of
-- `shared` electrons sitting in the bond.
type Bond = { a :: Int, b :: Int, order :: Int, shared :: Int }

-- One atom in a molecule: which element (atomic number) and where its nucleus
-- centre sits in scene units.
type MolAtom = { element :: Int, center :: V3 }

-- A labelled row for the molecule info overlay.
type Property = { label :: String, value :: String }

-- A molecule: a name, a chemical formula, its atoms, its bonds, and a
-- data-driven property list for the overlay.
type Molecule =
  { name :: String
  , formula :: String
  , atoms :: Array MolAtom
  , bonds :: Array Bond
  , properties :: Array Property
  }

-- Internuclear bond length in scene units. Chosen so the two nuclei (nucleus
-- radius ~60) sit clearly apart with room for the shared pair between them,
-- while staying on-screen at the atomos scale (shells start ~140).
bondLength :: Number
bondLength = 160.0

-- The molecule registry. Entry 0 is the diatomic hydrogen slice (H₂).
molecules :: Array Molecule
molecules = [ hydrogen ]

-- H₂: two hydrogen atoms on the x axis at ±bondLength/2, one covalent single
-- bond sharing two electrons.
hydrogen :: Molecule
hydrogen =
  { name: "Hydrogen"
  , formula: "H₂"
  , atoms:
      [ { element: 1, center: { x: -bondLength / 2.0, y: 0.0, z: 0.0 } }
      , { element: 1, center: { x: bondLength / 2.0, y: 0.0, z: 0.0 } }
      ]
  , bonds: [ { a: 0, b: 1, order: 1, shared: 2 } ]
  , properties:
      [ { label: "Formula", value: "H₂" }
      , { label: "Bond", value: "covalent single" }
      , { label: "Bond length", value: "~74 pm" }
      , { label: "Bond energy", value: "~436 kJ/mol" }
      , { label: "Shared", value: "2 electrons" }
      ]
  }

-- Total, clamp-safe lookup into the registry: out-of-range indices clamp to the
-- nearest valid entry so callers never crash.
moleculeOf :: Int -> Molecule
moleculeOf i = fromMaybe hydrogen (molecules !! clamp i)
  where
  clamp j
    | j < 0 = 0
    | j > length molecules - 1 = length molecules - 1
    | otherwise = j

-- Shared (bonding) electron positions for a molecule at animation time `frame`.
-- For each bond, its `shared` electrons are distributed in the internuclear
-- overlap region around the bond midpoint. They are mirrored in pairs so the
-- mean position stays at the midpoint, gently animated about the bond axis so
-- the cloud breathes with the frame. Every electron's x stays within the
-- inter-nuclear span [-bondLength/2, +bondLength/2].
sharedElectronPositions :: Molecule -> Number -> Array V3
sharedElectronPositions mol frame = concatMap bondElectrons mol.bonds
  where
  centerAt i =
    (fromMaybe { element: 0, center: { x: 0.0, y: 0.0, z: 0.0 } } (mol.atoms !! i)).center

  bondElectrons bond =
    let
      ca = centerAt bond.a
      cb = centerAt bond.b
      mid = { x: (ca.x + cb.x) / 2.0, y: (ca.y + cb.y) / 2.0, z: (ca.z + cb.z) / 2.0 }
      -- Half-span of the bond along x, used to keep electrons within the nuclei.
      span = abs (cb.x - ca.x) / 2.0
      speed = 0.03
      n = bond.shared
    in
      mapWithIndex
        ( \k _ ->
            let
              -- Alternate sign so paired electrons mirror across the midpoint.
              -- The x offset uses a SHARED frame phase (no per-k offset) so the
              -- mirrored pair cancels exactly, keeping the mean at the midpoint.
              sign = if k `mod` 2 == 0 then 1.0 else -1.0
              xPhase = frame * speed
              phase = xPhase + toNumber k * (pi / toNumber (max 1 n))
              -- Small, bounded offsets that breathe with the frame. The x
              -- offset stays well within the span.
              dx = sign * 0.25 * span * cos xPhase
              dy = sign * electronCloud * sin phase
              dz = sign * electronCloud * cos phase
            in
              { x: mid.x + dx, y: mid.y + dy, z: mid.z + dz }
        )
        (range 0 (n - 1))

-- Small transverse radius of the shared-electron cloud around the bond axis.
electronCloud :: Number
electronCloud = 14.0

-- Absolute value helper (avoids pulling in Data.Number.abs alongside the rest).
abs :: Number -> Number
abs x = if x < 0.0 then -x else x

-- Nucleons for the whole molecule: each atom's nucleon cluster, translated so it
-- sits at that atom's centre. For H₂ this yields two protons, one per centre.
moleculeNucleons :: Molecule -> Array Particle
moleculeNucleons mol = concatMap atomNucleons mol.atoms
  where
  atomNucleons atom =
    map
      (\p -> p { pos = translate atom.center p.pos })
      (nucleons (elementOf atom.element))

  translate c v = { x: v.x + c.x, y: v.y + c.y, z: v.z + c.z }
