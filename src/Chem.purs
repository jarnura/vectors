-- Pure chemistry helper for the molecule builder: a clamp-safe valence table
-- for Z = 1..36 (H..Kr). Valences are the common bonding capacities used to cap
-- how many single bonds an atom may form.
--
-- SIMPLIFICATION: the transition metals (Sc..Zn, Z = 21..30) have variable,
-- context-dependent valences in reality. The builder is a teaching toy, not a
-- chemistry engine, so every transition metal is given a flat default valence
-- of 2. This keeps the model total and deterministic without modelling
-- oxidation states.
module Chem
  ( valence
  ) where

import Prelude

import Data.Array ((!!))
import Data.Maybe (fromMaybe)

-- Highest atomic number the table supports (Krypton). Mirrors Atom.maxElectron.
maxZ :: Int
maxZ = 36

-- Clamp an atomic number into the supported range [1, 36] so bad input can't
-- crash. Mirrors Atom.clampZ.
clampZ :: Int -> Int
clampZ z
  | z < 1 = 1
  | z > maxZ = maxZ
  | otherwise = z

-- Common bonding valence for Z = 1..36 (index Z-1). Main-group elements use
-- their typical covalent valence; the transition metals (Sc..Zn) use a flat
-- default of 2 (see module note).
valenceTable :: Array Int
valenceTable =
  [ 1 -- H
  , 0 -- He
  , 1 -- Li
  , 2 -- Be
  , 3 -- B
  , 4 -- C
  , 3 -- N
  , 2 -- O
  , 1 -- F
  , 0 -- Ne
  , 1 -- Na
  , 2 -- Mg
  , 3 -- Al
  , 4 -- Si
  , 3 -- P
  , 2 -- S
  , 1 -- Cl
  , 0 -- Ar
  , 1 -- K
  , 2 -- Ca
  , 2 -- Sc  (transition-metal default)
  , 2 -- Ti  (transition-metal default)
  , 2 -- V   (transition-metal default)
  , 2 -- Cr  (transition-metal default)
  , 2 -- Mn  (transition-metal default)
  , 2 -- Fe  (transition-metal default)
  , 2 -- Co  (transition-metal default)
  , 2 -- Ni  (transition-metal default)
  , 2 -- Cu  (transition-metal default)
  , 2 -- Zn  (transition-metal default)
  , 3 -- Ga
  , 4 -- Ge
  , 3 -- As
  , 2 -- Se
  , 1 -- Br
  , 0 -- Kr
  ]

-- Clamp-safe bonding valence for an atomic number. Out-of-range Z clamps into
-- [1, 36] (H..Kr), so the result is always a defined Int. Pure and total.
valence :: Int -> Int
valence raw = fromMaybe 0 (valenceTable !! (clampZ raw - 1))
