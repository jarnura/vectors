-- Pure chemistry helpers for the molecule builder: a clamp-safe valence table
-- and a clamp-safe single-bond energy table for Z = 1..36 (H..Kr). Valences are
-- the common bonding capacities used to cap how many single bonds an atom may
-- form; bond energies are how hard a bond resists being torn apart by a drag.
--
-- SIMPLIFICATION (valence): the transition metals (Sc..Zn, Z = 21..30) have
-- variable, context-dependent valences in reality. The builder is a teaching
-- toy, not a chemistry engine, so every transition metal is given a flat
-- default valence of 2. This keeps the model total and deterministic without
-- modelling oxidation states.
--
-- PHYSICS (bond energy): a covalent single bond has a dissociation energy —
-- the work needed to pull the two atoms apart — measured in kJ/mol (H-H is
-- ~436 kJ/mol, O-O a feeble ~146 kJ/mol). Strong bonds (O-H, H-F) resist a
-- tug; weak ones (O-O, N-N, F-F) snap first. `bondEnergy` tabulates real
-- average single-bond energies normalised by ÷100 (so H-H = 4.36) for the
-- common teaching pairs, plus homonuclear anchors for the other main-group
-- elements.
--
-- SIMPLIFICATION (bond energy): untabulated cross pairs whose BOTH homonuclear
-- energies are tabulated use the geometric mean sqrt(E(a,a)·E(b,b)) — a
-- Pauling-style estimate ignoring the electronegativity (ionic) correction.
-- A pair where NEITHER element has a tabulated homonuclear anchor behaves as
-- if both were `homoDefault` (1.5), so two such elements give 1.5; a mixed
-- pair (exactly one anchor tabulated) falls back to a flat, documented
-- `fallbackEnergy` (2.5). Like the transition-metal valence default, this
-- keeps the model total and deterministic without a full thermochemistry
-- table.
module Chem
  ( valence
  , bondEnergy
  , lengthFactor
  , depthFactor
  ) where

import Prelude

import Data.Array (find, (!!))
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Number (sqrt)

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

-- ───── Single-bond energies (normalised kJ/mol ÷ 100) ────────────────

-- Tabulated average single-bond energies, keyed by the UNORDERED pair
-- (min Z, max Z) so the table is symmetric by construction. Values are real
-- average bond-dissociation energies divided by 100 (H-H 436 kJ/mol → 4.36).
-- Cross pairs cover the common teaching bonds; the homonuclear anchors
-- (Si-Si, P-P, S-S, Se-Se, B-B) seed the geometric-mean estimate for the
-- rest of the main group.
energyTable :: Array { a :: Int, b :: Int, e :: Number }
energyTable =
  [ { a: 1, b: 1, e: 4.36 } -- H-H
  , { a: 6, b: 6, e: 3.46 } -- C-C
  , { a: 1, b: 6, e: 4.13 } -- C-H
  , { a: 1, b: 8, e: 4.63 } -- O-H
  , { a: 1, b: 7, e: 3.91 } -- N-H
  , { a: 6, b: 8, e: 3.58 } -- C-O
  , { a: 6, b: 7, e: 3.05 } -- C-N
  , { a: 8, b: 8, e: 1.46 } -- O-O
  , { a: 7, b: 7, e: 1.63 } -- N-N
  , { a: 9, b: 9, e: 1.55 } -- F-F
  , { a: 1, b: 9, e: 5.65 } -- H-F
  , { a: 17, b: 17, e: 2.42 } -- Cl-Cl
  , { a: 1, b: 17, e: 4.31 } -- H-Cl
  , { a: 35, b: 35, e: 1.90 } -- Br-Br
  , { a: 14, b: 14, e: 2.22 } -- Si-Si (homonuclear anchor)
  , { a: 15, b: 15, e: 2.01 } -- P-P   (homonuclear anchor)
  , { a: 16, b: 16, e: 2.66 } -- S-S   (homonuclear anchor)
  , { a: 34, b: 34, e: 1.72 } -- Se-Se (homonuclear anchor)
  , { a: 5, b: 5, e: 2.93 } -- B-B   (homonuclear anchor)
  ]

-- Energy an element pair behaves as when NEITHER homonuclear anchor is
-- tabulated: both elements act as homoDefault, and sqrt(1.5·1.5) = 1.5.
homoDefault :: Number
homoDefault = 1.5

-- Flat documented fallback for a mixed pair (exactly one homonuclear anchor
-- tabulated): no principled estimate exists, so use a middling energy.
fallbackEnergy :: Number
fallbackEnergy = 2.5

-- Tabulated energy for the unordered pair (lo, hi), if present.
lookupPair :: Int -> Int -> Maybe Number
lookupPair lo hi = map _.e (find (\r -> r.a == lo && r.b == hi) energyTable)

-- ───── Per-order bond-length and depth scaling factors ───────────────────────
--
-- These pure factors generalise C-C experimental bond-length and dissociation-
-- energy ratios to any element pair, providing order-aware geometry/energy for
-- the Morse potential without a full multi-centre quantum chemistry table.
--
-- Source ratios from C-C bonds (teaching values, see CODING_STANDARDS.md):
--   R0:  single 154 pm / double 134 pm / triple 120 pm
--        → factor_2 = 134/154 ≈ 0.870 ; factor_3 = 120/154 ≈ 0.779
--   De:  single 346 kJ/mol / double 614 kJ/mol / triple 839 kJ/mol
--        → factor_2 = 614/346 ≈ 1.775 ; factor_3 = 839/346 ≈ 2.425
--        (both < the naive order multiple, confirming diminishing π contribution)
--
-- SIMPLIFICATION: these C-C-derived factors are applied universally. For pairs
-- without explicit multi-bond data this is a teaching approximation; the σ + π
-- energy gain relative to the σ baseline will differ by element, but the
-- qualitative trends (shorter, stronger) are universal covalent chemistry.
--
-- Continuity: order=1 returns exactly 1.0 for both factors so every existing
-- Pe call — which implicitly assumes order 1 — is unchanged by S3.

-- | Ratio by which the equilibrium bond distance R0 shrinks for a given bond
-- | order relative to a single bond. lengthFactor 1 = 1.0 (continuity).
-- | 2 → ~0.870 (double bond ~13% shorter); 3 → ~0.779 (triple ~22% shorter).
-- | Out-of-range order clamps to order=1 (returns 1.0), keeping the function total.
lengthFactor :: Int -> Number
lengthFactor 1 = 1.0
lengthFactor 2 = 0.870
lengthFactor 3 = 0.779
lengthFactor _ = 1.0

-- | Ratio by which the Morse well depth De deepens for a given bond order
-- | relative to a single bond. depthFactor 1 = 1.0 (continuity).
-- | 2 → ~1.775 (double bond less than 2× stronger — diminishing π);
-- | 3 → ~2.425 (triple bond less than 3× — same reason).
-- | Out-of-range order clamps to order=1 (returns 1.0), keeping the function total.
depthFactor :: Int -> Number
depthFactor 1 = 1.0
depthFactor 2 = 1.775
depthFactor 3 = 2.425
depthFactor _ = 1.0

-- Clamp-safe normalised single-bond energy (kJ/mol ÷ 100) between two atomic
-- numbers. Both args clamp into [1, 36]; the lookup is on the unordered pair,
-- so the function is symmetric for ALL inputs. Fallback chain:
--   1. exact unordered pair tabulated            → that value;
--   2. BOTH homonuclear anchors tabulated        → sqrt(E(a,a)·E(b,b));
--   3. NEITHER homonuclear anchor tabulated      → homoDefault (1.5);
--   4. exactly one anchor tabulated (mixed pair) → fallbackEnergy (2.5).
-- Pure, total, deterministic, never NaN.
bondEnergy :: Int -> Int -> Number
bondEnergy rawA rawB =
  let
    za = clampZ rawA
    zb = clampZ rawB
  in
    case lookupPair (min za zb) (max za zb) of
      Just e -> e
      Nothing ->
        case lookupPair za za, lookupPair zb zb of
          Just ea, Just eb -> sqrt (ea * eb)
          Nothing, Nothing -> homoDefault
          _, _ -> fallbackEnergy
