-- | Pure Morse-potential model for element-pair interactions in the Builder.
-- |
-- | The Morse potential E(r) = De*(1 - exp(-a*(r-R0)))^2 - De gives a smooth
-- | diatomic interaction energy: a deep well of depth De at equilibrium distance
-- | R0, a repulsive wall for r < R0 (Pauli-exclusion shoulder), and dissociation
-- | to 0 as r → ∞. `stretchEnergy r = morseE + De` shifts the floor to 0 so the
-- | well bottom is 0 and the asymptote is +De (tension energy).
-- |
-- | Key physics choices (all documented below):
-- |   • R0 is PINNED to `pullRestLen` = max(minSep, bondThreshold - pullSlack) so
-- |     the potential minimum aligns with where the Gauss-Seidel bond-tug
-- |     places atoms: ~160 world units for most pairs.
-- |   • De = Chem.bondEnergy (the tabulated/geometric-mean bond energy in kJ/mol ÷ 100).
-- |   • Width `a` is chosen so the repulsive branch reaches wallEnergyFactor·De at
-- |     the Pauli contact floor (minSeparation), making the "wall" energetically
-- |     costly relative to the well depth.
-- |   • All functions are pure, total, deterministic; no Effect / WebGL imports.
-- |
-- | Stability note (explicit-Euler): a force-relaxation step dt is non-oscillatory
-- | when dt < 1 / morseK = 1 / (2 * a^2 * De). In world units (1 px ~ 1 pm),
-- | for a H-H pair a ≈ 0.04 and De ≈ 4.36, so morseK ≈ 0.014 and the stability
-- | bound is dt < ~71 pixels. A typical relaxation step of 0.5 px is well within
-- | this bound.
module Pe
  ( morse
  , forceMorse
  , morseE
  , morseForce
  , morseK
  , stretchEnergy
  , bondR0
  , bondDepth
  , morseWidth
  , wallDistanceFor
  , contactFactorPe
  , absoluteMinPe
  , floorCeilPe
  , pauliWallEnergy
  , wallEnergyFactor
  , peCurveSamples
  , bondCurveMarkers
  ) where

import Prelude

import Atom (atomicRadius)
import Chem (bondEnergy)
import Data.Array (range)
import Data.Int (toNumber) as Int
import Data.Number (exp, log, sqrt)

-- ─── Mirror of Builder.Overlap constants (kept local to avoid import cycle) ──
-- These must match Builder.Overlap exactly; a comment is better than a phantom
-- dependency that would force Builder.Overlap to import Pe or vice-versa.

-- | Scale from summed normalised covalent radii to world-unit contact floor.
-- | Mirrors Builder.Overlap.contactFactor = 55.0.
contactFactorPe :: Number
contactFactorPe = 55.0

-- | Hard non-collapse floor (world units). Mirrors Builder.Overlap.absoluteMin = 130.0.
absoluteMinPe :: Number
absoluteMinPe = 130.0

-- | Ceiling on contact floor (= bondThreshold − floorMargin = 165.0).
-- | Mirrors Builder.Overlap.floorCeil = 165.0.
floorCeilPe :: Number
floorCeilPe = 165.0

-- | Bond-formation distance (world units). Mirrors Builder.Geom.bondThreshold = 180.0.
bondThresholdPe :: Number
bondThresholdPe = 180.0

-- | Slack so pulled bonds settle inside the form threshold.
-- | Mirrors Builder.Bonds.pullSlack = 20.0.
pullSlackPe :: Number
pullSlackPe = 20.0

-- ─── Pauli wall parameters ────────────────────────────────────────────────────

-- | The repulsive wall energy is chosen as wallEnergyFactor times the well depth
-- | De. At the Pauli contact floor the Morse repulsive branch must cost at least
-- | this much: E(minSep) ≈ wallEnergyFactor * De.
-- |
-- | Choice: 10.0 — the wall is 10× the bond-dissociation energy, representing a
-- | stiff Pauli shoulder without an unrealistically steep gradient.
wallEnergyFactor :: Number
wallEnergyFactor = 10.0

-- | Morse energy at the Pauli contact floor (= wallEnergyFactor * De, in
-- | kJ/mol ÷ 100 units). Used to define the morseWidth `a` analytically.
-- |
-- | NOTE: this is NOT a per-pair constant; it is a FACTOR. The per-pair wall
-- | energy target is wallEnergyFactor * bondDepth z1 z2.
pauliWallEnergy :: Number
pauliWallEnergy = wallEnergyFactor

-- ─── Pure Morse primitive functions ──────────────────────────────────────────

-- | Morse potential energy.
-- |   morse De a R0 r = De * (1 - exp(-a*(r-R0)))^2 - De
-- |
-- | Properties:
-- |   morse De a R0 R0  = -De         (well minimum)
-- |   morse De a R0 ∞   =  0          (dissociation limit)
-- |   morse De a R0 r   > 0 for r well below R0 (repulsive wall)
morse :: Number -> Number -> Number -> Number -> Number
morse de a r0 r =
  let
    xi = 1.0 - exp ((-a) * (r - r0))
  in
    de * xi * xi - de

-- | Force from the Morse potential (= -dE/dr).
-- |   forceMorse De a R0 r = -2·a·De·exp(-a·(r-R0))·(1 - exp(-a·(r-R0)))
-- |
-- | Sign convention: positive force pushes atoms apart (repulsive), negative
-- | pulls them together (attractive).
-- |   r < R0 → repulsive (positive)
-- |   r > R0 → attractive (negative)
-- |   r = R0 → zero (equilibrium)
forceMorse :: Number -> Number -> Number -> Number -> Number
forceMorse de a r0 r =
  let
    xi = 1.0 - exp ((-a) * (r - r0))
    expTerm = exp ((-a) * (r - r0))
  in
    (-2.0) * a * de * expTerm * xi

-- ─── Per-pair inline Pauli floor (mirrors Builder.Overlap.minSeparation) ─────

-- | Clamp-safe Pauli contact floor for a pair, in world units. Inlined from
-- | Builder.Overlap.minSeparation to avoid an import cycle.
minSepPe :: Int -> Int -> Number
minSepPe z1 z2 =
  let
    raw = contactFactorPe * (atomicRadius z1 + atomicRadius z2)
  in
    max absoluteMinPe (min floorCeilPe raw)

-- ─── Per-pair parameters ──────────────────────────────────────────────────────

-- | Equilibrium bond distance for the Morse potential, in world units.
-- | Pinned to pullRestLen = max(minSep, bondThreshold − pullSlack) so the
-- | potential minimum aligns exactly with where the Gauss-Seidel bond-tug
-- | places atoms (~160 world units for most pairs including H-H and C-C).
bondR0 :: Int -> Int -> Number
bondR0 z1 z2 = max (minSepPe z1 z2) (bondThresholdPe - pullSlackPe)

-- | Well depth De = Chem.bondEnergy z1 z2 (kJ/mol ÷ 100, symmetric).
-- | This is the pair bond-dissociation energy tabulated or estimated by Chem.
bondDepth :: Int -> Int -> Number
bondDepth z1 z2 = bondEnergy z1 z2

-- | Width parameter `a` of the Morse potential, chosen analytically so that the
-- | repulsive branch reaches wallEnergyFactor·De at the Pauli contact floor:
-- |
-- |   E(minSep) = De · (1 - exp(-a·(minSep - R0)))^2 - De = wallEnergyFactor · De
-- |
-- | Solving for `a`:
-- |   (1 - exp(-a·Δr))^2 = wallEnergyFactor + 1    (where Δr = minSep - R0 < 0)
-- |   1 - exp(-a·Δr)     = -sqrt(wallEnergyFactor + 1)  (repulsive branch: Δr<0)
-- |   exp(-a·Δr)         = 1 + sqrt(wallEnergyFactor + 1)
-- |   a                  = ln(1 + sqrt(wallEnergyFactor + 1)) / (R0 - minSep)
-- |
-- | Guard: if R0 == minSep (which cannot happen for valid pairs since R0 >= minSep
-- | by construction, and equals minSep only if minSep >= 160, i.e. floorCeil=165
-- | is the active bound for a very large pair), fall back to a safe constant
-- | derived from the H-H pair geometry (a = 0.04) so the function stays total.
-- |
-- | NOTE: wallEnergyFactor = 10 → sqrt(11) ≈ 3.317, ln(4.317) ≈ 1.463. For a
-- | H-H pair where R0=160 and minSep=130, (R0-minSep)=30, so a ≈ 1.463/30 ≈ 0.049.
morseWidth :: Int -> Int -> Number
morseWidth z1 z2 =
  let
    r0 = bondR0 z1 z2
    ms = minSepPe z1 z2
    gap = r0 - ms
    -- Fallback: use 0.049 (derived from the typical H-H geometry above) if the
    -- gap is negligibly small (degenerate case where floorCeil is the active bound).
    fallbackA = 0.049
  in
    if gap < 1.0e-9 then fallbackA
    else log (1.0 + sqrt (wallEnergyFactor + 1.0)) / gap

-- ─── Convenience wrappers ─────────────────────────────────────────────────────

-- | Morse potential energy for a pair of atomic numbers at distance r.
morseE :: Int -> Int -> Number -> Number
morseE z1 z2 r =
  morse (bondDepth z1 z2) (morseWidth z1 z2) (bondR0 z1 z2) r

-- | Morse force for a pair of atomic numbers at distance r.
morseForce :: Int -> Int -> Number -> Number
morseForce z1 z2 r =
  forceMorse (bondDepth z1 z2) (morseWidth z1 z2) (bondR0 z1 z2) r

-- | Harmonic spring constant at equilibrium: k = 2·a²·De.
-- | This is the curvature of the Morse well at r=R0: d²E/dr²|R0 = 2·a²·De.
-- | The explicit-Euler stability bound for a force-relaxation step dt is:
-- |   dt < 1 / morseK = 1 / (2 * a^2 * De)
morseK :: Int -> Int -> Number
morseK z1 z2 =
  let
    a = morseWidth z1 z2
    de = bondDepth z1 z2
  in
    2.0 * a * a * de

-- | Stretch energy (zero at equilibrium, positive both sides):
-- |   stretchEnergy = morseE + De
-- | This shifts the Morse curve up by De so the well bottom is 0 and the
-- | dissociation asymptote is +De.
stretchEnergy :: Int -> Int -> Number -> Number
stretchEnergy z1 z2 r = morseE z1 z2 r + bondDepth z1 z2

-- | The r (< R0) where stretchEnergy equals pauliWallEnergy * De (the Pauli wall
-- | crossing). Closed-form inversion of the Morse:
-- |   r = R0 - ln(1 + sqrt(pauliWallEnergy * De / De)) / a
-- |     = R0 - ln(1 + sqrt(wallEnergyFactor)) / a
-- | By construction this should land ~ minSeparation (the Pauli contact floor).
-- | It is NOT identical to minSeparation: wallDistanceFor = R0 - (R0-minSep) *
-- | ln(1+sqrt(wEF)) / ln(1+sqrt(wEF+1)) where wEF=10, so the ratio is
-- | ln(1+sqrt(10)) / ln(1+sqrt(11)) ≈ 1.208/1.463 ≈ 0.826, landing ~4.4 world
-- | units above minSep for a 30-unit gap (H-H). This small discrepancy is the
-- | documented tolerance used in S2 equivalence assertions (see test/BuilderOverlapSpec.purs).
wallDistanceFor :: Int -> Int -> Number
wallDistanceFor z1 z2 =
  let
    r0 = bondR0 z1 z2
    a = morseWidth z1 z2
  in
    r0 - log (1.0 + sqrt wallEnergyFactor) / a

-- ─── Pure PE-curve data seam ──────────────────────────────────────────────────

-- | Sample E(r) = morseE z1 z2 r over `n` evenly-spaced r values in the range
-- | [rMin, rMax], producing an array of {r, e} records for curve rendering or
-- | overlay display. The Pauli-exclusion region is r < bondR0 z1 z2 (rising
-- | wall due to filled-shell interpenetration); the well minimum is at r = R0.
-- |
-- | Typical call: peCurveSamples 100 z1 z2 samples 100 points from ~minSep
-- | to ~3·R0, capturing the repulsive wall, well, and dissociation tail.
-- |
-- | Pure, total, deterministic; no Effect / WebGL.
peCurveSamples :: Int -> Int -> Int -> Array { r :: Number, e :: Number }
peCurveSamples n z1 z2 =
  let
    r0 = bondR0 z1 z2
    ms = minSepPe z1 z2
    -- Range: from just outside the hard Pauli floor to 3·R0 (dissociation tail).
    rMin = max 1.0 (ms - 10.0)
    rMax = r0 * 3.0
    -- Step: (rMax - rMin) / max(1, n-1) so n points span [rMin, rMax] exactly.
    step = if n <= 1 then 0.0 else (rMax - rMin) / Int.toNumber (n - 1)
    indices = range 0 (n - 1)
  in
    map (\i -> let r = rMin + step * Int.toNumber i in { r, e: morseE z1 z2 r }) indices

-- | For the current builder state (as a list of bond segments {a,b,z1,z2}),
-- | return the current-r marker for each live bond: the actual inter-nuclear
-- | distance plus the Morse energy at that distance, so the caller can render a
-- | dot on the PE curve at the bond's live position.
-- |
-- | The caller supplies bond data as an array of
-- |   { z1 :: Int, z2 :: Int, r :: Number }
-- | (distance already computed by the caller from the atom positions), keeping
-- | this helper free of BuilderState to avoid an import cycle.
-- |
-- | Pure, total, deterministic; no Effect / WebGL.
bondCurveMarkers
  :: Array { z1 :: Int, z2 :: Int, r :: Number }
  -> Array { z1 :: Int, z2 :: Int, r :: Number, e :: Number }
bondCurveMarkers bonds =
  map (\b -> { z1: b.z1, z2: b.z2, r: b.r, e: morseE b.z1 b.z2 b.r }) bonds

