-- | Pure bond-vibration model for the Builder render (M1.5).
-- |
-- | Bonded nuclei oscillate in the Morse potential well: each bond has a natural
-- | angular frequency derived from the Morse spring constant (Pe.morseK) and the
-- | reduced mass of the two nuclei (massProxy gives the nucleon count as a mass
-- | proxy). A non-zero `zeroPointAmp` ensures every bond jiggles visibly even at
-- | rest ("zero-point jiggle"), and `thermalAmp` adds classical thermal amplitude.
-- |
-- | Key design choices (all documented inline):
-- |   • massProxy = protons + neutrons (nucleon count per Atom.elementOf). No atomic-
-- |     mass table is needed; for the render-only visual, nucleon count is exact
-- |     enough and avoids a new data table.
-- |   • reducedMass = m1*m2/(m1+m2): the standard harmonic oscillator reduced mass
-- |     for a diatomic. Symmetric in its arguments.
-- |   • bondOmega = vibFreqScale * sqrt(k / mu): natural angular frequency from
-- |     Hooke's law ω² = k/μ. vibFreqScale converts from "world-units force per
-- |     world-unit displacement" (the Morse k units) into an observable
-- |     frames-per-cycle. Tuned so H-H buzzes visibly (~0.3 rad/frame) without
-- |     stroboscopic aliasing (ω < π → at least 2 frames per half-cycle).
-- |   • bondPhase = sin(id_a * 31 + id_b * 17): a deterministic, symmetric phase
-- |     offset per bond, derived only from the atom ids so distinct bonds buzz
-- |     out of sync (no two bonds reach their extrema at the same frame, avoiding
-- |     a visually monotonous lock-step). The hash is symmetric: the same value
-- |     is returned for (a,b) and (b,a), so the bond direction does not matter.
-- |   • vibratedEndpoints is RENDER-ONLY: it is a pure function of (BuilderState,
-- |     frame) and NEVER writes the BuilderState. The displacement s is applied
-- |     symmetrically about the bond midpoint (a' = a - s*u, b' = b + s*u), so
-- |     the midpoint is invariant — a property explicitly tested.
-- |   • NaN-safety: separationDir / tieBreakDir from Builder.Geom handle the
-- |     degenerate coincident-atom case, so no NaN can propagate.
-- |
-- | All functions are pure, total, deterministic; no Effect / WebGL imports.
module Builder.Vibration
  ( massProxy
  , reducedMass
  , bondOmega
  , bondPhase
  , vibFreqScale
  , zeroPointAmp
  , thermalAmp
  , vibAmplitude
  , vibratedEndpoints
  , vibratedBondLines
  , vibratedMidpoints
  ) where

import Prelude

import Atom (V3, elementOf)
import Builder.Geom (atomById, distance, separationDir)
import Builder.Types (BBond, BuilderState, PlacedAtom)
import Data.Array (foldl, snoc)
import Data.Int (toNumber)
import Data.Maybe (Maybe(..))
import Data.Number (cos, sin, sqrt)
import Pe (morseK)

-- ─── Mass proxy ──────────────────────────────────────────────────────────────

-- | Nucleon count (protons + neutrons) for element `z`, used as a mass proxy.
-- |
-- | Rationale: for the visual-only render, nucleon count is an excellent proxy
-- | for atomic mass (it matches the integer mass number A = Z + N and deviates
-- | from the true atomic mass by < 1% for all elements H..Kr). No separate
-- | atomic-mass table is needed — Atom.elementOf already carries the neutron
-- | count for the most-abundant isotope (neutronTable, index Z−1).
-- |
-- | Clamped via Atom.elementOf (which clamps Z to 1..36), so clamp-safe.
massProxy :: Int -> Number
massProxy z =
  let
    el = elementOf z
  in
    toNumber (el.protons + el.neutrons)

-- ─── Reduced mass ────────────────────────────────────────────────────────────

-- | Reduced mass of a diatomic pair: μ = m₁·m₂ / (m₁ + m₂).
-- |
-- | Properties:
-- |   • Symmetric: reducedMass a b == reducedMass b a.
-- |   • 0 < μ ≤ min(m₁, m₂) for positive masses.
-- |   • For H-H (m₁=m₂=1): μ = 0.5.
-- |
-- | Clamp-safety: massProxy is clamp-safe, so both m₁ and m₂ are always ≥ 1
-- | (since H has at least 1 proton), making the denominator ≥ 2 and the result
-- | always strictly positive and finite.
reducedMass :: Int -> Int -> Number
reducedMass z1 z2 =
  let
    m1 = massProxy z1
    m2 = massProxy z2
  in
    m1 * m2 / (m1 + m2)

-- ─── Vibration frequency ─────────────────────────────────────────────────────

-- | Scale factor converting √(k/μ) from Morse-potential units into rad/frame.
-- |
-- | Pe.morseK returns the harmonic spring constant k in (kJ/mol ÷ 100) per
-- | world-unit². With μ in nucleon-counts (dimensionless mass proxy), √(k/μ) has
-- | units of sqrt(energy/mass·length²) — not rad/frame. vibFreqScale absorbs the
-- | unit conversion plus a tuning factor chosen so H-H vibrates at ~0.3 rad/frame
-- | (≈20° per frame), fast enough to be visible but below the Nyquist limit (π
-- | rad/frame = 0.5 cycles/frame) so no stroboscopic aliasing occurs.
-- |
-- | Derivation: morseK(H-H) ≈ 0.021, mu(H-H) = 0.5.
-- |   omega_raw = sqrt(0.021 / 0.5) ≈ 0.205.
-- |   target omega ≈ 0.30 rad/frame => vibFreqScale ≈ 0.30 / 0.205 ≈ 1.46.
-- | We round to 1.5 for a clean constant. H-H will vibrate at ~0.31 rad/frame;
-- | C-C (k≈0.018, mu≈6) at ~0.067 rad/frame — visibly slower, as expected for a
-- | stiffer but much heavier bond.
vibFreqScale :: Number
vibFreqScale = 1.5

-- | Natural angular frequency (rad/frame) for a bond between elements z1 and z2:
-- |   ω = vibFreqScale · √(k / μ)
-- | where k = Pe.morseK z1 z2 and μ = reducedMass z1 z2.
-- |
-- | Properties:
-- |   • Monotone increasing in morseK at fixed μ (stiffer bonds buzz faster).
-- |   • Monotone decreasing in μ at fixed k (heavier pairs vibrate slower).
-- |   • Symmetric: bondOmega a b == bondOmega b a (both k and μ are symmetric).
-- |   • Always > 0 (k > 0 and μ > 0 by construction, sqrt and vibFreqScale > 0).
bondOmega :: Int -> Int -> Number
bondOmega z1 z2 =
  vibFreqScale * sqrt (morseK z1 z2 / reducedMass z1 z2)

-- ─── Phase ───────────────────────────────────────────────────────────────────

-- | Deterministic, symmetric phase offset for a bond between atom ids `a` and
-- | `b`. Derived from the integer ids only — no state, no random — so the same
-- | bond always starts at the same phase run-to-run (determinism), and the
-- | function is symmetric: bondPhase a b == bondPhase b a (the bond direction
-- | does not create a signed asymmetry in the visual jiggle).
-- |
-- | Formula: sin(sort(a,b) as (lo,hi) -> lo * 31.0 + hi * 17.0).
-- | The prime multipliers 31 and 17 spread integer id values into a dense phase
-- | range; taking sin maps the result to (−1, 1), keeping the initial phase
-- | bounded and the inter-bond decorrelation good for small id sets.
-- |
-- | Symmetry: by sorting (lo=min(a,b), hi=max(a,b)) the formula is identical
-- | for (a,b) and (b,a).
bondPhase :: Int -> Int -> Number
bondPhase a b =
  let
    lo = min a b
    hi = max a b
  in
    sin (toNumber lo * 31.0 + toNumber hi * 17.0)

-- ─── Amplitude constants ─────────────────────────────────────────────────────

-- | Zero-point (irreducible) vibration amplitude in world units.
-- |
-- | Every bond always carries at least this amplitude, so nothing is ever
-- | perfectly still — a visual metaphor for quantum zero-point motion.
-- | Value: 3.0 world units (≈ 3 pm scaled). Small enough that the nucleus
-- | centres stay well within their bond wells (bond R0 ≈ 160 wu), and large
-- | enough to be clearly visible at the Builder zoom level.
zeroPointAmp :: Number
zeroPointAmp = 3.0

-- | Classical thermal amplitude added on top of the zero-point jiggle, in world
-- | units. Together with zeroPointAmp this gives the total peak displacement
-- | vibAmplitude. Value: 7.0 world units, giving a total of 10 world units —
-- | comfortably below the Pauli floor gap (~30 wu for H-H) so the vibration
-- | never visually penetrates the partner nucleus.
thermalAmp :: Number
thermalAmp = 7.0

-- | Total vibration amplitude: zeroPointAmp + thermalAmp (10.0 world units).
-- | The signed displacement s at any frame satisfies |s| <= vibAmplitude.
vibAmplitude :: Number
vibAmplitude = zeroPointAmp + thermalAmp

-- ─── Vibrated endpoints ──────────────────────────────────────────────────────

-- | Render-only vibrated endpoints for every resolvable bond in the BuilderState.
-- |
-- | For each bond with atom ids (bd.a, bd.b):
-- |   1. Look up both atoms (skip unresolvable bonds, same as bondSegments).
-- |   2. Compute the unit axis u = separationDir(pa, pb, d) — NaN-safe via
-- |      Builder.Geom.tieBreakDir for coincident atoms.
-- |   3. Compute the signed displacement:
-- |        s = vibAmplitude · cos(bondOmega(za,zb) · frame + bondPhase(id_a, id_b))
-- |   4. Apply symmetrically about the model midpoint m:
-- |        a' = pos_a + s · u   (moves toward b when s > 0)
-- |        b' = pos_b − s · u   (moves toward a when s > 0)
-- |      Equivalently: a' = m - (half_dist - s)·u, b' = m + (half_dist - s)·u.
-- |      By construction (a'+b')/2 = (pos_a+pos_b)/2 = m exactly (no drift).
-- |
-- | NEVER mutates BuilderState. Pure, total, deterministic.
vibratedEndpoints :: BuilderState -> Number -> Array { a :: V3, b :: V3 }
vibratedEndpoints st frame = foldl collect [] st.bonds
  where
  collect acc bd =
    case atomById st bd.a, atomById st bd.b of
      Just pa, Just pb ->
        snoc acc (vibratedPair pa pb bd frame)
      _, _ -> acc

-- Compute the vibrated a/b endpoints for one resolved bond.
vibratedPair :: PlacedAtom -> PlacedAtom -> BBond -> Number -> { a :: V3, b :: V3 }
vibratedPair pa pb bd frame =
  let
    d = distance pa.pos pb.pos
    -- Unit axis from pa toward pb (NaN-safe: uses tieBreakDir when coincident).
    u = separationDir pa pb d
    -- Signed displacement amplitude.
    omega = bondOmega pa.z pb.z
    phase = bondPhase bd.a bd.b
    s = vibAmplitude * cos (omega * frame + phase)
    -- Vibrated endpoints: symmetric about the model midpoint.
    -- a moves in the +u direction by s, b moves in the -u direction by s.
    -- (positive s => a moves toward b, b moves toward a => compression)
    a' =
      { x: pa.pos.x + s * u.x
      , y: pa.pos.y + s * u.y
      , z: pa.pos.z + s * u.z
      }
    b' =
      { x: pb.pos.x - s * u.x
      , y: pb.pos.y - s * u.y
      , z: pb.pos.z - s * u.z
      }
  in
    { a: a', b: b' }

-- | Vibrated bond lines with order: the same as vibratedEndpoints but each
-- | entry also carries the bond's ORDER, so the renderer can place N parallel
-- | lines per bond (one sigma + (order-1) flanking pi lines).
-- |
-- | The {a, b} endpoints are the same vibrated values produced by
-- | vibratedEndpoints for the same (st, frame) — adding order is a zero-cost
-- | zip over the bond array. Pure, total, deterministic. Never mutates
-- | BuilderState.
vibratedBondLines :: BuilderState -> Number -> Array { a :: V3, b :: V3, order :: Int }
vibratedBondLines st frame = foldl collect [] st.bonds
  where
  collect acc bd =
    case atomById st bd.a, atomById st bd.b of
      Just pa, Just pb ->
        let ep = vibratedPair pa pb bd frame
        in snoc acc { a: ep.a, b: ep.b, order: bd.order }
      _, _ -> acc

-- | Midpoints of the vibrated bond endpoints.
-- |
-- | By the symmetry of vibratedEndpoints (a' and b' are displaced by equal and
-- | opposite amounts about the model midpoint), vibratedMidpoints is provably
-- | equal to Bonds.bondMidpoints: the midpoint is invariant under the symmetric
-- | displacement. This function is provided so the shared-pair renderer can use
-- | the same midpoint source as the bond-line renderer without re-deriving it.
-- |
-- | Pure, total, deterministic. Never mutates BuilderState.
vibratedMidpoints :: BuilderState -> Number -> Array V3
vibratedMidpoints st _ = foldl collect [] st.bonds
  where
  collect acc bd =
    case atomById st bd.a, atomById st bd.b of
      Just pa, Just pb ->
        -- The midpoint is frame-invariant (symmetric displacement cancels).
        -- We compute it directly from the model positions (no need to go
        -- through vibratedPair and average — exact same result, simpler).
        snoc acc
          { x: (pa.pos.x + pb.pos.x) / 2.0
          , y: (pa.pos.y + pb.pos.y) / 2.0
          , z: (pa.pos.z + pb.pos.z) / 2.0
          }
      _, _ -> acc
