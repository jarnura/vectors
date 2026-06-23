-- Lone / bonding electron placement for the Builder (electron conservation):
-- bond degree, lone counts, the valence shell split, and the model-space
-- positions of shared bonding pairs and per-atom core/valence lone electrons —
-- both flat arrays and per-atom grouped forms for the LOD bloom cross-fade.
-- Also provides phase-aware (Bonding / Antibonding) shared-electron placement:
-- the Bonding phase is byte-identical to bondElectronPositions; the Antibonding
-- phase emits a node at the bond midpoint with electrons pushed outward past
-- each nucleus (same electron count — conservation unchanged).
-- M3-S3: spin tags — a pure VISUAL label (Up | Down) for the sigma-paired
-- singlet electrons of each bond. The sigma pair is the FIRST 2 electrons per
-- bond (indices 0 and 1 of bondPairAt). One is tagged Up, its mirror Down,
-- forming a singlet (paired anti-parallel spins, as required by Pauli exclusion).
-- Pi pairs (bond order > 1) receive the same Up/Down alternating label so the
-- renderer can tint all bond electrons. VISUAL LAYER ONLY: exchange-integral
-- energetics (singlet vs. triplet splitting) are NOT modelled here — see
-- a future QM solver for that.
-- Pure, total, deterministic — no Effect/WebGL.
module Builder.Electrons
  ( BondPhase(..)
  , Spin(..)
  , degreeOf
  , loneCountOf
  , valenceShellOf
  , bondElectronPositions
  , bondElectronPositionsPhased
  , bondElectronGroups
  , bondElectronGroupsPhased
  , bondSigmaSpins
  , loneElectronPositions
  , coreLoneElectronPositions
  , valenceLoneElectronPositions
  , coreLoneElectronGroups
  , valenceLoneElectronGroups
  ) where

import Prelude

import Atom (V3, electronShells, nucleonRadius, nucleusRadius)
import Builder.Geom (atomById, degreeIn, distance)
import Builder.Types (BuilderState, PlacedAtom)
import Data.Array (concat, concatMap, foldl, init, last, length, mapWithIndex, range, snoc, sortBy, uncons)
import Data.Int (floor, toNumber)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Number (asin, atan2, cos, pi, sin, sqrt)

-- Phase selector for shared (bonding) electron placement.
-- Bonding:    the standard in-phase pair — sigma pair straddles the bond
--             midpoint along the axis, PI pairs perpendicular (byte-identical to
--             the existing bondElectronPositions).
-- Antibonding: out-of-phase — a NODE at the midpoint (zero electron density in
--             the middle); the shared electrons are pushed OUTWARD past each
--             nucleus along the bond axis (sigma) or outward along their
--             perpendicular direction (pi). Electron count is conserved: 2*order
--             per bond in both phases.
data BondPhase = Bonding | Antibonding

derive instance eqBondPhase :: Eq BondPhase

-- M3-S3: spin quantum number tag for a shared bond electron.
-- Up and Down represent the two anti-parallel spin projections (m_s = +½ and
-- m_s = −½) of the Pauli-mandated singlet pair. The sigma pair carries one Up
-- and one Down (in that positional order from bondPairAt, index 0 → Up, 1 → Down).
-- Pi pairs (bond order > 1) are tagged with the same Up/Down alternating pattern
-- so every bond electron gets a spin label.
-- VISUAL ONLY: exchange-integral energetics (singlet vs. triplet energy gap)
-- are deferred — a QM solver is needed for that. Do NOT use Spin for energy
-- calculations without the full Hamiltonian.
data Spin = Up | Down

derive instance eqSpin :: Eq Spin
derive instance ordSpin :: Ord Spin

-- Return the spin tag for every shared bond electron in the state, in the same
-- order as `bondElectronPositions`: for each bond (in st.bonds order) the first
-- 2*order positions carry alternating spins [Up, Down, Up, Down, ...], so the
-- sigma pair (positions 0,1) is always (Up, Down) — the singlet — and each
-- subsequent PI pair also carries (Up, Down).
-- Length equals length (bondElectronPositions st frame) for any frame.
-- Deterministic, pure, total. Does not depend on frame (spins are static labels).
bondSigmaSpins :: BuilderState -> Array Spin
bondSigmaSpins st = concatMap spinForBond st.bonds
  where
  -- Each bond of order n contributes 2*n spin tags: alternating Up/Down.
  spinForBond bd = map (\i -> if i `mod` 2 == 0 then Up else Down)
    (range 0 (2 * bd.order - 1))

-- Phase-aware shared electron positions. Bonding is byte-identical to
-- bondElectronPositions. Antibonding places the SAME number of electrons but
-- with a node at the midpoint (electrons pushed outward past each nucleus).
-- Pure, total, deterministic, NaN-safe.
bondElectronPositionsPhased :: BondPhase -> BuilderState -> Number -> Array V3
bondElectronPositionsPhased phase st frame = concatMap bondPair st.bonds
  where
  bondPair bd =
    case atomById st bd.a, atomById st bd.b of
      Just a, Just b ->
        case phase of
          Bonding -> bondPairAt bd.order a b frame
          Antibonding -> antibondPairAt bd.order a b frame
      _, _ -> []

-- Number of bonds incident to atom `aid` in the world's bond set. A public view
-- of the internal degree count, exposed for the renderer's electron clouds.
degreeOf :: BuilderState -> Int -> Int
degreeOf st aid = degreeIn st.bonds aid

-- Lone (non-bonding) electron count for atom `aid`: its element valence minus its
-- bond degree, clamped at 0. Unknown ids (no such atom) yield 0. Together with the
-- bonding electrons (2 per incident bond) this conserves Chem.valence per atom.
-- An atom shows ALL its electrons (Z for a neutral atom): the ones used in bonds
-- live in the bonds (degree of them), the rest are its lone electrons. So a free
-- Carbon shows 6, a free Oxygen 8 — not just the valence/bonding count. Bond
-- FORMATION is still capped by `valence` in recomputeBonds; this is display only.
loneCountOf :: BuilderState -> Int -> Int
loneCountOf st aid =
  case atomById st aid of
    Just a -> max 0 (a.z - degreeOf st aid)
    Nothing -> 0

-- Shared (bonding) electron positions: 2*order electrons per resolvable bond.
-- Order 1 (single bond): 1 sigma pair mirrored about the midpoint along the bond
-- axis — byte-identical to the pre-S4 formula. Order N > 1: 1 sigma pair plus
-- (N-1) PI pairs offset PERPENDICULAR to the bond axis, each pair also mirrored
-- about the midpoint and frame-animated. Total length = Σ 2*order over bonds.
-- Pure, total, deterministic. Model-space.
-- Defined as bondElectronPositionsPhased Bonding for byte-identical behaviour.
bondElectronPositions :: BuilderState -> Number -> Array V3
bondElectronPositions = bondElectronPositionsPhased Bonding

-- The bloom centre + the 2*order shared electrons of each resolvable bond (the
-- bond MIDPOINT and its electron positions). Used by the renderer's LOD
-- cross-fade so a bond's shared electrons bloom out of the bond midpoint as
-- detail rises. Same data as `bondElectronPositions`, grouped by bond.
bondElectronGroups
  :: BuilderState -> Number -> Array { center :: V3, positions :: Array V3 }
bondElectronGroups st frame = foldl collect [] st.bonds
  where
  collect acc bd =
    case atomById st bd.a, atomById st bd.b of
      Just a, Just b ->
        let
          mid =
            { x: (a.pos.x + b.pos.x) / 2.0
            , y: (a.pos.y + b.pos.y) / 2.0
            , z: (a.pos.z + b.pos.z) / 2.0
            }
        in
          snoc acc { center: mid, positions: bondPairAt bd.order a b frame }
      _, _ -> acc

-- Phase-aware companion to `bondElectronGroups`: the bloom centre is always the
-- bond midpoint (same as the Bonding case — the LOD detail bloom is
-- centre-relative, so using the midpoint gives a sensible fade for antibonding
-- electrons too). The positions array carries the phase-appropriate electrons
-- (Bonding = bondPairAt, Antibonding = antibondPairAt). Render-only; no model
-- mutation. `bondElectronGroups = bondElectronGroupsPhased Bonding`.
bondElectronGroupsPhased
  :: BondPhase -> BuilderState -> Number -> Array { center :: V3, positions :: Array V3 }
bondElectronGroupsPhased phase st frame = foldl collect [] st.bonds
  where
  collect acc bd =
    case atomById st bd.a, atomById st bd.b of
      Just a, Just b ->
        let
          mid =
            { x: (a.pos.x + b.pos.x) / 2.0
            , y: (a.pos.y + b.pos.y) / 2.0
            , z: (a.pos.z + b.pos.z) / 2.0
            }
          positions = case phase of
            Bonding -> bondPairAt bd.order a b frame
            Antibonding -> antibondPairAt bd.order a b frame
        in
          snoc acc { center: mid, positions }
      _, _ -> acc

-- The 2*order shared electrons for a bond of the given order between endpoints
-- `a`/`b` at `frame`. For order 1: the existing sigma pair — a mirrored pair
-- about the midpoint with a frame-driven offset along the bond axis and a small
-- transverse breathe. For order N > 1: the sigma pair followed by (N-1) PI
-- pairs, each a mirrored pair offset PERPENDICULAR to the bond axis by
-- `piRadius`, frame-animated. Extracted so both `bondElectronPositions` and
-- `bondElectronGroups` share one definition.
--
-- NaN-safe: a coincident or degenerate axis falls back to tieBreakDir-like
-- perpendicular construction (world-X or world-Y, always finite).
--
-- ORDER-1 BYTE-IDENTITY: at order=1, only the sigma pair is produced; the
-- formula is identical to the pre-S4 implementation.
bondPairAt :: Int -> PlacedAtom -> PlacedAtom -> Number -> Array V3
bondPairAt order a b frame =
  let
    mid =
      { x: (a.pos.x + b.pos.x) / 2.0
      , y: (a.pos.y + b.pos.y) / 2.0
      , z: (a.pos.z + b.pos.z) / 2.0
      }
    span = distance a.pos b.pos / 2.0
    speed = 0.03
    phase = frame * speed
    -- SIGMA pair: along (a rough projection of the bond axis onto the frame
    -- animation). Same formula as pre-S4 for order-1 byte-identity.
    dx = 0.25 * span * cos phase
    dy = electronCloud * sin phase
    dz = electronCloud * cos phase
    sigmaPair =
      [ { x: mid.x + dx, y: mid.y + dy, z: mid.z + dz }
      , { x: mid.x - dx, y: mid.y - dy, z: mid.z - dz }
      ]
  in
    if order <= 1 then
      -- Order 1: byte-identical to pre-S4 output.
      sigmaPair
    else
      -- Order > 1: sigma pair + (order-1) PI pairs perpendicular to the axis.
      sigmaPair <> piPairs order a b mid frame

-- Build (order - 1) PI pairs offset PERPENDICULAR to the bond axis.
-- The perpendicular basis {p1, p2} is derived deterministically from the unit
-- bond axis `u`:
--   ref = world-Y ({0,1,0}), unless |u.y| > 0.9 (nearly parallel to Y),
--         in which case ref = world-X ({1,0,0}).
--   p1 = normalize(ref × u)   (first perpendicular; always non-zero by construction)
--   p2 = normalize(u × p1)    (second perpendicular; orthogonal to both u and p1)
-- PI pair k (k = 1 .. order-1) is placed at angle θ_k = k * π / order around
-- the axis, i.e. the direction cos(θ_k)*p1 + sin(θ_k)*p2, at offset `piRadius`
-- from the axis, mirrored about the midpoint, frame-animated.
-- NaN-safe: all inputs are finite by construction (distance ≥ 0; if zero the
-- fallback ref gives a valid non-zero cross product with any non-zero vector).
piPairs :: Int -> PlacedAtom -> PlacedAtom -> V3 -> Number -> Array V3
piPairs order a b mid frame =
  let
    -- Raw axis vector (not yet normalised).
    axRaw = { x: b.pos.x - a.pos.x, y: b.pos.y - a.pos.y, z: b.pos.z - a.pos.z }
    d = sqrt (axRaw.x * axRaw.x + axRaw.y * axRaw.y + axRaw.z * axRaw.z)
    -- Safe normalised axis: fallback to world-X if degenerate (d < eps).
    u =
      if d < 1.0e-9 then { x: 1.0, y: 0.0, z: 0.0 }
      else { x: axRaw.x / d, y: axRaw.y / d, z: axRaw.z / d }
    -- Reference vector: world-Y unless nearly parallel to u.
    ref =
      if absN u.y > 0.9 then { x: 1.0, y: 0.0, z: 0.0 }
      else { x: 0.0, y: 1.0, z: 0.0 }
    -- p1 = normalize(ref × u)
    cross1 = crossV3 ref u
    lenC1 = sqrt (cross1.x * cross1.x + cross1.y * cross1.y + cross1.z * cross1.z)
    p1 =
      if lenC1 < 1.0e-9 then { x: 1.0, y: 0.0, z: 0.0 }
      else { x: cross1.x / lenC1, y: cross1.y / lenC1, z: cross1.z / lenC1 }
    -- p2 = normalize(u × p1)
    cross2 = crossV3 u p1
    lenC2 = sqrt (cross2.x * cross2.x + cross2.y * cross2.y + cross2.z * cross2.z)
    p2 =
      if lenC2 < 1.0e-9 then { x: 0.0, y: 0.0, z: 1.0 }
      else { x: cross2.x / lenC2, y: cross2.y / lenC2, z: cross2.z / lenC2 }
    -- Frame-animation phase for PI pairs (different speed/phase from sigma).
    piSpeed = 0.025
    piPhase = frame * piSpeed
    -- Build the (order-1) PI pairs.
    numPi = order - 1
  in
    concatMap
      ( \k ->
          let
            -- Distribute PI pairs evenly around the axis: θ_k = k * π / order.
            theta = toNumber k * pi / toNumber order
            -- Perpendicular direction for this PI pair.
            cT = cos theta
            sT = sin theta
            -- Radial offset from the midpoint along this perpendicular direction.
            ox = piRadius * (cT * p1.x + sT * p2.x)
            oy = piRadius * (cT * p1.y + sT * p2.y)
            oz = piRadius * (cT * p1.z + sT * p2.z)
            -- Small frame-driven breathe ALONG the perpendicular (not the axis).
            breathe = electronCloud * cos (piPhase + toNumber k * 1.2)
            bx = breathe * (cT * p1.x + sT * p2.x)
            by = breathe * (cT * p1.y + sT * p2.y)
            bz = breathe * (cT * p1.z + sT * p2.z)
          in
            [ { x: mid.x + ox + bx, y: mid.y + oy + by, z: mid.z + oz + bz }
            , { x: mid.x - ox - bx, y: mid.y - oy - by, z: mid.z - oz - bz }
            ]
      )
      (range 1 numPi)

-- ──────────────────────────────────────────────────────────────────────────────
-- ANTIBONDING placement: 2*order electrons per bond, node at the midpoint.
-- ──────────────────────────────────────────────────────────────────────────────

-- The 2*order anti-bonding electrons for a bond between endpoints `a`/`b`.
-- Sigma pair (1st pair): the two electrons are placed OUTWARD past each nucleus
-- along the bond axis — one beyond atom a (on the far side from b) and one
-- beyond atom b (on the far side from a), so the midpoint has zero density (the
-- node). Frame-animated with a bounded breathe like the bonding case.
-- Pi pairs (order>1): the (order-1) perpendicular pairs are also split outward —
-- each pair's two electrons are pushed to the a-side / b-side along the axis at
-- `antiAxialShift`, while keeping the same perpendicular offset `piRadius`, so
-- they clear the midpoint region. Deterministic, NaN-safe (same coincident-axis
-- fallback as bondPairAt / piPairs).
-- Electron count: always 2*order per bond (same as the bonding branch).
antibondPairAt :: Int -> PlacedAtom -> PlacedAtom -> Number -> Array V3
antibondPairAt order a b frame =
  let
    -- Raw axis.
    axRaw = { x: b.pos.x - a.pos.x, y: b.pos.y - a.pos.y, z: b.pos.z - a.pos.z }
    d = sqrt (axRaw.x * axRaw.x + axRaw.y * axRaw.y + axRaw.z * axRaw.z)
    -- Safe normalised axis (fallback world-X for degenerate pairs).
    u =
      if d < 1.0e-9 then { x: 1.0, y: 0.0, z: 0.0 }
      else { x: axRaw.x / d, y: axRaw.y / d, z: axRaw.z / d }
    -- Half the bond length: distance from midpoint to each nucleus.
    halfBond = d / 2.0
    -- Outward displacement beyond each nucleus: nucleus + outwardExtra beyond midpoint.
    -- We push each electron past the nucleus by outwardExtra model units, so it
    -- clearly clears both the midpoint AND the nucleus.
    outwardExtra = electronCloud * 2.0
    outwardDist = halfBond + outwardExtra
    -- Frame-driven bounded breathe along the axis (same speed as the bonding sigma).
    speed = 0.03
    breathe = electronCloud * cos (frame * speed)
    -- The two sigma electrons: one beyond a (in -u direction) and one beyond b (+u).
    sigmaA =
      { x: a.pos.x - u.x * (outwardDist + breathe)
      , y: a.pos.y - u.y * (outwardDist + breathe)
      , z: a.pos.z - u.z * (outwardDist + breathe)
      }
    sigmaB =
      { x: b.pos.x + u.x * (outwardDist + breathe)
      , y: b.pos.y + u.y * (outwardDist + breathe)
      , z: b.pos.z + u.z * (outwardDist + breathe)
      }
    sigmaPair = [ sigmaA, sigmaB ]
  in
    if order <= 1 then
      sigmaPair
    else
      sigmaPair <> antiPiPairs order a b u d frame

-- Build (order - 1) anti-bonding PI pairs. Each pair's two electrons are
-- pushed to the a-side / b-side of the bond along the axis at `antiAxialShift`
-- (so they clear the midpoint) while keeping the perpendicular offset `piRadius`
-- from the axis. The perpendicular basis {p1, p2} is computed exactly as in
-- `piPairs` (same NaN-safe construction) for determinism.
antiPiPairs :: Int -> PlacedAtom -> PlacedAtom -> V3 -> Number -> Number -> Array V3
antiPiPairs order a b u d frame =
  let
    -- Reference vector for perpendicular basis (same as piPairs).
    ref =
      if absN u.y > 0.9 then { x: 1.0, y: 0.0, z: 0.0 }
      else { x: 0.0, y: 1.0, z: 0.0 }
    cross1 = crossV3 ref u
    lenC1 = sqrt (cross1.x * cross1.x + cross1.y * cross1.y + cross1.z * cross1.z)
    p1 =
      if lenC1 < 1.0e-9 then { x: 1.0, y: 0.0, z: 0.0 }
      else { x: cross1.x / lenC1, y: cross1.y / lenC1, z: cross1.z / lenC1 }
    cross2 = crossV3 u p1
    lenC2 = sqrt (cross2.x * cross2.x + cross2.y * cross2.y + cross2.z * cross2.z)
    p2 =
      if lenC2 < 1.0e-9 then { x: 0.0, y: 0.0, z: 1.0 }
      else { x: cross2.x / lenC2, y: cross2.y / lenC2, z: cross2.z / lenC2 }
    -- Midpoint (from a and b positions, derivable from a + d*u/2).
    mid =
      { x: (a.pos.x + b.pos.x) / 2.0
      , y: (a.pos.y + b.pos.y) / 2.0
      , z: (a.pos.z + b.pos.z) / 2.0
      }
    -- Each PI electron is shifted ±antiAxialShift along the axis from the midpoint,
    -- clearing the midpoint region. We use a fraction of the half-bond length so
    -- the shift scales with bond length; at least outwardExtra to clear degenerate bonds.
    halfBond = d / 2.0
    antiAxialShift = halfBond * 0.6 + electronCloud
    -- Frame phase for PI breathe.
    piSpeed = 0.025
    piPhase = frame * piSpeed
    numPi = order - 1
  in
    concatMap
      ( \k ->
          let
            theta = toNumber k * pi / toNumber order
            cT = cos theta
            sT = sin theta
            -- Perpendicular offset (same as piPairs).
            ox = piRadius * (cT * p1.x + sT * p2.x)
            oy = piRadius * (cT * p1.y + sT * p2.y)
            oz = piRadius * (cT * p1.z + sT * p2.z)
            -- Frame-driven breathe along the perpendicular (same as piPairs).
            breathe = electronCloud * cos (piPhase + toNumber k * 1.2)
            bx = breathe * (cT * p1.x + sT * p2.x)
            by = breathe * (cT * p1.y + sT * p2.y)
            bz = breathe * (cT * p1.z + sT * p2.z)
            -- Axial shift: one electron to the a-side (-u), one to the b-side (+u).
            sA = antiAxialShift
            sB = antiAxialShift
          in
            -- Electron on the a-side: midpoint - sA*u + perp offset
            [ { x: mid.x - u.x * sA + ox + bx
              , y: mid.y - u.y * sA + oy + by
              , z: mid.z - u.z * sA + oz + bz
              }
            -- Electron on the b-side: midpoint + sB*u - perp offset (mirrored perp).
            , { x: mid.x + u.x * sB - ox - bx
              , y: mid.y + u.y * sB - oy - by
              , z: mid.z + u.z * sB - oz - bz
              }
            ]
      )
      (range 1 numPi)

-- Cross product of two V3 vectors.
crossV3 :: V3 -> V3 -> V3
crossV3 u v =
  { x: u.y * v.z - u.z * v.y
  , y: u.z * v.x - u.x * v.z
  , z: u.x * v.y - u.y * v.x
  }

-- Absolute value of a Number (local alias so we don't import Data.Number.abs).
absN :: Number -> Number
absN x = if x < 0.0 then -x else x

-- Radial offset of PI electron pairs from the bond axis. Sized like the
-- sigma breathe so both layers read at the same scale. Model-space units.
piRadius :: Number
piRadius = electronCloud

-- Per-atom CORE lone electrons grouped with the bloom centre (the atom centre):
-- the same data as `coreLoneElectronPositions`, kept per atom so the renderer can
-- bloom each atom's core electrons out of that atom's centre under the LOD fade.
coreLoneElectronGroups
  :: BuilderState -> Number -> Array { center :: V3, positions :: Array V3 }
coreLoneElectronGroups st frame =
  map (\a -> { center: a.pos, positions: atomCorePositions st a frame }) st.atoms

-- Per-atom VALENCE lone electrons grouped with the bloom centre (the atom
-- centre): the per-atom companion to `valenceLoneElectronPositions`.
valenceLoneElectronGroups
  :: BuilderState -> Number -> Array { center :: V3, positions :: Array V3 }
valenceLoneElectronGroups st frame =
  map (\a -> { center: a.pos, positions: atomValencePositions st a frame }) st.atoms

-- Lone electron positions: `loneCountOf` electrons per atom, orbiting on a ring
-- clearly OUTSIDE the nucleus cluster (radius `loneOrbitRadius`) around the atom
-- centre, evenly spaced and rotated by the frame so they visibly orbit the
-- nucleus. Length = Σ loneCountOf over atoms. Deterministic for a fixed frame.
-- Pure, total. Model-space.
-- Lone electrons arranged in concentric Bohr SHELLS: an atom's lone electrons
-- fill shells inner-first (capacities 2, 8, 18, 32), each shell a ring at an
-- increasing radius, evenly spaced and frame-rotated (inner shells faster). So a
-- free Carbon (6) shows 2 on an inner ring + 4 on an outer ring; a free Oxygen (8)
-- → 2 + 6; Hydrogen (1) → 1. Length = Σ loneCountOf over atoms. Deterministic for
-- a fixed frame. Pure, total. Model-space.
loneElectronPositions :: BuilderState -> Number -> Array V3
loneElectronPositions st frame =
  coreLoneElectronPositions st frame <> valenceLoneElectronPositions st frame

-- Outermost-shell electron count of element `z`: the last entry of
-- Atom.electronShells (per-principal-shell totals). Carbon→4, Oxygen→6,
-- Hydrogen→1, Neon→8. Clamp-safe (0 for an empty/unknown shell list).
valenceShellOf :: Int -> Int
valenceShellOf z = fromMaybe 0 (last (electronShells z))

-- CORE lone electrons: the inner (always-lone) shells. Per atom, coreCount =
-- z − valenceShellOf z electrons are distributed across the inner shell rings
-- (indices 0..numInner-1) using the SAME ring layout as the old single-pass
-- fillShells placement. Length = Σ (z − valenceShellOf z) over atoms.
-- Deterministic for a fixed frame. Pure, total. Model-space.
coreLoneElectronPositions :: BuilderState -> Number -> Array V3
coreLoneElectronPositions st frame =
  concatMap (\a -> atomCorePositions st a frame) st.atoms

-- CORE lone electron positions for a single atom (the inner always-lone shells).
-- Shared by `coreLoneElectronPositions` (flat) and `coreLoneElectronGroups`.
atomCorePositions :: BuilderState -> PlacedAtom -> Number -> Array V3
atomCorePositions st a frame =
  let
    coreCount = a.z - valenceShellOf a.z
    dirs = bondDirsXY st a
  in
    concat (mapWithIndex (\i count -> shellRing a dirs frame i count) (fillShells coreCount))

-- VALENCE lone electrons: the outermost shell's non-bonding electrons. Per atom,
-- valenceLone = max 0 (valenceShellOf z − degree) electrons sit on the VALENCE
-- ring at index numInner (= number of core shells = length (electronShells z) −
-- 1), so the valence ring is strictly outside every core ring. Length =
-- Σ max 0 (valenceShellOf z − degree). Deterministic for a fixed frame. Pure,
-- total. Model-space.
valenceLoneElectronPositions :: BuilderState -> Number -> Array V3
valenceLoneElectronPositions st frame =
  concatMap (\a -> atomValencePositions st a frame) st.atoms

-- VALENCE lone electron positions for a single atom (its outermost-shell
-- non-bonding electrons). Shared by `valenceLoneElectronPositions` (flat) and
-- `valenceLoneElectronGroups`.
atomValencePositions :: BuilderState -> PlacedAtom -> Number -> Array V3
atomValencePositions st a frame =
  let
    valenceCount = valenceShellOf a.z
    valenceLone = max 0 (valenceCount - degreeOf st a.id)
    numInner = max 0 (length (electronShells a.z) - 1)
    dirs = bondDirsXY st a
  in
    shellRing a dirs frame numInner valenceLone

-- Bond-direction angles in the XY plane from atom `a` to each bonded neighbour.
-- Returns `atan2(dy, dx)` for each non-degenerate neighbour (dx²+dy² > eps).
-- Degenerate coincident pairs are dropped (NaN-safe). Pure, total.
bondDirsXY :: BuilderState -> PlacedAtom -> Array Number
bondDirsXY st a =
  let
    bondedIds = concatMap
      ( \bd ->
          if bd.a == a.id then [ bd.b ]
          else if bd.b == a.id then [ bd.a ]
          else []
      )
      st.bonds
  in
    concatMap
      ( \bid ->
          case atomById st bid of
            Nothing -> []
            Just other ->
              let
                dx = other.pos.x - a.pos.x
                dy = other.pos.y - a.pos.y
              in
                if dx * dx + dy * dy < 1.0e-12 then []
                else [ atan2 dy dx ]
      )
      bondedIds

-- Shared ring-placement helper: `count` electrons placed on the ring at shell
-- index `idx` around atom `a`'s centre, frame-rotated, with partner-facing sectors
-- excluded via `bondDirs`.
--
-- When `bondDirs` is empty the placement is BYTE-IDENTICAL to the old full-circle
-- even spacing (theta = 2π*k/count + phase), preserving all free-atom tests.
--
-- When `bondDirs` is non-empty, each bond direction `dir` contributes an
-- excluded sector [dir − excludeHalf, dir + excludeHalf] where
-- excludeHalf = asin(min 1.0 (nucleusRadius * safetyMargin / r)).
-- The `count` electrons are distributed evenly across the union of safe arcs
-- (complement of excluded sectors), so the COUNT is always exactly `count`.
-- Radius and phase are the same as before; z = a.pos.z (model-space XY ring).
shellRing :: PlacedAtom -> Array Number -> Number -> Int -> Int -> Array V3
shellRing a bondDirs frame idx count
  | count <= 0 = []
  | otherwise =
      let
        r = loneOrbitRadius + toNumber idx * shellSpacing
        phase = frame * (0.05 / (toNumber idx + 1.0))
      in
        if length bondDirs == 0 then
          -- Free-atom path: byte-identical to previous implementation.
          map
            ( \k ->
                let
                  theta = 2.0 * pi * toNumber k / toNumber (max 1 count) + phase
                in
                  { x: a.pos.x + r * cos theta
                  , y: a.pos.y + r * sin theta
                  , z: a.pos.z
                  }
            )
            (range 0 (count - 1))
        else
          -- Angular-exclusion path: sample `count` angles evenly across safe arcs.
          let
            excludeHalf = asin (min 1.0 (nucleusRadius * safetyMargin / r))
            safeArcs = buildSafeArcs bondDirs excludeHalf
            totalLen = foldl (\acc arc -> acc + arc.len) 0.0 safeArcs
            -- Scale the frame phase offset into arc-space [0, totalLen).
            phaseOffset =
              if totalLen > 0.0 then moduloF (phase * totalLen / (2.0 * pi)) totalLen
              else 0.0
          in
            map
              ( \k ->
                  let
                    arcPos =
                      if totalLen > 0.0 then
                        moduloF
                          (toNumber k * totalLen / toNumber count + phaseOffset)
                          totalLen
                      else
                        0.0
                    theta = arcPosToAngle safeArcs arcPos
                  in
                    { x: a.pos.x + r * cos theta
                    , y: a.pos.y + r * sin theta
                    , z: a.pos.z
                    }
              )
              (range 0 (count - 1))

-- Safety margin multiplier for the exclusion half-angle: the excluded sector
-- sweeps a chord of `nucleusRadius * safetyMargin` at each bond direction,
-- keeping lone electrons comfortably outside the partner nucleus.
safetyMargin :: Number
safetyMargin = 1.6

-- Build the list of safe arcs (complement of excluded sectors) on [0, 2π).
-- Each arc has a start angle and a length. Returns the full circle if all
-- sectors are excluded (degenerate; should not occur for sensible inputs).
buildSafeArcs :: Array Number -> Number -> Array { start :: Number, len :: Number }
buildSafeArcs bondDirs excludeHalf =
  let
    twoPi = 2.0 * pi
    -- Build excluded intervals, with wraparound splitting at 0/2π.
    rawIntervals = concatMap (dirToIntervals twoPi excludeHalf) bondDirs
    -- Sort by start angle for merging.
    sorted = sortBy (\x y -> compare x.lo y.lo) rawIntervals
    -- Merge overlapping/adjacent intervals.
    merged = mergeIntervals sorted
    -- Complement of merged intervals in [0, 2π).
    safes = complementArcs merged twoPi
  in
    safes

-- Convert a bond-direction angle to one or two excluded intervals on [0, 2π).
-- An interval that crosses 0 is split into two pieces.
dirToIntervals
  :: Number -> Number -> Number -> Array { lo :: Number, hi :: Number }
dirToIntervals twoPi excHalf dir =
  let
    -- Normalise dir to [0, 2π).
    nDir = moduloF dir twoPi
    lo = nDir - excHalf
    hi = nDir + excHalf
  in
    if lo < 0.0 && hi >= twoPi then
      -- Covers the full circle.
      [ { lo: 0.0, hi: twoPi } ]
    else if lo < 0.0 then
      -- Wraps below 0: split into tail and head.
      [ { lo: lo + twoPi, hi: twoPi }, { lo: 0.0, hi: hi } ]
    else if hi >= twoPi then
      -- Wraps above 2π: split.
      [ { lo: lo, hi: twoPi }, { lo: 0.0, hi: hi - twoPi } ]
    else
      [ { lo: lo, hi: hi } ]

-- Merge a sorted array of intervals, combining overlapping/adjacent ones.
-- Input must be sorted by `lo`. Returns a sorted, non-overlapping array.
-- We accumulate into a list where the LAST element is the open (current) interval.
mergeIntervals
  :: Array { lo :: Number, hi :: Number } -> Array { lo :: Number, hi :: Number }
mergeIntervals intervals = case uncons intervals of
  Nothing -> []
  Just { head, tail } ->
    foldl
      ( \acc ivl ->
          case last acc of
            Nothing -> [ ivl ]
            Just top ->
              let
                initAcc = fromMaybe [] (init acc)
              in
                if ivl.lo <= top.hi then
                  -- Overlapping: extend the last interval.
                  snoc initAcc { lo: top.lo, hi: max top.hi ivl.hi }
                else
                  snoc acc ivl
      )
      [ head ]
      tail

-- Complement of a sorted, non-overlapping array of intervals in [0, twoPi).
-- Returns safe arc segments as { start, len }.
complementArcs
  :: Array { lo :: Number, hi :: Number }
  -> Number
  -> Array { start :: Number, len :: Number }
complementArcs merged twoPi =
  let
    -- Walk the gaps between consecutive intervals.
    result = foldl
      ( \acc ivl ->
          let
            gapLen = ivl.lo - acc.cursor
            newArcs =
              if gapLen > 1.0e-12 then
                snoc acc.arcs { start: acc.cursor, len: gapLen }
              else
                acc.arcs
          in
            { cursor: ivl.hi, arcs: newArcs }
      )
      { cursor: 0.0, arcs: [] }
      merged
    -- Final gap from last interval end to 2π.
    finalLen = twoPi - result.cursor
    allArcs =
      if finalLen > 1.0e-12 then
        snoc result.arcs { start: result.cursor, len: finalLen }
      else
        result.arcs
  in
    -- If nothing is safe (total exclusion), emit the whole circle as fallback.
    if length allArcs == 0 then [ { start: 0.0, len: twoPi } ]
    else allArcs

-- Map an arc-space position (0 ≤ pos < totalLen) to an actual angle in [0, 2π).
-- Walks the safe-arc list; each arc contributes its length to the offset.
arcPosToAngle :: Array { start :: Number, len :: Number } -> Number -> Number
arcPosToAngle arcs pos = go arcs pos
  where
  go remaining arcPos = case uncons remaining of
    Nothing -> 0.0 -- degenerate: no arcs (should not happen)
    Just { head: arc, tail: rest } ->
      if arcPos < arc.len || length rest == 0 then
        arc.start + arcPos
      else
        go rest (arcPos - arc.len)

-- Floating-point modulo: x mod m, result in [0, m).
-- Uses Data.Int.floor for the quotient, which is exact for all finite inputs.
moduloF :: Number -> Number -> Number
moduloF x m =
  let
    q = x / m
    floored = toNumber (floor q)
  in
    x - floored * m

-- Distribute `total` electrons into shells (capacities 2, 8, 18, 32), filling the
-- inner shells first; any overflow lands in a final shell. fillShells 6 = [2,4],
-- fillShells 8 = [2,6], fillShells 1 = [1], fillShells 0 = [].
shellCapacities :: Array Int
shellCapacities = [ 2, 8, 18, 32 ]

fillShells :: Int -> Array Int
fillShells = go shellCapacities
  where
  go caps remaining
    | remaining <= 0 = []
    | otherwise = case uncons caps of
        Nothing -> [ remaining ]
        Just { head: cap, tail } ->
          let
            here = min remaining cap
          in
            [ here ] <> go tail (remaining - here)

-- Small transverse radius for the shared bonding-pair breathe.
electronCloud :: Number
electronCloud = nucleonRadius

-- Radius of the innermost electron shell: well outside the nucleon cluster (which
-- spans ~nucleusRadius), so the nucleus reads clearly and the electrons visibly
-- ring it instead of sitting inside it.
loneOrbitRadius :: Number
loneOrbitRadius = nucleusRadius * 1.4

-- Radial gap between successive electron shells.
shellSpacing :: Number
shellSpacing = nucleusRadius * 1.0
