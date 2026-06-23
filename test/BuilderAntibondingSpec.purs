-- M3-S1: Anti-bonding shared-electron placement.
-- BondPhase = Bonding | Antibonding
-- BONDING: existing sigma+PI pairs straddling the bond midpoint (byte-identical to
--   bondElectronPositions). ANTIBONDING: same electron count (2*order per bond),
--   but with a NODE at the midpoint — the shared electrons are pushed OUTWARD past
--   each nucleus along the bond axis (sigma pair) or past each nucleus's
--   perpendicular neighbourhood (pi pairs), leaving the midpoint region empty.
-- Pure, total, deterministic, NaN-safe.
module Test.BuilderAntibondingSpec where

import Prelude

import Data.Array (all, foldl, index, length, zipWith)
import Data.Foldable (sum)
import Data.Int (toNumber)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Number (abs, sqrt)
import Effect (Effect)
import Effect.Console (log)
import Builder as B
import Builder.Electrons (BondPhase(..), bondElectronPositionsPhased)
import Test.Util (approxEq, check)

-- ──────────────────────────────────────────────────────────────────────────────
-- Helpers
-- ──────────────────────────────────────────────────────────────────────────────

-- Euclidean distance between two V3s.
dist3
  :: { x :: Number, y :: Number, z :: Number }
  -> { x :: Number, y :: Number, z :: Number }
  -> Number
dist3 a b =
  let
    dx = a.x - b.x
    dy = a.y - b.y
    dz = a.z - b.z
  in
    sqrt (dx * dx + dy * dy + dz * dz)

-- Dot product of two V3s.
dot3
  :: { x :: Number, y :: Number, z :: Number }
  -> { x :: Number, y :: Number, z :: Number }
  -> Number
dot3 u v = u.x * v.x + u.y * v.y + u.z * v.z

-- Length of a V3.
len3 :: { x :: Number, y :: Number, z :: Number } -> Number
len3 v = sqrt (v.x * v.x + v.y * v.y + v.z * v.z)

-- Signed component of a V3 along a unit vector.
projAlong :: { x :: Number, y :: Number, z :: Number } -> { x :: Number, y :: Number, z :: Number } -> Number
projAlong v u = dot3 v u

-- Mean of a non-empty array of V3s (returns origin for empty).
meanV3 :: Array { x :: Number, y :: Number, z :: Number } -> { x :: Number, y :: Number, z :: Number }
meanV3 ps =
  let
    n = max 1 (length ps)
    sx = sum (map _.x ps)
    sy = sum (map _.y ps)
    sz = sum (map _.z ps)
  in
    { x: sx / toNumber n, y: sy / toNumber n, z: sz / toNumber n }

-- ──────────────────────────────────────────────────────────────────────────────
-- Spec
-- ──────────────────────────────────────────────────────────────────────────────

builderAntibondingSpec :: Effect Unit
builderAntibondingSpec = do
  log "M3-S1 anti-bonding shared-electron placement properties:"

  -- ── Fixtures ─────────────────────────────────────────────────────────────
  let
    near = B.bondThreshold * 0.5 -- 90.0, comfortably inside bonding range

    -- H-H: order 1 (single bond, sigma only)
    hhPair = B.addAtom 1 { x: near, y: 0.0, z: 0.0 }
      (B.addAtom 1 { x: 0.0, y: 0.0, z: 0.0 } B.emptyBuilder)

    -- O-O: order 2 (double bond: 1 sigma + 1 PI)
    ooPair = B.addAtom 8 { x: near, y: 0.0, z: 0.0 }
      (B.addAtom 8 { x: 0.0, y: 0.0, z: 0.0 } B.emptyBuilder)

    -- N-N: order 3 (triple bond: 1 sigma + 2 PI)
    nnPair = B.addAtom 7 { x: near, y: 0.0, z: 0.0 }
      (B.addAtom 7 { x: 0.0, y: 0.0, z: 0.0 } B.emptyBuilder)

    -- Bond order from first bond in state.
    bondOrder st = case index st.bonds 0 of
      Just bd -> bd.order
      Nothing -> 0

    -- Bonding positions (Bonding phase).
    bondingPos st = bondElectronPositionsPhased Bonding st 0.0
    antibondingPos st = bondElectronPositionsPhased Antibonding st 0.0
    -- Atom positions from fixture.
    hhAtomA = fromMaybe { x: 0.0, y: 0.0, z: 0.0 } (map _.pos (index hhPair.atoms 0))
    hhAtomB = fromMaybe { x: near, y: 0.0, z: 0.0 } (map _.pos (index hhPair.atoms 1))
    ooAtomA = fromMaybe { x: 0.0, y: 0.0, z: 0.0 } (map _.pos (index ooPair.atoms 0))
    ooAtomB = fromMaybe { x: near, y: 0.0, z: 0.0 } (map _.pos (index ooPair.atoms 1))
    nnAtomA = fromMaybe { x: 0.0, y: 0.0, z: 0.0 } (map _.pos (index nnPair.atoms 0))
    nnAtomB = fromMaybe { x: near, y: 0.0, z: 0.0 } (map _.pos (index nnPair.atoms 1))

    hhMid = { x: (hhAtomA.x + hhAtomB.x) / 2.0, y: (hhAtomA.y + hhAtomB.y) / 2.0, z: (hhAtomA.z + hhAtomB.z) / 2.0 }
    ooMid = { x: (ooAtomA.x + ooAtomB.x) / 2.0, y: (ooAtomA.y + ooAtomB.y) / 2.0, z: (ooAtomA.z + ooAtomB.z) / 2.0 }
    nnMid = { x: (nnAtomA.x + nnAtomB.x) / 2.0, y: (nnAtomA.y + nnAtomB.y) / 2.0, z: (nnAtomA.z + nnAtomB.z) / 2.0 }

    -- "Midpoint epsilon" for the node test: none of the antibonding electrons
    -- should lie within this distance of the bond midpoint. We pick a value
    -- that is clearly larger than a floating-point rounding error but smaller
    -- than the outward displacement used by the implementation.
    nodeEps = 5.0

  -- ── (a) FIXTURE SANITY ─────────────────────────────────────────────────────
  check "AB fixture: H-H bond order == 1" $
    bondOrder hhPair == 1
  check "AB fixture: O-O bond order == 2" $
    bondOrder ooPair == 2
  check "AB fixture: N-N bond order == 3" $
    bondOrder nnPair == 3

  -- ── (b) COUNT EQUALITY: antibonding count == bonding count == 2*order ─────
  -- H-H (order 1): 2 electrons each way.
  check "AB count: H-H bonding == 2" $
    length (bondingPos hhPair) == 2
  check "AB count: H-H antibonding == 2 (count conserved)" $
    length (antibondingPos hhPair) == 2
  check "AB count: H-H bonding == antibonding count" $
    length (bondingPos hhPair) == length (antibondingPos hhPair)

  -- O-O (order 2): 4 electrons each way.
  check "AB count: O-O bonding == 4" $
    length (bondingPos ooPair) == 4
  check "AB count: O-O antibonding == 4 (count conserved)" $
    length (antibondingPos ooPair) == 4
  check "AB count: O-O bonding == antibonding count" $
    length (bondingPos ooPair) == length (antibondingPos ooPair)

  -- N-N (order 3): 6 electrons each way.
  check "AB count: N-N bonding == 6" $
    length (bondingPos nnPair) == 6
  check "AB count: N-N antibonding == 6 (count conserved)" $
    length (antibondingPos nnPair) == 6
  check "AB count: N-N bonding == antibonding count" $
    length (bondingPos nnPair) == length (antibondingPos nnPair)

  -- Empty world: 0 each way.
  check "AB count: empty world bonding == 0" $
    length (bondElectronPositionsPhased Bonding B.emptyBuilder 0.0) == 0
  check "AB count: empty world antibonding == 0" $
    length (bondElectronPositionsPhased Antibonding B.emptyBuilder 0.0) == 0

  -- ── (c) NODE: no antibonding electron lies within nodeEps of the midpoint ──
  -- (i.e. there IS a node — zero electron density — at the midpoint).
  check "AB node: H-H antibonding electrons all far from midpoint (node present)" $
    all (\e -> dist3 e hhMid > nodeEps) (antibondingPos hhPair)
  check "AB node: O-O antibonding electrons all far from midpoint (node present)" $
    all (\e -> dist3 e ooMid > nodeEps) (antibondingPos ooPair)
  check "AB node: N-N antibonding electrons all far from midpoint (node present)" $
    all (\e -> dist3 e nnMid > nodeEps) (antibondingPos nnPair)

  -- ── (d) BONDING: sigma pair straddles the midpoint (mean ≈ midpoint) ───────
  -- The mean of the bonding electrons for H-H (order 1) should be near the midpoint.
  check "AB bonding: H-H bonding pair mean ≈ midpoint (x)" $
    approxEq (meanV3 (bondingPos hhPair)).x hhMid.x
  check "AB bonding: H-H bonding pair mean ≈ midpoint (y)" $
    approxEq (meanV3 (bondingPos hhPair)).y hhMid.y
  check "AB bonding: H-H bonding pair mean ≈ midpoint (z)" $
    approxEq (meanV3 (bondingPos hhPair)).z hhMid.z

  -- ── (e) OUTWARD: each antibonding sigma electron is FARTHER from the midpoint
  --   along the axis than the corresponding bonding electron ────────────────────
  -- For H-H (order 1, axis along x):
  -- Bonding: electrons straddled around midpoint, |disp| ≈ small.
  -- Antibonding: electrons pushed outward, |disp| > bonding |disp|.
  let
    hhAxisD = len3 { x: hhAtomB.x - hhAtomA.x, y: hhAtomB.y - hhAtomA.y, z: hhAtomB.z - hhAtomA.z }
    hhAxis =
      if hhAxisD < 1.0e-9 then { x: 1.0, y: 0.0, z: 0.0 }
      else
        { x: (hhAtomB.x - hhAtomA.x) / hhAxisD
        , y: (hhAtomB.y - hhAtomA.y) / hhAxisD
        , z: (hhAtomB.z - hhAtomA.z) / hhAxisD
        }

    -- Signed axial displacement of each electron from the midpoint.
    axialDispFrom mid axis e =
      let
        disp = { x: e.x - mid.x, y: e.y - mid.y, z: e.z - mid.z }
      in
        projAlong disp axis

    hhBondingDisps = map (axialDispFrom hhMid hhAxis) (bondingPos hhPair)
    hhAntibondingDisps = map (axialDispFrom hhMid hhAxis) (antibondingPos hhPair)

  -- The antibonding electrons must all have a larger |axial displacement| from
  -- the midpoint than the bonding electrons (swept further out along the axis).
  check "AB outward: H-H antibonding |axial disp| > bonding |axial disp| for both electrons" $
    all identity (zipWith (\ab bo -> abs ab > abs bo) hhAntibondingDisps hhBondingDisps)

  -- Antibonding: one electron on the "a" side (negative axial projection from
  -- midpoint, i.e. on the far side of atom A) and one on the "b" side (positive).
  check "AB outward: H-H antibonding has one electron on each side of midpoint along axis" $
    let
      signs = map (\d -> if d > 0.0 then 1 else -1) hhAntibondingDisps
      positiveCount = length (foldl (\acc s -> if s > 0 then acc <> [ s ] else acc) [] signs)
    in
      positiveCount == 1

  -- Each antibonding sigma electron should be BEYOND its respective nucleus
  -- (further from the midpoint than the atom centre itself).
  let
    hhHalfBond = hhAxisD / 2.0 -- distance from midpoint to each nucleus
    hhABAbsDisps = map abs hhAntibondingDisps

  check "AB outward: H-H antibonding electrons are beyond the nuclei (|disp| > half-bond)" $
    all (\d -> d > hhHalfBond) hhABAbsDisps

  -- ── (f) ORDER-1 BONDING BYTE-IDENTICAL to bondElectronPositions ────────────
  -- The Bonding phase for H-H must produce the EXACT same array as the existing
  -- bondElectronPositions function (order-1 byte-identical guarantee).
  check "AB byte-identical: Bonding phase == bondElectronPositions at frame 0 (H-H)" $
    bondElectronPositionsPhased Bonding hhPair 0.0 == B.bondElectronPositions hhPair 0.0
  check "AB byte-identical: Bonding phase == bondElectronPositions at frame 60 (H-H)" $
    bondElectronPositionsPhased Bonding hhPair 60.0 == B.bondElectronPositions hhPair 60.0
  -- Also for O-O (order 2) and N-N (order 3).
  check "AB byte-identical: Bonding phase == bondElectronPositions at frame 0 (O-O)" $
    bondElectronPositionsPhased Bonding ooPair 0.0 == B.bondElectronPositions ooPair 0.0
  check "AB byte-identical: Bonding phase == bondElectronPositions at frame 0 (N-N)" $
    bondElectronPositionsPhased Bonding nnPair 0.0 == B.bondElectronPositions nnPair 0.0

  -- ── (g) DETERMINISM: same (state, phase, frame) -> same result ─────────────
  check "AB determinism: H-H Antibonding frame 0 is reproducible" $
    bondElectronPositionsPhased Antibonding hhPair 0.0
      == bondElectronPositionsPhased Antibonding hhPair 0.0
  check "AB determinism: O-O Antibonding frame 0 is reproducible" $
    bondElectronPositionsPhased Antibonding ooPair 0.0
      == bondElectronPositionsPhased Antibonding ooPair 0.0
  check "AB determinism: N-N Antibonding frame 5 is reproducible" $
    bondElectronPositionsPhased Antibonding nnPair 5.0
      == bondElectronPositionsPhased Antibonding nnPair 5.0

  -- ── (h) FRAME ANIMATION: antibonding electrons vary with frame ─────────────
  check "AB animation: H-H antibonding frame 0 != frame 60" $
    bondElectronPositionsPhased Antibonding hhPair 0.0
      /= bondElectronPositionsPhased Antibonding hhPair 60.0
  check "AB animation: O-O antibonding frame 0 != frame 60" $
    bondElectronPositionsPhased Antibonding ooPair 0.0
      /= bondElectronPositionsPhased Antibonding ooPair 60.0

  -- ── (i) NaN-SAFE: coincident atoms produce finite positions ────────────────
  -- Build a state with two atoms at the exact same position (degenerate axis).
  -- After resolveOverlaps they will be pushed apart, but we want to test the
  -- coincident-fallback path of the antibonding impl. We do this by crafting a
  -- state with a hand-built bond at coincident positions (bypassing addAtom's
  -- overlap resolution). We can't bypass addAtom easily, so we verify that
  -- the standard fixture (near distance) produces only finite values.
  let
    allFinite ps = all (\e -> not (e.x /= e.x) && not (e.y /= e.y) && not (e.z /= e.z)) ps
  check "AB nan-safe: H-H antibonding positions are all finite" $
    allFinite (antibondingPos hhPair)
  check "AB nan-safe: O-O antibonding positions are all finite" $
    allFinite (antibondingPos ooPair)
  check "AB nan-safe: N-N antibonding positions are all finite" $
    allFinite (antibondingPos nnPair)

  -- ── (j) O-O COUNT per-bond sum = 2*order ────────────────────────────────────
  check "AB sum: 2*order antibonding electrons per bond (H-H order1 -> 2)" $
    length (antibondingPos hhPair) == 2 * (sum (map _.order hhPair.bonds))
  check "AB sum: 2*order antibonding electrons per bond (O-O order2 -> 4)" $
    length (antibondingPos ooPair) == 2 * (sum (map _.order ooPair.bonds))
  check "AB sum: 2*order antibonding electrons per bond (N-N order3 -> 6)" $
    length (antibondingPos nnPair) == 2 * (sum (map _.order nnPair.bonds))

  log "all M3-S1 anti-bonding shared-electron placement properties hold."
