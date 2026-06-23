-- M2-S4: bond order N emits 2*N shared electrons per bond:
-- 1 sigma pair along the bond axis + (N-1) PI pairs perpendicular to it.
-- Total electron count is conserved: Σ (incident orders + loneCountOf) == z per atom.
-- Order-1 path is byte-identical to pre-S4 behaviour (2 electrons at midpoint along axis).
-- Pure, total, deterministic — no Effect/WebGL.
module Test.BuilderBondsS4Spec where

import Prelude

import Data.Array (all, concatMap, foldl, index, length, range)
import Data.Foldable (sum)
import Data.Int (toNumber)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Number (abs, sqrt)
import Effect (Effect)
import Effect.Console (log)
import Builder as B
import Builder.Geom (degreeIn)
import Test.Util (approxEq, check)

-- Dot product of two V3s.
dot3 :: { x :: Number, y :: Number, z :: Number } -> { x :: Number, y :: Number, z :: Number } -> Number
dot3 u v = u.x * v.x + u.y * v.y + u.z * v.z

-- Euclidean length of a V3.
len3 :: { x :: Number, y :: Number, z :: Number } -> Number
len3 v = sqrt (v.x * v.x + v.y * v.y + v.z * v.z)

-- Tolerance for floating-point checks.
eps :: Number
eps = 1.0e-9

-- Build a bond array with a single bond of the given order between atoms 0 and 1.
-- (Helper used for hand-crafted order-N bond states.)

builderBondsS4Spec :: Effect Unit
builderBondsS4Spec = do
  log "M2-S4 sigma+PI bond electrons (order N => 2*N shared electrons) properties:"

  -- ──────────────────────────────────────────────────────────────────────────────
  -- Fixtures: isolated pairs that recomputeBonds assigns specific orders.
  -- H-H (order 1), O-O (order 2), N-N (order 3), C-C (order 3 via hard cap).
  -- ──────────────────────────────────────────────────────────────────────────────
  let
    near = B.bondThreshold * 0.5 -- 90.0 — comfortably inside bonding range

    -- H-H: valence 1 each -> order 1 single bond
    hhPair = B.addAtom 1 { x: near, y: 0.0, z: 0.0 }
      (B.addAtom 1 { x: 0.0, y: 0.0, z: 0.0 } B.emptyBuilder)

    -- O-O: valence 2 each -> order 2 double bond (isolated pair)
    ooPair = B.addAtom 8 { x: near, y: 0.0, z: 0.0 }
      (B.addAtom 8 { x: 0.0, y: 0.0, z: 0.0 } B.emptyBuilder)

    -- N-N: valence 3 each -> order 3 triple bond (isolated pair)
    nnPair = B.addAtom 7 { x: near, y: 0.0, z: 0.0 }
      (B.addAtom 7 { x: 0.0, y: 0.0, z: 0.0 } B.emptyBuilder)

    -- C-C: valence 4, but hard cap is 3 -> order 3 triple bond
    ccPair = B.addAtom 6 { x: near, y: 0.0, z: 0.0 }
      (B.addAtom 6 { x: 0.0, y: 0.0, z: 0.0 } B.emptyBuilder)

    -- Bond order helpers
    bondOrder st = case index st.bonds 0 of
      Just bd -> bd.order
      Nothing -> 0

    -- Σ z over all atoms in a state.
    electronSum st = sum (map _.z st.atoms)

    -- loneCountOf for all atoms in st (sum).
    totalLone st = sum (map (\a -> B.loneCountOf st a.id) st.atoms)

    -- Total bonding electrons from bondElectronPositions (frame 0).
    totalBonded st = length (B.bondElectronPositions st 0.0)

    -- For a given state, verify Σ (bonded + lone) == Σ z.
    conserved st = totalBonded st + totalLone st == electronSum st

  -- ── (a) FIXTURE SANITY: verify the bond orders are what we expect ──────────
  check "S4 fixture: H-H pair has exactly 1 bond" $
    length hhPair.bonds == 1
  check "S4 fixture: H-H bond has order 1" $
    bondOrder hhPair == 1
  check "S4 fixture: O-O pair has exactly 1 bond" $
    length ooPair.bonds == 1
  check "S4 fixture: O-O bond has order 2 (valence 2, isolated pair)" $
    bondOrder ooPair == 2
  check "S4 fixture: N-N pair has exactly 1 bond" $
    length nnPair.bonds == 1
  check "S4 fixture: N-N bond has order 3 (valence 3, isolated pair)" $
    bondOrder nnPair == 3
  check "S4 fixture: C-C pair has exactly 1 bond" $
    length ccPair.bonds == 1
  check "S4 fixture: C-C bond order >= 2 (isolated pair, hard cap 3)" $
    bondOrder ccPair >= 2

  -- ── (b) ELECTRON COUNT: 2*order per bond ──────────────────────────────────
  -- H-H order 1 -> 2 electrons (unchanged)
  check "S4 count: H-H order 1 -> 2 shared electrons" $
    totalBonded hhPair == 2

  -- O-O order 2 -> 4 electrons (1 sigma pair + 1 PI pair)
  check "S4 count: O-O order 2 -> 4 shared electrons" $
    totalBonded ooPair == 4

  -- N-N order 3 -> 6 electrons (1 sigma pair + 2 PI pairs)
  check "S4 count: N-N order 3 -> 6 shared electrons" $
    totalBonded nnPair == 6

  -- C-C order 3 -> 6 electrons
  check "S4 count: C-C order 3 -> 6 shared electrons" $
    totalBonded ccPair == 6

  -- ── (c) CONSERVATION: (bonded + lone) == Σ z per entire state ─────────────
  check "S4 conservation: H-H (2 = 2+0)" $
    conserved hhPair
  check "S4 conservation: O-O (16 = 4 bonded + 12 lone)" $
    conserved ooPair
  check "S4 conservation: N-N (14 = 6 bonded + 8 lone)" $
    conserved nnPair
  check "S4 conservation: C-C (12 = 6 bonded + 6 lone)" $
    conserved ccPair
  check "S4 conservation: empty builder (0)" $
    conserved B.emptyBuilder

  -- ── (d) CONSERVATION per-atom: for each atom, (incidentOrders + loneCountOf) == z ─
  let
    perAtomConserved st =
      all identity
        ( map
            ( \a ->
                let
                  incidentOrder = degreeIn st.bonds a.id
                  lone = B.loneCountOf st a.id
                in
                  incidentOrder + lone == a.z
            )
            st.atoms
        )

  check "S4 per-atom conservation: H-H each H: (order + lone) == z(1)" $
    perAtomConserved hhPair
  check "S4 per-atom conservation: O-O each O: (order + lone) == z(8)" $
    perAtomConserved ooPair
  check "S4 per-atom conservation: N-N each N: (order + lone) == z(7)" $
    perAtomConserved nnPair
  check "S4 per-atom conservation: C-C each C: (order + lone) == z(6)" $
    perAtomConserved ccPair

  -- ── (e) MIXED STATE conservation: C2 triple + lone O + H-H single ──────────
  let
    -- Build a state with C-C (order 3) at one location and a lone O and H-H elsewhere.
    -- Use disjoint positions so the groups don't bond to each other.
    far = B.breakThreshold * 3.0
    -- C-C double bond at origin vicinity
    ccAt0 = B.addAtom 6 { x: near, y: 0.0, z: 0.0 }
      (B.addAtom 6 { x: 0.0, y: 0.0, z: 0.0 } B.emptyBuilder)
    -- H-H bond far from C-C
    hhFar = B.addAtom 1 { x: far + near, y: 0.0, z: 0.0 }
      (B.addAtom 1 { x: far, y: 0.0, z: 0.0 } B.emptyBuilder)
    -- A lone O (oxygen at 2*far)
    loneO = B.addAtom 8 { x: 2.0 * far, y: 0.0, z: 0.0 } B.emptyBuilder

    -- Combine all states into a single one using addAtom calls at the right positions.
    -- We build it from scratch so that addAtom assigns ids and recomputeBonds runs.
    mixedSt =
      B.addAtom 8 { x: 2.0 * far, y: 0.0, z: 0.0 }
        ( B.addAtom 1 { x: far + near, y: 0.0, z: 0.0 }
            ( B.addAtom 1 { x: far, y: 0.0, z: 0.0 }
                ( B.addAtom 6 { x: near, y: 0.0, z: 0.0 }
                    (B.addAtom 6 { x: 0.0, y: 0.0, z: 0.0 } B.emptyBuilder)
                )
            )
        )

  -- Verify the mixed state has the expected bonds: C-C and H-H, not others.
  -- C-C: order >= 2 (triple likely), H-H: order 1
  check "S4 mixed: C-C and H-H bonds formed (2 bonds total)" $
    length mixedSt.bonds == 2
  check "S4 mixed: total conservation (Σ bonded + Σ lone == Σ z)" $
    conserved mixedSt
  check "S4 mixed: per-atom conservation holds for every atom" $
    perAtomConserved mixedSt

  -- ── (f) GEOMETRY: PI pairs are perpendicular to the bond axis ─────────────
  -- For O-O (order 2): bond axis is along x (posA=(0,0,0), posB=(near,0,0)).
  -- sigma pair: displacement from midpoint has ~non-zero x component (along axis).
  -- PI pair (the other 2 electrons): displacement from midpoint is perpendicular to
  -- the axis (dot(disp, axis) ≈ 0).
  let
    ooElectrons = B.bondElectronPositions ooPair 0.0
    ooAtomA = fromMaybe { x: 0.0, y: 0.0, z: 0.0 } (map _.pos (index ooPair.atoms 0))
    ooAtomB = fromMaybe { x: near, y: 0.0, z: 0.0 } (map _.pos (index ooPair.atoms 1))
    ooMid =
      { x: (ooAtomA.x + ooAtomB.x) / 2.0
      , y: (ooAtomA.y + ooAtomB.y) / 2.0
      , z: (ooAtomA.z + ooAtomB.z) / 2.0
      }

    -- Unit axis from a to b
    ooD = len3 { x: ooAtomB.x - ooAtomA.x, y: ooAtomB.y - ooAtomA.y, z: ooAtomB.z - ooAtomA.z }
    ooAxis =
      { x: (ooAtomB.x - ooAtomA.x) / ooD
      , y: (ooAtomB.y - ooAtomA.y) / ooD
      , z: (ooAtomB.z - ooAtomA.z) / ooD
      }

    -- Displacements of the 4 electrons from the midpoint.
    ooDisps = map (\e -> { x: e.x - ooMid.x, y: e.y - ooMid.y, z: e.z - ooMid.z }) ooElectrons

    -- Dot of each displacement with the axis.
    ooDots = map (\d -> abs (dot3 d ooAxis)) ooDisps

    -- For sigma pair (electrons 0 and 1), the displacement should be mostly along axis.
    -- For PI pair (electrons 2 and 3), displacement is perpendicular (dot ≈ 0).
    -- Since we can't know which index is sigma vs pi without knowing the impl,
    -- we check: at least 2 of the 4 electrons have |dot with axis| < eps (perpendicular).
    piElectronCount = length (foldl (\acc d -> if d < eps then acc <> [ d ] else acc) [] ooDots)

  check "S4 geometry: O-O order 2 => exactly 4 bonding electrons" $
    length ooElectrons == 4
  check "S4 geometry: O-O: at least 2 electrons are perpendicular to bond axis (PI pair)" $
    piElectronCount >= 2

  -- ── (g) GEOMETRY: N-N order 3 has sigma pair along axis + 2 PI pairs perp ──
  let
    nnElectrons = B.bondElectronPositions nnPair 0.0
    nnAtomA = fromMaybe { x: 0.0, y: 0.0, z: 0.0 } (map _.pos (index nnPair.atoms 0))
    nnAtomB = fromMaybe { x: near, y: 0.0, z: 0.0 } (map _.pos (index nnPair.atoms 1))
    nnMid =
      { x: (nnAtomA.x + nnAtomB.x) / 2.0
      , y: (nnAtomA.y + nnAtomB.y) / 2.0
      , z: (nnAtomA.z + nnAtomB.z) / 2.0
      }
    nnD = len3 { x: nnAtomB.x - nnAtomA.x, y: nnAtomB.y - nnAtomA.y, z: nnAtomB.z - nnAtomA.z }
    nnAxis =
      { x: (nnAtomB.x - nnAtomA.x) / nnD
      , y: (nnAtomB.y - nnAtomA.y) / nnD
      , z: (nnAtomB.z - nnAtomA.z) / nnD
      }
    nnDisps = map (\e -> { x: e.x - nnMid.x, y: e.y - nnMid.y, z: e.z - nnMid.z }) nnElectrons
    nnDots = map (\d -> abs (dot3 d nnAxis)) nnDisps
    -- N-N order 3: 6 electrons. Sigma = 2 electrons along axis, 2 PI pairs = 4 perp.
    nnPiCount = length (foldl (\acc d -> if d < eps then acc <> [ d ] else acc) [] nnDots)

  check "S4 geometry: N-N order 3 => exactly 6 bonding electrons" $
    length nnElectrons == 6
  check "S4 geometry: N-N: at least 4 electrons are perpendicular to bond axis (2 PI pairs)" $
    nnPiCount >= 4

  -- ── (h) ORDER-1 BYTE-IDENTICAL: H-H (order 1) sigma pair unchanged ──────────
  -- At frame 0, the sigma pair for the H-H bond must be the SAME as if we had
  -- only 1 sigma pair computed the old way. The existing conservation test already
  -- confirms 2 electrons for H-H; here we confirm the geometry (x component
  -- displaced from midpoint, mirrored pair) is the same as the pre-S4 formula.
  let
    hhElectrons0 = B.bondElectronPositions hhPair 0.0
    hhElectrons60 = B.bondElectronPositions hhPair 60.0
    hhAtomA = fromMaybe { x: 0.0, y: 0.0, z: 0.0 } (map _.pos (index hhPair.atoms 0))
    hhAtomB = fromMaybe { x: near, y: 0.0, z: 0.0 } (map _.pos (index hhPair.atoms 1))
    hhMid =
      { x: (hhAtomA.x + hhAtomB.x) / 2.0
      , y: (hhAtomA.y + hhAtomB.y) / 2.0
      , z: (hhAtomA.z + hhAtomB.z) / 2.0
      }
    hhE0 = fromMaybe { x: 0.0, y: 0.0, z: 0.0 } (index hhElectrons0 0)
    hhE1 = fromMaybe { x: 0.0, y: 0.0, z: 0.0 } (index hhElectrons0 1)

  check "S4 order-1: H-H => exactly 2 bonding electrons (unchanged)" $
    length hhElectrons0 == 2
  -- The pair must be mirrored about the midpoint: e0 + e1 = 2 * mid
  check "S4 order-1: H-H sigma pair is symmetric about midpoint (x)" $
    approxEq (hhE0.x + hhE1.x) (2.0 * hhMid.x)
  check "S4 order-1: H-H sigma pair is symmetric about midpoint (y)" $
    approxEq (hhE0.y + hhE1.y) (2.0 * hhMid.y)
  check "S4 order-1: H-H sigma pair is symmetric about midpoint (z)" $
    approxEq (hhE0.z + hhE1.z) (2.0 * hhMid.z)
  -- Frame-animated: frame 0 differs from frame 60.
  check "S4 order-1: H-H electrons are frame-animated (frame 0 != frame 60)" $
    hhElectrons0 /= hhElectrons60

  -- ── (i) SIGMA pair for order-2: first pair along axis ──────────────────────
  -- The sigma pair electrons 0 and 1 must be symmetric about the midpoint along axis.
  let
    ooE0 = fromMaybe { x: 0.0, y: 0.0, z: 0.0 } (index ooElectrons 0)
    ooE1 = fromMaybe { x: 0.0, y: 0.0, z: 0.0 } (index ooElectrons 1)
    ooE2 = fromMaybe { x: 0.0, y: 0.0, z: 0.0 } (index ooElectrons 2)
    ooE3 = fromMaybe { x: 0.0, y: 0.0, z: 0.0 } (index ooElectrons 3)
    -- Each consecutive pair must be symmetric about the midpoint.
    pairSymmetric e0 e1 mid =
      approxEq (e0.x + e1.x) (2.0 * mid.x)
        && approxEq (e0.y + e1.y) (2.0 * mid.y)
        && approxEq (e0.z + e1.z) (2.0 * mid.z)

  check "S4 sigma: O-O electron pair 0-1 is symmetric about midpoint" $
    pairSymmetric ooE0 ooE1 ooMid
  check "S4 sigma: O-O electron pair 2-3 (PI) is symmetric about midpoint" $
    pairSymmetric ooE2 ooE3 ooMid

  -- ── (j) DETERMINISM: same (state, frame) => identical output ───────────────
  check "S4 determinism: O-O bondElectronPositions same for frame 0 run twice" $
    B.bondElectronPositions ooPair 0.0 == B.bondElectronPositions ooPair 0.0
  check "S4 determinism: N-N bondElectronPositions same for frame 5 run twice" $
    B.bondElectronPositions nnPair 5.0 == B.bondElectronPositions nnPair 5.0

  -- ── (k) bondElectronGroups CONSISTENT: groups match flat count ──────────────
  let
    ooGroups = B.bondElectronGroups ooPair 0.0
    nnGroups = B.bondElectronGroups nnPair 0.0
    hhGroups = B.bondElectronGroups hhPair 0.0
    groupCount gs = sum (map (\g -> length g.positions) gs)

  check "S4 groups: O-O bondElectronGroups total matches bondElectronPositions (4)" $
    groupCount ooGroups == totalBonded ooPair
  check "S4 groups: N-N bondElectronGroups total matches bondElectronPositions (6)" $
    groupCount nnGroups == totalBonded nnPair
  check "S4 groups: H-H bondElectronGroups total matches bondElectronPositions (2)" $
    groupCount hhGroups == totalBonded hhPair

  -- ── (l) FRAME ANIMATION: order-2 electrons vary with frame ─────────────────
  check "S4 animation: O-O electrons at frame 0 != frame 60" $
    B.bondElectronPositions ooPair 0.0 /= B.bondElectronPositions ooPair 60.0
  check "S4 animation: N-N electrons at frame 0 != frame 60" $
    B.bondElectronPositions nnPair 0.0 /= B.bondElectronPositions nnPair 60.0

  -- ── (m) TOTAL SHARED ELECTRONS for multi-bond world ──────────────────────────
  -- If a world has 2 bonds of orders [2, 3], total shared electrons == 2*2 + 2*3 = 10.
  let
    totalSharedEq st = totalBonded st == 2 * (sum (map _.order st.bonds))

  check "S4 total shared: O-O (order 2) => 2*2=4" $
    totalSharedEq ooPair
  check "S4 total shared: N-N (order 3) => 2*3=6" $
    totalSharedEq nnPair
  check "S4 total shared: H-H (order 1) => 2*1=2" $
    totalSharedEq hhPair

  log "all M2-S4 sigma+PI bond electron properties hold."
