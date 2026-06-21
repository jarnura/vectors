module Test.BuilderOverlapSpec where

import Prelude

import Data.Array (all, index, length, range)
import Data.Maybe (fromMaybe)
import Data.Number (abs, sqrt)
import Effect (Effect)
import Effect.Console (log)
import Atom (atomicRadius) as Atom
import Builder as B
import Builder.Overlap (minSeparation) as Overlap
import Pe as Pe
import Test.Util (approxEq, check)

builderOverlapSpec :: Effect Unit
builderOverlapSpec = do
  -- ───── M1: minSeparation + resolveOverlaps (constraint model) ────────
  log "M1 constraint model (minSeparation + resolveOverlaps) properties:"

  let
    -- ── helpers ──────────────────────────────────────────────────────────
    -- Euclidean distance between two V3 points.
    dist3 a b =
      let
        ddx = a.x - b.x
        ddy = a.y - b.y
        ddz = a.z - b.z
      in
        sqrt (ddx * ddx + ddy * ddy + ddz * ddz)

    -- Distance between two placed atoms in a BuilderState (by array index).
    pairDist st i j =
      let
        pi_ = fromMaybe { id: -1, z: 1, pos: { x: 0.0, y: 0.0, z: 0.0 } } (index st.atoms i)
        pj_ = fromMaybe { id: -1, z: 1, pos: { x: 0.0, y: 0.0, z: 0.0 } } (index st.atoms j)
      in
        dist3 pi_.pos pj_.pos

    -- The geometry tolerance used for solver assertions (1e-6).
    geoTol :: Number
    geoTol = 1.0e-6

    -- The exact-position tolerance (1e-10 for unchanged atoms).
    exactTol :: Number
    exactTol = 1.0e-10

  -- (a) FLOOR SYMMETRY: minSeparation z1 z2 == minSeparation z2 z1 for ALL z1,z2 in 1..36.
  check "minSeparation is symmetric for all element pairs (1..36)" $
    all identity do
      z1 <- range 1 36
      z2 <- range 1 36
      pure (B.minSeparation z1 z2 == B.minSeparation z2 z1)

  -- (b) WORST-CASE FLOOR: absoluteMin(130) <= minSeparation z1 z2 < bondThreshold for all pairs.
  --     AND minSeparation 19 19 (K+K) == 165.0 (the clamped ceiling).
  check "minSeparation is always >= absoluteMin (130) for all element pairs" $
    all identity do
      z1 <- range 1 36
      z2 <- range 1 36
      pure (B.minSeparation z1 z2 >= 130.0)

  check "minSeparation is always < bondThreshold (180) for all element pairs" $
    all identity do
      z1 <- range 1 36
      z2 <- range 1 36
      pure (B.minSeparation z1 z2 < B.bondThreshold)

  check "minSeparation 19 19 (K+K) == 165.0 (clamped ceiling)" $
    approxEq (B.minSeparation 19 19) 165.0

  -- (c) PAIR AT DISTANCE 0 AND PAIR AT SUB-FLOOR DISTANCE → resolveOverlaps separates them.
  let
    -- Two H atoms at the exact same position (coincident).
    coincidentA = B.addAtom 1 { x: 0.0, y: 0.0, z: 0.0 } B.emptyBuilder
    coincidentB = B.addAtom 1 { x: 0.0, y: 0.0, z: 0.0 } coincidentA
    resolvedCoincident = B.resolveOverlaps [] coincidentB
    floorHH = B.minSeparation 1 1

    -- Two H atoms at distance 50 (well below the floor).
    subFloorA = B.addAtom 1 { x: 0.0, y: 0.0, z: 0.0 } B.emptyBuilder
    subFloorB = B.addAtom 1 { x: 50.0, y: 0.0, z: 0.0 } subFloorA
    resolvedSubFloor = B.resolveOverlaps [] subFloorB
    resolvedSubFloorDist = pairDist resolvedSubFloor 0 1

  check "resolveOverlaps: pair at d=0 (coincident) → distance >= floor - 1e-6" $
    pairDist resolvedCoincident 0 1 >= floorHH - geoTol

  check "resolveOverlaps: pair at d=50 (sub-floor) → distance >= floor - 1e-6" $
    resolvedSubFloorDist >= floorHH - geoTol

  -- (d) THREE MUTUALLY OVERLAPPING ATOMS → every pair >= floor after resolution.
  let
    triA = B.addAtom 1 { x: 0.0, y: 0.0, z: 0.0 } B.emptyBuilder
    triB = B.addAtom 6 { x: 10.0, y: 0.0, z: 0.0 } triA
    triC = B.addAtom 8 { x: 0.0, y: 10.0, z: 0.0 } triB
    resolvedTri = B.resolveOverlaps [] triC
    triAtom i = fromMaybe { id: -1, z: 1, pos: { x: 0.0, y: 0.0, z: 0.0 } } (index resolvedTri.atoms i)
    triZ i = (triAtom i).z
    triPos i = (triAtom i).pos
    triPairFloor i j = B.minSeparation (triZ i) (triZ j)
    triPairDist i j = dist3 (triPos i) (triPos j)

  check "resolveOverlaps: triple overlap → pair 0-1 >= floor - 1e-6" $
    triPairDist 0 1 >= triPairFloor 0 1 - geoTol
  check "resolveOverlaps: triple overlap → pair 0-2 >= floor - 1e-6" $
    triPairDist 0 2 >= triPairFloor 0 2 - geoTol
  check "resolveOverlaps: triple overlap → pair 1-2 >= floor - 1e-6" $
    triPairDist 1 2 >= triPairFloor 1 2 - geoTol

  -- (e) ANCHOR IMMOBILITY: overlapping pair, one anchor → anchor unchanged (1e-10),
  --     partner ends up >= floor.
  let
    anchorSt = B.addAtom 1 { x: 0.0, y: 0.0, z: 0.0 } B.emptyBuilder
    anchorSt2 = B.addAtom 1 { x: 40.0, y: 0.0, z: 0.0 } anchorSt
    anchorId = fromMaybe (-1) (map _.id (index anchorSt2.atoms 0))
    partnerId = fromMaybe (-1) (map _.id (index anchorSt2.atoms 1))
    resolvedAnchor = B.resolveOverlaps [ anchorId ] anchorSt2
    anchorOrigPos = fromMaybe { x: 0.0, y: 0.0, z: 0.0 } (map _.pos (B.atomById anchorSt2 anchorId))
    anchorNewPos = fromMaybe { x: 999.0, y: 999.0, z: 999.0 } (map _.pos (B.atomById resolvedAnchor anchorId))
    partnerNewPos = fromMaybe { x: 999.0, y: 999.0, z: 999.0 } (map _.pos (B.atomById resolvedAnchor partnerId))

  check "resolveOverlaps anchor immobility: anchor x unchanged (1e-10)" $
    abs (anchorNewPos.x - anchorOrigPos.x) < exactTol
  check "resolveOverlaps anchor immobility: anchor y unchanged (1e-10)" $
    abs (anchorNewPos.y - anchorOrigPos.y) < exactTol
  check "resolveOverlaps anchor immobility: anchor z unchanged (1e-10)" $
    abs (anchorNewPos.z - anchorOrigPos.z) < exactTol
  check "resolveOverlaps anchor immobility: partner ends up >= floor - 1e-6" $
    dist3 anchorNewPos partnerNewPos >= B.minSeparation 1 1 - geoTol

  -- (f) BOTH-ANCHOR SKIP: overlapping pair, both anchored → BOTH positions unchanged.
  let
    bothAnchorSt = B.addAtom 1 { x: 0.0, y: 0.0, z: 0.0 } B.emptyBuilder
    bothAnchorSt2 = B.addAtom 8 { x: 30.0, y: 0.0, z: 0.0 } bothAnchorSt
    bothIdA = fromMaybe (-1) (map _.id (index bothAnchorSt2.atoms 0))
    bothIdB = fromMaybe (-1) (map _.id (index bothAnchorSt2.atoms 1))
    resolvedBoth = B.resolveOverlaps [ bothIdA, bothIdB ] bothAnchorSt2
    bothPosA0 = fromMaybe { x: 0.0, y: 0.0, z: 0.0 } (map _.pos (B.atomById bothAnchorSt2 bothIdA))
    bothPosA1 = fromMaybe { x: 0.0, y: 0.0, z: 0.0 } (map _.pos (B.atomById resolvedBoth bothIdA))
    bothPosB0 = fromMaybe { x: 0.0, y: 0.0, z: 0.0 } (map _.pos (B.atomById bothAnchorSt2 bothIdB))
    bothPosB1 = fromMaybe { x: 0.0, y: 0.0, z: 0.0 } (map _.pos (B.atomById resolvedBoth bothIdB))

  check "resolveOverlaps both-anchor: atom A x unchanged (1e-10)" $
    abs (bothPosA1.x - bothPosA0.x) < exactTol
  check "resolveOverlaps both-anchor: atom A y unchanged (1e-10)" $
    abs (bothPosA1.y - bothPosA0.y) < exactTol
  check "resolveOverlaps both-anchor: atom A z unchanged (1e-10)" $
    abs (bothPosA1.z - bothPosA0.z) < exactTol
  check "resolveOverlaps both-anchor: atom B x unchanged (1e-10)" $
    abs (bothPosB1.x - bothPosB0.x) < exactTol
  check "resolveOverlaps both-anchor: atom B y unchanged (1e-10)" $
    abs (bothPosB1.y - bothPosB0.y) < exactTol
  check "resolveOverlaps both-anchor: atom B z unchanged (1e-10)" $
    abs (bothPosB1.z - bothPosB0.z) < exactTol

  -- (g) COINCIDENT DETERMINISM: two atoms at identical positions, resolve twice independently
  --     → identical resulting positions and distance >= floor; no NaN.
  let
    coinA = B.addAtom 6 { x: 100.0, y: 100.0, z: 0.0 } B.emptyBuilder
    coinB = B.addAtom 6 { x: 100.0, y: 100.0, z: 0.0 } coinA
    resolvedCoin1 = B.resolveOverlaps [] coinB
    resolvedCoin2 = B.resolveOverlaps [] coinB
    coin1Pos0 = fromMaybe { x: 999.0, y: 999.0, z: 999.0 } (map _.pos (index resolvedCoin1.atoms 0))
    coin1Pos1 = fromMaybe { x: 999.0, y: 999.0, z: 999.0 } (map _.pos (index resolvedCoin1.atoms 1))
    coin2Pos0 = fromMaybe { x: 999.0, y: 999.0, z: 999.0 } (map _.pos (index resolvedCoin2.atoms 0))
    coin2Pos1 = fromMaybe { x: 999.0, y: 999.0, z: 999.0 } (map _.pos (index resolvedCoin2.atoms 1))

  check "resolveOverlaps coincident determinism: run1 pos0.x == run2 pos0.x (1e-10)" $
    abs (coin1Pos0.x - coin2Pos0.x) < exactTol
  check "resolveOverlaps coincident determinism: run1 pos0.y == run2 pos0.y (1e-10)" $
    abs (coin1Pos0.y - coin2Pos0.y) < exactTol
  check "resolveOverlaps coincident determinism: run1 pos1.x == run2 pos1.x (1e-10)" $
    abs (coin1Pos1.x - coin2Pos1.x) < exactTol
  check "resolveOverlaps coincident determinism: run1 pos1.y == run2 pos1.y (1e-10)" $
    abs (coin1Pos1.y - coin2Pos1.y) < exactTol
  check "resolveOverlaps coincident determinism: resulting distance >= floor - 1e-6" $
    dist3 coin1Pos0 coin1Pos1 >= B.minSeparation 6 6 - geoTol
  -- No NaN: a valid Number equals itself.
  check "resolveOverlaps coincident: pos0.x is not NaN" $
    coin1Pos0.x == coin1Pos0.x
  check "resolveOverlaps coincident: pos0.y is not NaN" $
    coin1Pos0.y == coin1Pos0.y
  check "resolveOverlaps coincident: pos1.x is not NaN" $
    coin1Pos1.x == coin1Pos1.x
  check "resolveOverlaps coincident: pos1.y is not NaN" $
    coin1Pos1.y == coin1Pos1.y

  -- (h) IDEMPOTENCE: a state already satisfying all floors is unchanged by resolveOverlaps.
  let
    farSt = B.addAtom 1 { x: 0.0, y: 0.0, z: 0.0 } B.emptyBuilder
    farSt2 = B.addAtom 8 { x: 200.0, y: 0.0, z: 0.0 } farSt
    resolvedFar = B.resolveOverlaps [] farSt2
    farPos0Before = fromMaybe { x: 999.0, y: 999.0, z: 999.0 } (map _.pos (index farSt2.atoms 0))
    farPos1Before = fromMaybe { x: 999.0, y: 999.0, z: 999.0 } (map _.pos (index farSt2.atoms 1))
    farPos0After = fromMaybe { x: 999.0, y: 999.0, z: 999.0 } (map _.pos (index resolvedFar.atoms 0))
    farPos1After = fromMaybe { x: 999.0, y: 999.0, z: 999.0 } (map _.pos (index resolvedFar.atoms 1))

  check "resolveOverlaps idempotent: well-separated pair pos0.x unchanged (1e-10)" $
    abs (farPos0After.x - farPos0Before.x) < exactTol
  check "resolveOverlaps idempotent: well-separated pair pos0.y unchanged (1e-10)" $
    abs (farPos0After.y - farPos0Before.y) < exactTol
  check "resolveOverlaps idempotent: well-separated pair pos1.x unchanged (1e-10)" $
    abs (farPos1After.x - farPos1Before.x) < exactTol
  check "resolveOverlaps idempotent: well-separated pair pos1.y unchanged (1e-10)" $
    abs (farPos1After.y - farPos1Before.y) < exactTol

  -- (i) VALIDLY-BONDED PAIR UNTOUCHED: pair at distance 150 (floor ~130 <= 150 < 180)
  --     is within the valid contact range → positions unchanged (1e-10).
  let
    validSt = B.addAtom 1 { x: 0.0, y: 0.0, z: 0.0 } B.emptyBuilder
    validSt2 = B.addAtom 1 { x: 150.0, y: 0.0, z: 0.0 } validSt
    resolvedValid = B.resolveOverlaps [] validSt2
    validPos0Before = fromMaybe { x: 999.0, y: 999.0, z: 999.0 } (map _.pos (index validSt2.atoms 0))
    validPos1Before = fromMaybe { x: 999.0, y: 999.0, z: 999.0 } (map _.pos (index validSt2.atoms 1))
    validPos0After = fromMaybe { x: 999.0, y: 999.0, z: 999.0 } (map _.pos (index resolvedValid.atoms 0))
    validPos1After = fromMaybe { x: 999.0, y: 999.0, z: 999.0 } (map _.pos (index resolvedValid.atoms 1))

  check "resolveOverlaps valid-bond: d=150 pair pos0.x unchanged (1e-10)" $
    abs (validPos0After.x - validPos0Before.x) < exactTol
  check "resolveOverlaps valid-bond: d=150 pair pos0.y unchanged (1e-10)" $
    abs (validPos0After.y - validPos0Before.y) < exactTol
  check "resolveOverlaps valid-bond: d=150 pair pos1.x unchanged (1e-10)" $
    abs (validPos1After.x - validPos1Before.x) < exactTol
  check "resolveOverlaps valid-bond: d=150 pair pos1.y unchanged (1e-10)" $
    abs (validPos1After.y - validPos1Before.y) < exactTol

  -- (j) TERMINATION SMOKE: 4-atom cluster all within a 30-unit blob → resolveOverlaps
  --     terminates and every pair ends up >= floor - 1e-6 (10 passes sufficient for 4 atoms).
  let
    clust0 = B.addAtom 1 { x: 0.0, y: 0.0, z: 0.0 } B.emptyBuilder
    clust1 = B.addAtom 8 { x: 10.0, y: 5.0, z: 0.0 } clust0
    clust2 = B.addAtom 6 { x: -8.0, y: 3.0, z: 0.0 } clust1
    clust3 = B.addAtom 1 { x: 2.0, y: -7.0, z: 0.0 } clust2
    resolvedClust = B.resolveOverlaps [] clust3
    clustAtom i = fromMaybe { id: -1, z: 1, pos: { x: 0.0, y: 0.0, z: 0.0 } } (index resolvedClust.atoms i)
    clustZ i = (clustAtom i).z
    clustPos i = (clustAtom i).pos
    clustPairOk i j =
      dist3 (clustPos i) (clustPos j) >= B.minSeparation (clustZ i) (clustZ j) - geoTol
    clustPairs = [ { i: 0, j: 1 }, { i: 0, j: 2 }, { i: 0, j: 3 }, { i: 1, j: 2 }, { i: 1, j: 3 }, { i: 2, j: 3 } ]

  check "resolveOverlaps termination: 4-atom cluster completes (smoke)" $
    length resolvedClust.atoms == 4
  check "resolveOverlaps termination: 4-atom cluster every pair >= floor - 1e-6" $
    all (\p -> clustPairOk p.i p.j) clustPairs

  -- Also verify 8-atom cluster terminates (no assertion on geometry — termination only).
  let
    c8_0 = B.addAtom 1 { x: 0.0, y: 0.0, z: 0.0 } B.emptyBuilder
    c8_1 = B.addAtom 6 { x: 5.0, y: 0.0, z: 0.0 } c8_0
    c8_2 = B.addAtom 8 { x: 0.0, y: 5.0, z: 0.0 } c8_1
    c8_3 = B.addAtom 1 { x: -5.0, y: 0.0, z: 0.0 } c8_2
    c8_4 = B.addAtom 7 { x: 0.0, y: -5.0, z: 0.0 } c8_3
    c8_5 = B.addAtom 6 { x: 3.0, y: 3.0, z: 0.0 } c8_4
    c8_6 = B.addAtom 1 { x: -3.0, y: 3.0, z: 0.0 } c8_5
    c8_7 = B.addAtom 8 { x: 2.0, y: -3.0, z: 0.0 } c8_6
    resolved8 = B.resolveOverlaps [] c8_7

  check "resolveOverlaps termination: 8-atom cluster returns (smoke, count unchanged)" $
    length resolved8.atoms == 8

  log "all M1 constraint model properties hold."

  -- ───── M2: op-wiring (addAtom / moveAtom / moveMolecule invoke resolveOverlaps) ──
  log "M2 op-wiring (resolveOverlaps in addAtom/moveAtom/moveMolecule) properties:"

  let
    -- Shared geometry tolerance for solver tests (1e-6).
    m2GeoTol :: Number
    m2GeoTol = 1.0e-6

    -- Exact-position tolerance (1e-10): for positions that must be unchanged.
    m2ExactTol :: Number
    m2ExactTol = 1.0e-10

    -- Euclidean distance between two V3 points (local helper).
    m2Dist a b =
      let
        ddx = a.x - b.x
        ddy = a.y - b.y
        ddz = a.z - b.z
      in
        sqrt (ddx * ddx + ddy * ddy + ddz * ddz)

    -- Retrieve a placed-atom position by id from a BuilderState.
    m2PosById st aid_ = map _.pos (B.atomById st aid_)

    -- ── (a) add-at-occupied ───────────────────────────────────────────────
    -- Place C at the origin, then add H ON TOP of C (same pos). The OLD atom
    -- (C, the anchor) must not move (1e-10). The new H is pushed so the pair
    -- distance >= minSeparation C H - m2GeoTol.
    aBaseC = B.addAtom 6 { x: 0.0, y: 0.0, z: 0.0 } B.emptyBuilder
    aCId = fromMaybe (-1) (map _.id (index aBaseC.atoms 0))
    aOccupied = B.addAtom 1 { x: 0.0, y: 0.0, z: 0.0 } aBaseC
    aNewHId = fromMaybe (-1) (map _.id (index aOccupied.atoms 1))
    aCPosAfter = fromMaybe { x: 999.0, y: 999.0, z: 999.0 } (m2PosById aOccupied aCId)
    aNewHPos = fromMaybe { x: 999.0, y: 999.0, z: 999.0 } (m2PosById aOccupied aNewHId)
    aFloorCH = B.minSeparation 6 1

    -- ── (b) addAtom far from everything ──────────────────────────────────
    -- Add H at x=600, far from the C at origin. Must land EXACTLY at {600,0,0}.
    bSt = B.addAtom 1 { x: 600.0, y: 0.0, z: 0.0 } aBaseC
    bNewHId = fromMaybe (-1) (map _.id (index bSt.atoms 1))
    bNewHPos = fromMaybe { x: -1.0, y: -1.0, z: -1.0 } (m2PosById bSt bNewHId)

    -- ── (c) moveAtom collision ────────────────────────────────────────────
    -- Two H far apart (1000 apart). Move H0 onto H1's exact position. H0 lands
    -- EXACTLY at target (1e-10, because it is the anchor). H1 gets pushed to
    -- >= minSeparation(1,1) - m2GeoTol.
    cH0 = B.addAtom 1 { x: 0.0, y: 0.0, z: 0.0 } B.emptyBuilder
    cH1 = B.addAtom 1 { x: 1000.0, y: 0.0, z: 0.0 } cH0
    cH0Id = fromMaybe (-1) (map _.id (index cH1.atoms 0))
    cH1Id = fromMaybe (-1) (map _.id (index cH1.atoms 1))
    cH1OrigPos = fromMaybe { x: 1000.0, y: 0.0, z: 0.0 } (m2PosById cH1 cH1Id)
    cMoved = B.moveAtom cH0Id cH1OrigPos cH1
    cH0PosAfter = fromMaybe { x: -1.0, y: -1.0, z: -1.0 } (m2PosById cMoved cH0Id)
    cH1PosAfter = fromMaybe { x: -1.0, y: -1.0, z: -1.0 } (m2PosById cMoved cH1Id)
    cFloorHH = B.minSeparation 1 1

    -- ── (d) moveAtom to a free spot ───────────────────────────────────────
    -- Two H far apart. Move H0 to {600,0,0} (free, > floor from H1 at 1000).
    -- H0 at target exactly (1e-10). H1 unmoved (1e-10).
    dFreeTarget = { x: 600.0, y: 0.0, z: 0.0 }
    dMoved = B.moveAtom cH0Id dFreeTarget cH1
    dH0PosAfter = fromMaybe { x: -1.0, y: -1.0, z: -1.0 } (m2PosById dMoved cH0Id)
    dH1PosAfter = fromMaybe { x: -1.0, y: -1.0, z: -1.0 } (m2PosById dMoved cH1Id)
    dH1OrigPos = fromMaybe { x: 1000.0, y: 0.0, z: 0.0 } (m2PosById cH1 cH1Id)

    -- ── (e) moveMolecule collision ─────────────────────────────────────────
    eMolBase = B.addAtom 1 { x: 0.0, y: 0.0, z: 0.0 } B.emptyBuilder
    eMolH1Id = fromMaybe (-1) (map _.id (index eMolBase.atoms 0))
    eMolWithH2 = B.addAtom 1 { x: 150.0, y: 0.0, z: 0.0 } eMolBase
    eMolH2Id = fromMaybe (-1) (map _.id (index eMolWithH2.atoms 1))
    eMolWithC = B.addAtom 6 { x: 2000.0, y: 0.0, z: 0.0 } eMolWithH2
    eMolCId = fromMaybe (-1) (map _.id (index eMolWithC.atoms 2))
    eMolCPos = fromMaybe { x: 2000.0, y: 0.0, z: 0.0 } (m2PosById eMolWithC eMolCId)
    eMolInternalDistBefore = m2Dist
      (fromMaybe { x: 0.0, y: 0.0, z: 0.0 } (m2PosById eMolWithC eMolH1Id))
      (fromMaybe { x: 150.0, y: 0.0, z: 0.0 } (m2PosById eMolWithC eMolH2Id))
    eMolMoved = B.moveMolecule eMolH1Id eMolCPos eMolWithC
    eMolH1PosAfter = fromMaybe { x: -1.0, y: -1.0, z: -1.0 } (m2PosById eMolMoved eMolH1Id)
    eMolH2PosAfter = fromMaybe { x: -1.0, y: -1.0, z: -1.0 } (m2PosById eMolMoved eMolH2Id)
    eMolCPosAfter = fromMaybe { x: -1.0, y: -1.0, z: -1.0 } (m2PosById eMolMoved eMolCId)
    eMolInternalDistAfter = m2Dist eMolH1PosAfter eMolH2PosAfter

    -- ── (f) bonding through the floor ─────────────────────────────────────
    fBase = B.addAtom 1 { x: 0.0, y: 0.0, z: 0.0 } B.emptyBuilder
    fWith2 = B.addAtom 1 { x: 50.0, y: 0.0, z: 0.0 } fBase
    fH0Id = fromMaybe (-1) (map _.id (index fWith2.atoms 0))
    fH1Id = fromMaybe (-1) (map _.id (index fWith2.atoms 1))
    fH0Pos = fromMaybe { x: 0.0, y: 0.0, z: 0.0 } (m2PosById fWith2 fH0Id)
    fH1Pos = fromMaybe { x: 50.0, y: 0.0, z: 0.0 } (m2PosById fWith2 fH1Id)
    fPairDist = m2Dist fH0Pos fH1Pos
    fFloorHH = B.minSeparation 1 1
    fComps = B.molecules fWith2

    -- ── (g) determinism ──────────────────────────────────────────────────
    gRun1 = B.addAtom 1 { x: 0.0, y: 0.0, z: 0.0 } (B.addAtom 6 { x: 0.0, y: 0.0, z: 0.0 } B.emptyBuilder)
    gRun2 = B.addAtom 1 { x: 0.0, y: 0.0, z: 0.0 } (B.addAtom 6 { x: 0.0, y: 0.0, z: 0.0 } B.emptyBuilder)
    gCId1 = fromMaybe (-1) (map _.id (index gRun1.atoms 0))
    gHId1 = fromMaybe (-1) (map _.id (index gRun1.atoms 1))
    gCId2 = fromMaybe (-1) (map _.id (index gRun2.atoms 0))
    gHId2 = fromMaybe (-1) (map _.id (index gRun2.atoms 1))
    gCPos1 = fromMaybe { x: 999.0, y: 999.0, z: 999.0 } (m2PosById gRun1 gCId1)
    gHPos1 = fromMaybe { x: 999.0, y: 999.0, z: 999.0 } (m2PosById gRun1 gHId1)
    gCPos2 = fromMaybe { x: 999.0, y: 999.0, z: 999.0 } (m2PosById gRun2 gCId2)
    gHPos2 = fromMaybe { x: 999.0, y: 999.0, z: 999.0 } (m2PosById gRun2 gHId2)

  -- (a) add-at-occupied: old atom (C) unchanged, new atom (H) pushed to >= floor
  check "M2 add-at-occupied: existing C position unchanged (x, 1e-10)" $
    abs (aCPosAfter.x - 0.0) < m2ExactTol
  check "M2 add-at-occupied: existing C position unchanged (y, 1e-10)" $
    abs (aCPosAfter.y - 0.0) < m2ExactTol
  check "M2 add-at-occupied: existing C position unchanged (z, 1e-10)" $
    abs (aCPosAfter.z - 0.0) < m2ExactTol
  check "M2 add-at-occupied: new H pushed to >= minSeparation(C,H) - 1e-6" $
    m2Dist aCPosAfter aNewHPos >= aFloorCH - m2GeoTol

  -- (b) addAtom far: new atom lands EXACTLY at requested position (1e-10)
  check "M2 addAtom far: new H lands exactly at x=600 (1e-10)" $
    abs (bNewHPos.x - 600.0) < m2ExactTol
  check "M2 addAtom far: new H lands exactly at y=0 (1e-10)" $
    abs (bNewHPos.y - 0.0) < m2ExactTol
  check "M2 addAtom far: new H lands exactly at z=0 (1e-10)" $
    abs (bNewHPos.z - 0.0) < m2ExactTol

  -- (c) moveAtom collision: moved atom (anchor) lands exactly at target, pushed atom >= floor
  check "M2 moveAtom collision: moved H0 lands exactly at target x (1e-10)" $
    abs (cH0PosAfter.x - cH1OrigPos.x) < m2ExactTol
  check "M2 moveAtom collision: moved H0 lands exactly at target y (1e-10)" $
    abs (cH0PosAfter.y - cH1OrigPos.y) < m2ExactTol
  check "M2 moveAtom collision: moved H0 lands exactly at target z (1e-10)" $
    abs (cH0PosAfter.z - cH1OrigPos.z) < m2ExactTol
  check "M2 moveAtom collision: pushed H1 >= minSeparation(H,H) - 1e-6 from H0" $
    m2Dist cH0PosAfter cH1PosAfter >= cFloorHH - m2GeoTol

  -- (d) moveAtom free spot: moved atom exactly at target, other atom unmoved
  check "M2 moveAtom free: H0 lands exactly at target x=600 (1e-10)" $
    abs (dH0PosAfter.x - dFreeTarget.x) < m2ExactTol
  check "M2 moveAtom free: H0 lands exactly at target y=0 (1e-10)" $
    abs (dH0PosAfter.y - dFreeTarget.y) < m2ExactTol
  check "M2 moveAtom free: H0 lands exactly at target z=0 (1e-10)" $
    abs (dH0PosAfter.z - dFreeTarget.z) < m2ExactTol
  check "M2 moveAtom free: H1 unmoved (x, 1e-10)" $
    abs (dH1PosAfter.x - dH1OrigPos.x) < m2ExactTol
  check "M2 moveAtom free: H1 unmoved (y, 1e-10)" $
    abs (dH1PosAfter.y - dH1OrigPos.y) < m2ExactTol

  -- (e) moveMolecule collision: anchor at target exactly, rigid internal distance, cross >= floor
  check "M2 moveMolecule collision: anchor H1 lands exactly at C target x (1e-10)" $
    abs (eMolH1PosAfter.x - eMolCPos.x) < m2ExactTol
  check "M2 moveMolecule collision: anchor H1 lands exactly at C target y (1e-10)" $
    abs (eMolH1PosAfter.y - eMolCPos.y) < m2ExactTol
  check "M2 moveMolecule collision: anchor H1 lands exactly at C target z (1e-10)" $
    abs (eMolH1PosAfter.z - eMolCPos.z) < m2ExactTol
  check "M2 moveMolecule collision: internal H-H distance preserved after move (1e-6)" $
    abs (eMolInternalDistAfter - eMolInternalDistBefore) < m2GeoTol
  check "M2 moveMolecule collision: H1 x C >= minSeparation(H,C) - 1e-6" $
    m2Dist eMolH1PosAfter eMolCPosAfter >= B.minSeparation 1 6 - m2GeoTol
  check "M2 moveMolecule collision: H2 x C >= minSeparation(H,C) - 1e-6" $
    m2Dist eMolH2PosAfter eMolCPosAfter >= B.minSeparation 1 6 - m2GeoTol

  -- (f) bonding through the floor: sub-floor add → pair separates to >= floor AND bonds
  check "M2 bonding through floor: pair distance >= floor - 1e-6 after addAtom" $
    fPairDist >= fFloorHH - m2GeoTol
  check "M2 bonding through floor: pair distance < bondThreshold (bond range)" $
    fPairDist < B.bondThreshold
  check "M2 bonding through floor: exactly one bond forms" $
    length fWith2.bonds == 1
  check "M2 bonding through floor: molecules has ONE 2-atom component" $
    length fComps == 1 && all (\comp -> length comp == 2) fComps

  -- (g) determinism: identical inputs → identical final positions (1e-10)
  check "M2 determinism: run1 C pos.x == run2 C pos.x (1e-10)" $
    abs (gCPos1.x - gCPos2.x) < m2ExactTol
  check "M2 determinism: run1 C pos.y == run2 C pos.y (1e-10)" $
    abs (gCPos1.y - gCPos2.y) < m2ExactTol
  check "M2 determinism: run1 H pos.x == run2 H pos.x (1e-10)" $
    abs (gHPos1.x - gHPos2.x) < m2ExactTol
  check "M2 determinism: run1 H pos.y == run2 H pos.y (1e-10)" $
    abs (gHPos1.y - gHPos2.y) < m2ExactTol

  log "all M2 op-wiring properties hold."

  -- ───── S2: Pe / Builder.Overlap.minSeparation equivalence guard ──────────────
  -- CRITICAL: Pe.purs inlines copies of Builder.Overlap's floor constants
  -- (contactFactorPe, absoluteMinPe, floorCeilPe) to avoid an import cycle.
  -- These tests import Builder.Overlap.minSeparation directly and compare it
  -- against Pe's inlined formula, catching any silent constant drift.
  --
  -- Tolerance note:
  --   • Pe.wallDistanceFor z1 z2 is NOT identical to minSeparation: it equals
  --     R0 - (R0-minSep) * ln(1+sqrt(10)) / ln(1+sqrt(11)) ≈ minSep + 4.4
  --     for a 30-unit gap (H-H/C-C). Documented tolerance: 6.0 world units.
  --   • The inline formula (contactFactorPe * (r1+r2) clamped to [130,165])
  --     must produce EXACTLY the same result as Builder.Overlap.minSeparation
  --     (same constants, same clamp): tolerance 1e-10.
  log "S2 Pe / Builder.Overlap.minSeparation equivalence guard:"

  let
    -- Re-compute minSepPe inline (Pe.purs formula, using Pe's exported constants)
    -- so we can assert it equals Builder.Overlap.minSeparation exactly.
    minSepPeInline z1 z2 =
      let raw = Pe.contactFactorPe * (Atom.atomicRadius z1 + Atom.atomicRadius z2)
      in max Pe.absoluteMinPe (min Pe.floorCeilPe raw)

    -- Tolerance for wallDistanceFor ≈ minSeparation (documented ~4.4 + margin).
    wallEquivTol :: Number
    wallEquivTol = 6.0

    -- Tolerance for the inlined-formula exact match (floating-point identity).
    exactEquivTol :: Number
    exactEquivTol = 1.0e-10

  -- ── (1) Inlined Pe floor constants match Builder.Overlap constants exactly ──
  check "S2 constant: Pe.contactFactorPe == Builder.Overlap.contactFactor (55.0)" $
    approxEq Pe.contactFactorPe B.contactFactor
  check "S2 constant: Pe.absoluteMinPe == Builder.Overlap.absoluteMin (130.0)" $
    approxEq Pe.absoluteMinPe B.absoluteMin
  check "S2 constant: Pe.floorCeilPe == Builder.Overlap.floorCeil (165.0)" $
    approxEq Pe.floorCeilPe B.floorCeil

  -- ── (2) Inline formula produces same result as Builder.Overlap.minSeparation ──
  -- H-H (1,1): absoluteMin is active (55*(0.31+0.31)=34.1 < 130) → 130.0.
  check "S2 inline formula == minSeparation H-H (1,1): exact match (1e-10)" $
    abs (minSepPeInline 1 1 - Overlap.minSeparation 1 1) < exactEquivTol
  -- C-C (6,6): absoluteMin is active → 130.0.
  check "S2 inline formula == minSeparation C-C (6,6): exact match (1e-10)" $
    abs (minSepPeInline 6 6 - Overlap.minSeparation 6 6) < exactEquivTol
  -- O-H (8,1): absoluteMin is active → 130.0.
  check "S2 inline formula == minSeparation O-H (8,1): exact match (1e-10)" $
    abs (minSepPeInline 8 1 - Overlap.minSeparation 8 1) < exactEquivTol
  -- K-K (19,19): floorCeil is active → 165.0.
  check "S2 inline formula == minSeparation K-K (19,19): exact match (1e-10)" $
    abs (minSepPeInline 19 19 - Overlap.minSeparation 19 19) < exactEquivTol
  -- Full sweep: inline formula == Builder.Overlap.minSeparation for ALL z1,z2 in 1..36.
  check "S2 inline formula == minSeparation for all 36x36 element pairs (1e-10)" $
    all identity do
      z1 <- range 1 36
      z2 <- range 1 36
      pure (abs (minSepPeInline z1 z2 - Overlap.minSeparation z1 z2) < exactEquivTol)

  -- ── (3) Pe.wallDistanceFor ≈ Builder.Overlap.minSeparation (within 6 wu) ──
  -- wallDistanceFor lands slightly above minSep by design (see Pe.purs comment).
  -- The tolerance proves the Morse wall crossings are physically consistent with
  -- the Pauli contact floor. Tighter than the S1 test (which used 5.0 vs absoluteMinPe).
  check "S2 wallDistanceFor H-H ≈ minSeparation H-H (within 6 world units)" $
    abs (Pe.wallDistanceFor 1 1 - Overlap.minSeparation 1 1) < wallEquivTol
  check "S2 wallDistanceFor C-C ≈ minSeparation C-C (within 6 world units)" $
    abs (Pe.wallDistanceFor 6 6 - Overlap.minSeparation 6 6) < wallEquivTol
  check "S2 wallDistanceFor O-H ≈ minSeparation O-H (within 6 world units)" $
    abs (Pe.wallDistanceFor 8 1 - Overlap.minSeparation 8 1) < wallEquivTol
  -- Direction: wallDistanceFor >= minSeparation (wall crossing is above the hard floor).
  check "S2 wallDistanceFor H-H >= minSeparation H-H (wall at or above floor)" $
    Pe.wallDistanceFor 1 1 >= Overlap.minSeparation 1 1
  check "S2 wallDistanceFor C-C >= minSeparation C-C (wall at or above floor)" $
    Pe.wallDistanceFor 6 6 >= Overlap.minSeparation 6 6
  check "S2 wallDistanceFor O-H >= minSeparation O-H (wall at or above floor)" $
    Pe.wallDistanceFor 8 1 >= Overlap.minSeparation 8 1

  log "all S2 Pe/Builder.Overlap.minSeparation equivalence properties hold."
