module Test.BuilderBondsSpec where

import Prelude

import Data.Array (all, any, index, length, range)
import Data.Foldable (minimum)
import Data.Maybe (Maybe(..), fromMaybe, isJust)
import Data.Number (abs, sqrt)
import Effect (Effect)
import Effect.Console (log)
import Chem (bondEnergy)
import Builder as B
import Test.Util (approxEq, check)

builderBondsSpec :: Effect Unit
builderBondsSpec = do
  -- ───── M1: Chem.bondEnergy + Builder.pullBonds + Builder.moveAtomWith ───────
  log "M1 bond-strength (bondEnergy / pullBonds / moveAtomWith) properties:"

  -- (a) SYMMETRY: bondEnergy z1 z2 == bondEnergy z2 z1 for ALL z1, z2 in 1..36.
  check "bondEnergy is symmetric for all element pairs (1..36)" $
    all identity do
      z1 <- range 1 36
      z2 <- range 1 36
      pure (bondEnergy z1 z2 == bondEnergy z2 z1)

  -- (b) CLAMP-SAFETY: out-of-range Z clamps without crashing; NaN-free (x == x).
  check "bondEnergy 0 0 == bondEnergy 1 1 (clamp low)" $
    bondEnergy 0 0 == bondEnergy 1 1
  check "bondEnergy 99 99 == bondEnergy 36 36 (clamp high)" $
    bondEnergy 99 99 == bondEnergy 36 36
  check "bondEnergy (-5) 200 == bondEnergy 1 36 (clamp both)" $
    bondEnergy (-5) 200 == bondEnergy 1 36
  check "bondEnergy 1 1 is finite (not NaN)" $
    bondEnergy 1 1 == bondEnergy 1 1
  check "bondEnergy 36 36 is finite (not NaN)" $
    bondEnergy 36 36 == bondEnergy 36 36
  check "bondEnergy 1 36 is finite (not NaN)" $
    bondEnergy 1 36 == bondEnergy 1 36

  -- (c) TABULATED SPOT VALUES (1e-10 tolerance): pins the agreed energy table.
  check "bondEnergy H-H (1,1) == 4.36 (1e-10)" $
    approxEq (bondEnergy 1 1) 4.36
  check "bondEnergy C-H (6,1) == 4.13 (1e-10)" $
    approxEq (bondEnergy 6 1) 4.13
  check "bondEnergy O-H (8,1) == 4.63 (1e-10)" $
    approxEq (bondEnergy 8 1) 4.63
  check "bondEnergy O-O (8,8) == 1.46 (1e-10)" $
    approxEq (bondEnergy 8 8) 1.46
  check "bondEnergy C-C (6,6) == 3.46 (1e-10)" $
    approxEq (bondEnergy 6 6) 3.46
  check "bondEnergy H-F (1,9) == 5.65 (1e-10)" $
    approxEq (bondEnergy 1 9) 5.65

  -- (d) FALLBACK — geometric mean when BOTH homonuclear anchors are tabulated.
  let
    cSiGeoMean = sqrt (3.46 * 2.22)
  check "bondEnergy C-Si (6,14) == sqrt(3.46*2.22) within 1e-10 (geo-mean fallback)" $
    approxEq (bondEnergy 6 14) cSiGeoMean

  -- FALLBACK — two untabulated-homonuclear elements → geometric mean of 1.5·1.5 = 1.5.
  check "bondEnergy Ne-Ar (10,18) == 1.5 (untabulated-homonuclear fallback)" $
    approxEq (bondEnergy 10 18) 1.5

  -- (e) WEAK-BREAK: two O atoms (Z=8) bonded at 150 apart; drag one O to x=600
  --     using moveAtomWith 3.0 (strength 3.0 > O-O energy 1.46, so bond is
  --     weak relative to drag strength → bond breaks).
  let
    -- Build O-O bonded pair: O at origin, O at x=150 (< bondThreshold 180 → bonds).
    ooBase = B.addAtom 8 { x: 0.0, y: 0.0, z: 0.0 } B.emptyBuilder
    ooWith2nd = B.addAtom 8 { x: 150.0, y: 0.0, z: 0.0 } ooBase
    ooId0 = fromMaybe (-1) (map _.id (index ooWith2nd.atoms 0))
    ooId1 = fromMaybe (-1) (map _.id (index ooWith2nd.atoms 1))
    ooOrigPos1 = fromMaybe { x: 150.0, y: 0.0, z: 0.0 } (map _.pos (B.atomById ooWith2nd ooId1))
    -- Drag ooId0 to x=600 with strength 3.0 (> O-O 1.46 → bond too weak to resist).
    ooTarget = { x: 600.0, y: 0.0, z: 0.0 }
    ooMoved = B.moveAtomWith 3.0 ooId0 ooTarget ooWith2nd
    ooDraggedPos = fromMaybe { x: -1.0, y: -1.0, z: -1.0 } (map _.pos (B.atomById ooMoved ooId0))
    ooPartnerPos = fromMaybe { x: -1.0, y: -1.0, z: -1.0 } (map _.pos (B.atomById ooMoved ooId1))

  check "WEAK-BREAK: O-O bond exists before drag" $
    length ooWith2nd.bonds == 1
  check "WEAK-BREAK: O-O (1.46 < 3.0) bond breaks after moveAtomWith 3.0" $
    length ooMoved.bonds == 0
  check "WEAK-BREAK: dragged O lands exactly at (600,0,0) (1e-10)" $
    approxEq ooDraggedPos.x 600.0 && approxEq ooDraggedPos.y 0.0 && approxEq ooDraggedPos.z 0.0
  check "WEAK-BREAK: partner O stays within 1e-6 of its original position" $
    abs (ooPartnerPos.x - ooOrigPos1.x) < 1.0e-6
      && abs (ooPartnerPos.y - ooOrigPos1.y) < 1.0e-6
      && abs (ooPartnerPos.z - ooOrigPos1.z) < 1.0e-6

  -- (f) STRONG-HOLD: O(Z=8) and H(Z=1) bonded at 150; drag O to x=600 with
  --     strength 3.0 (O-H energy 4.63 >= 3.0 → bond is STRONG → H is pulled).
  let
    ohBase = B.addAtom 8 { x: 0.0, y: 0.0, z: 0.0 } B.emptyBuilder
    ohWith1 = B.addAtom 1 { x: 150.0, y: 0.0, z: 0.0 } ohBase
    ohOId = fromMaybe (-1) (map _.id (index ohWith1.atoms 0))
    ohHId = fromMaybe (-1) (map _.id (index ohWith1.atoms 1))
    ohHOrigPos = fromMaybe { x: 150.0, y: 0.0, z: 0.0 } (map _.pos (B.atomById ohWith1 ohHId))
    ohTarget = { x: 600.0, y: 0.0, z: 0.0 }
    ohMoved = B.moveAtomWith 3.0 ohOId ohTarget ohWith1
    ohDraggedPos = fromMaybe { x: -1.0, y: -1.0, z: -1.0 } (map _.pos (B.atomById ohMoved ohOId))
    ohPartnerPos = fromMaybe { x: -1.0, y: -1.0, z: -1.0 } (map _.pos (B.atomById ohMoved ohHId))

  check "STRONG-HOLD: O-H bond exists before drag" $
    length ohWith1.bonds == 1
  check "STRONG-HOLD: O-H (4.63 >= 3.0) bond survives after moveAtomWith 3.0" $
    length ohMoved.bonds == 1
  check "STRONG-HOLD: pulled H distance from dragged O <= breakThreshold + 1e-6" $
    let
      pullDist3 a b =
        let
          ddx = a.x - b.x
          ddy = a.y - b.y
          ddz = a.z - b.z
        in
          sqrt (ddx * ddx + ddy * ddy + ddz * ddz)
    in
      pullDist3 ohDraggedPos ohPartnerPos <= B.breakThreshold + 1.0e-6
  check "STRONG-HOLD: pulled H distance from dragged O >= minSeparation O H - 1e-6" $
    let
      pullDist3 a b =
        let
          ddx = a.x - b.x
          ddy = a.y - b.y
          ddz = a.z - b.z
        in
          sqrt (ddx * ddx + ddy * ddy + ddz * ddz)
    in
      pullDist3 ohDraggedPos ohPartnerPos >= B.minSeparation 8 1 - 1.0e-6
  check "STRONG-HOLD: H partner position CHANGED (was pulled by the strong bond)" $
    abs (ohPartnerPos.x - ohHOrigPos.x) > 1.0
      || abs (ohPartnerPos.y - ohHOrigPos.y) > 1.0
      || abs (ohPartnerPos.z - ohHOrigPos.z) > 1.0

  -- (g) CHAIN TUG: 3-atom chain H(id0)-O-H(id2), O in the middle.
  let
    chainH0Base = B.addAtom 1 { x: 0.0, y: 0.0, z: 0.0 } B.emptyBuilder
    chainH0Id = fromMaybe (-1) (map _.id (index chainH0Base.atoms 0))
    chainWithO = B.addAtom 8 { x: 150.0, y: 0.0, z: 0.0 } chainH0Base
    chainOId = fromMaybe (-1) (map _.id (index chainWithO.atoms 1))
    chainWith3 = B.addAtom 1 { x: 300.0, y: 0.0, z: 0.0 } chainWithO
    chainH2Id = fromMaybe (-1) (map _.id (index chainWith3.atoms 2))
    chainOrigH0Pos = fromMaybe { x: 0.0, y: 0.0, z: 0.0 } (map _.pos (B.atomById chainWith3 chainH0Id))
    chainOrigOPos = fromMaybe { x: 150.0, y: 0.0, z: 0.0 } (map _.pos (B.atomById chainWith3 chainOId))
    chainOrigH2Pos = fromMaybe { x: 300.0, y: 0.0, z: 0.0 } (map _.pos (B.atomById chainWith3 chainH2Id))
    chainTarget = { x: -400.0, y: 0.0, z: 0.0 }
    chainMoved = B.moveAtomWith 3.0 chainH0Id chainTarget chainWith3
    chainMovedH0Pos = fromMaybe { x: 0.0, y: 0.0, z: 0.0 } (map _.pos (B.atomById chainMoved chainH0Id))
    chainMovedOPos = fromMaybe { x: 150.0, y: 0.0, z: 0.0 } (map _.pos (B.atomById chainMoved chainOId))
    chainMovedH2Pos = fromMaybe { x: 300.0, y: 0.0, z: 0.0 } (map _.pos (B.atomById chainMoved chainH2Id))
    chainDisp p0 p1 = abs (p1.x - p0.x) + abs (p1.y - p0.y) + abs (p1.z - p0.z)

  check "CHAIN TUG: two H-O bonds exist before drag (H-O and O-H)" $
    length chainWith3.bonds == 2
  check "CHAIN TUG: no H-H bond (300 apart > bondThreshold)" $
    not (any (\bd -> (bd.a == chainH0Id && bd.b == chainH2Id) || (bd.a == chainH2Id && bd.b == chainH0Id)) chainWith3.bonds)
  check "CHAIN TUG: BOTH bonds survive after drag (4.63 >= 3.0)" $
    length chainMoved.bonds == 2
  check "CHAIN TUG: dragged H (id0) is displaced > 1.0" $
    chainDisp chainOrigH0Pos chainMovedH0Pos > 1.0
  check "CHAIN TUG: middle O is displaced > 1.0 (pulled by chain)" $
    chainDisp chainOrigOPos chainMovedOPos > 1.0
  check "CHAIN TUG: far H (id2) is displaced > 1.0 (pulled transitively)" $
    chainDisp chainOrigH2Pos chainMovedH2Pos > 1.0
  check "CHAIN TUG: atom ids are preserved in chain" $
    isJust (B.atomById chainMoved chainH0Id)
      && isJust (B.atomById chainMoved chainOId)
      && isJust (B.atomById chainMoved chainH2Id)

  -- (h) MIXED CHAIN: O-O-H chain (O at x=0, O at x=150 bonded, H at x=300 bonded
  --     to the second O).  Drag the FIRST O (id0) far with strength 3.0:
  --     the O-O bond (1.46 < 3.0) breaks, the O-H bond (4.63 >= 3.0) survives.
  let
    mixedO1Base = B.addAtom 8 { x: 0.0, y: 0.0, z: 0.0 } B.emptyBuilder
    mixedO1Id = fromMaybe (-1) (map _.id (index mixedO1Base.atoms 0))
    mixedWithO2 = B.addAtom 8 { x: 150.0, y: 0.0, z: 0.0 } mixedO1Base
    mixedO2Id = fromMaybe (-1) (map _.id (index mixedWithO2.atoms 1))
    mixedWithH = B.addAtom 1 { x: 300.0, y: 0.0, z: 0.0 } mixedWithO2
    mixedHId = fromMaybe (-1) (map _.id (index mixedWithH.atoms 2))
    mixedTarget = { x: 2000.0, y: 0.0, z: 0.0 }
    mixedMoved = B.moveAtomWith 3.0 mixedO1Id mixedTarget mixedWithH
    mixedBonds = mixedMoved.bonds
    mixedSurvivingBond = index mixedBonds 0

  check "MIXED CHAIN: O-O and O-H bonds exist before drag (2 bonds)" $
    length mixedWithH.bonds == 2
  check "MIXED CHAIN: after drag with strength 3.0, exactly 1 bond remains" $
    length mixedBonds == 1
  check "MIXED CHAIN: surviving bond connects O2 and H (not O1)" $
    case mixedSurvivingBond of
      Nothing -> false
      Just bd ->
        (bd.a == mixedO2Id && bd.b == mixedHId)
          || (bd.a == mixedHId && bd.b == mixedO2Id)

  -- (i) MOVEATOMCOMPAT: moveAtom dragging O far → bond breaks (legacy behaviour).
  let
    compatBase = B.addAtom 8 { x: 0.0, y: 0.0, z: 0.0 } B.emptyBuilder
    compatWith1 = B.addAtom 1 { x: 150.0, y: 0.0, z: 0.0 } compatBase
    compatOId = fromMaybe (-1) (map _.id (index compatWith1.atoms 0))
    compatHId = fromMaybe (-1) (map _.id (index compatWith1.atoms 1))
    compatFarTarget = { x: 2000.0, y: 0.0, z: 0.0 }
    -- Legacy moveAtom: drags O far, bond breaks (O-H 4.63, but moveAtom ignores energy).
    compatLegacy = B.moveAtom compatOId compatFarTarget compatWith1
    -- moveAtomWith 1e18: strength > any bond energy → pullBonds never pulls → same as moveAtom.
    compatWith1e18 = B.moveAtomWith 1.0e18 compatOId compatFarTarget compatWith1
    -- Positions of both atoms after each operation.
    compatLegacyOPos = map _.pos (B.atomById compatLegacy compatOId)
    compatLegacyHPos = map _.pos (B.atomById compatLegacy compatHId)
    compatWith1e18OPos = map _.pos (B.atomById compatWith1e18 compatOId)
    compatWith1e18HPos = map _.pos (B.atomById compatWith1e18 compatHId)

  check "COMPAT: moveAtom drags O far → bond breaks (legacy)" $
    length compatLegacy.bonds == 0
  check "COMPAT: moveAtomWith 1e18 O pos equals moveAtom O pos (1e-10)" $
    case compatLegacyOPos, compatWith1e18OPos of
      Just a, Just b -> approxEq a.x b.x && approxEq a.y b.y && approxEq a.z b.z
      _, _ -> false
  check "COMPAT: moveAtomWith 1e18 H pos equals moveAtom H pos (1e-10)" $
    case compatLegacyHPos, compatWith1e18HPos of
      Just a, Just b -> approxEq a.x b.x && approxEq a.y b.y && approxEq a.z b.z
      _, _ -> false
  check "COMPAT: moveAtomWith 1e18 bond count equals moveAtom bond count" $
    length compatLegacy.bonds == length compatWith1e18.bonds

  -- (j) DETERMINISM: scenario (f) run twice → identical positions and bond count (1e-10).
  let
    detRun1 = B.moveAtomWith 3.0 ohOId ohTarget ohWith1
    detRun2 = B.moveAtomWith 3.0 ohOId ohTarget ohWith1
    detOPos1 = map _.pos (B.atomById detRun1 ohOId)
    detHPos1 = map _.pos (B.atomById detRun1 ohHId)
    detOPos2 = map _.pos (B.atomById detRun2 ohOId)
    detHPos2 = map _.pos (B.atomById detRun2 ohHId)

  check "DETERMINISM: run1 dragged O pos == run2 dragged O pos (1e-10)" $
    case detOPos1, detOPos2 of
      Just a, Just b -> approxEq a.x b.x && approxEq a.y b.y && approxEq a.z b.z
      _, _ -> false
  check "DETERMINISM: run1 pulled H pos == run2 pulled H pos (1e-10)" $
    case detHPos1, detHPos2 of
      Just a, Just b -> approxEq a.x b.x && approxEq a.y b.y && approxEq a.z b.z
      _, _ -> false
  check "DETERMINISM: run1 bond count == run2 bond count" $
    length detRun1.bonds == length detRun2.bonds

  -- (k) DRAGGED-AT-TARGET: in scenario (f) the dragged O lands exactly at (600,0,0) (1e-10).
  check "DRAGGED-AT-TARGET: dragged O is exactly at (600,0,0) x (1e-10)" $
    approxEq ohDraggedPos.x 600.0
  check "DRAGGED-AT-TARGET: dragged O is exactly at (600,0,0) y (1e-10)" $
    approxEq ohDraggedPos.y 0.0
  check "DRAGGED-AT-TARGET: dragged O is exactly at (600,0,0) z (1e-10)" $
    approxEq ohDraggedPos.z 0.0

  log "all M1 bond-strength properties hold."

  -- ───── M2: short-stretch O-H scenario (within breakThreshold) ────────
  log "M2 short-stretch O-H scenario properties:"

  let
    shortOBase = B.addAtom 8 { x: 0.0, y: 0.0, z: 0.0 } B.emptyBuilder
    shortOId = fromMaybe (-1) (map _.id (index shortOBase.atoms 0))
    shortWithH = B.addAtom 1 { x: 150.0, y: 0.0, z: 0.0 } shortOBase
    shortHId = fromMaybe (-1) (map _.id (index shortWithH.atoms 1))
    shortHOrigPos = fromMaybe { x: 150.0, y: 0.0, z: 0.0 } (map _.pos (B.atomById shortWithH shortHId))
    -- Drag O to x=250 — the bond stretches but, since O-H is strong (4.63 >= 3.0),
    -- pullBonds pulls H back so the pair stays within breakThreshold.
    shortTarget = { x: 250.0, y: 0.0, z: 0.0 }
    shortMoved = B.moveAtomWith 3.0 shortOId shortTarget shortWithH
    shortMovedOPos = fromMaybe { x: -1.0, y: -1.0, z: -1.0 } (map _.pos (B.atomById shortMoved shortOId))
    shortMovedHPos = fromMaybe { x: -1.0, y: -1.0, z: -1.0 } (map _.pos (B.atomById shortMoved shortHId))
    shortDist3 a b =
      let
        dx3 = a.x - b.x
        dy3 = a.y - b.y
        dz3 = a.z - b.z
      in
        sqrt (dx3 * dx3 + dy3 * dy3 + dz3 * dz3)

  check "short-stretch: O-H bond exists before drag" $
    length shortWithH.bonds == 1
  check "short-stretch: O-H bond survives after drag to x=250 (strength 3.0)" $
    length shortMoved.bonds == 1
  check "short-stretch: O lands at target x=250 (1e-10)" $
    approxEq shortMovedOPos.x 250.0
  check "short-stretch: O-H distance <= breakThreshold + 1e-6 after drag" $
    shortDist3 shortMovedOPos shortMovedHPos <= B.breakThreshold + 1.0e-6
  check "short-stretch: H partner IS moved (pulled by strong O-H bond)" $
    abs (shortMovedHPos.x - shortHOrigPos.x) > 1.0
      || abs (shortMovedHPos.y - shortHOrigPos.y) > 1.0
      || abs (shortMovedHPos.z - shortHOrigPos.z) > 1.0

  log "all M2 short-stretch O-H scenario properties hold."

  -- ───── M2: strength-0 scenario (nothing breaks, every bond holds) ─────
  log "M2 strength-0 scenario (weakest bond resists drag to 600) properties:"

  let
    s0OBase = B.addAtom 8 { x: 0.0, y: 0.0, z: 0.0 } B.emptyBuilder
    s0OId0 = fromMaybe (-1) (map _.id (index s0OBase.atoms 0))
    s0WithO2 = B.addAtom 8 { x: 150.0, y: 0.0, z: 0.0 } s0OBase
    s0OId1 = fromMaybe (-1) (map _.id (index s0WithO2.atoms 1))
    s0Target = { x: 600.0, y: 0.0, z: 0.0 }
    s0Moved = B.moveAtomWith 0.0 s0OId0 s0Target s0WithO2
    s0DraggedPos = fromMaybe { x: -1.0, y: -1.0, z: -1.0 } (map _.pos (B.atomById s0Moved s0OId0))
    s0PartnerPos = fromMaybe { x: -1.0, y: -1.0, z: -1.0 } (map _.pos (B.atomById s0Moved s0OId1))
    s0Dist = sqrt
      ( (s0DraggedPos.x - s0PartnerPos.x) * (s0DraggedPos.x - s0PartnerPos.x)
          + (s0DraggedPos.y - s0PartnerPos.y) * (s0DraggedPos.y - s0PartnerPos.y)
          + (s0DraggedPos.z - s0PartnerPos.z) * (s0DraggedPos.z - s0PartnerPos.z)
      )

  check "strength-0: O-O bond exists before drag" $
    length s0WithO2.bonds == 1
  check "strength-0: O-O bond SURVIVES moveAtomWith 0.0 to x=600 (held + tugged)" $
    length s0Moved.bonds == 1
  check "strength-0: dragged O lands exactly at x=600 (1e-10)" $
    approxEq s0DraggedPos.x 600.0
  check "strength-0: dragged O lands exactly at y=0 (1e-10)" $
    approxEq s0DraggedPos.y 0.0
  check "strength-0: partner was pulled along (within breakThreshold of dragged)" $
    s0Dist <= B.breakThreshold + 1.0e-6
  check "strength-0: partner not below the Pauli floor of the pair" $
    s0Dist >= B.minSeparation 8 8 - 1.0e-6

  log "all M2 strength-0 scenario properties hold."

  -- ───── M1 3D spawn: Builder.spawnPos (golden-angle/Fibonacci-shell) ──────────
  log "M1 3D spawn (Builder.spawnPos) properties:"

  let
    -- Collect spawnPos values for i = 0..5 for spread tests.
    spawnVals = map B.spawnPos (range 0 5)
    spawnZs = map _.z spawnVals
    spawnYs = map _.y spawnVals
    sp0 = B.spawnPos 0
    sp1 = B.spawnPos 1
    sp3 = B.spawnPos 3

    -- Distance between two V3 points (shared geometry helper).
    spawnDist a b =
      let
        sdx = a.x - b.x
        sdy = a.y - b.y
        sdz = a.z - b.z
      in
        sqrt (sdx * sdx + sdy * sdy + sdz * sdz)

    -- Build a 5-atom world using spawnPos for each atom's position, mirroring
    -- how BuilderApi will call it: addAtom z (spawnPos n) where n = current count.
    spawn5 =
      B.addAtom 1 (B.spawnPos 4)
        ( B.addAtom 1 (B.spawnPos 3)
            ( B.addAtom 1 (B.spawnPos 2)
                ( B.addAtom 1 (B.spawnPos 1)
                    (B.addAtom 1 (B.spawnPos 0) B.emptyBuilder)
                )
            )
        )

    -- Minimum pairwise centre distance over all atom pairs in a BuilderState.
    minPairDist st =
      let
        atoms = st.atoms
        n = length atoms
        pairs = do
          i <- range 0 (n - 1)
          j <- range 0 (n - 1)
          if j <= i then []
          else case index atoms i, index atoms j of
            Just a, Just b -> [ spawnDist a.pos b.pos ]
            _, _ -> []
      in
        fromMaybe 0.0 (minimum pairs)

  -- (a) DETERMINISM: calling spawnPos with the same index twice returns the same V3.
  check "spawnPos determinism: spawnPos 0 == spawnPos 0 (x)" $
    (B.spawnPos 0).x == sp0.x
  check "spawnPos determinism: spawnPos 0 == spawnPos 0 (y)" $
    (B.spawnPos 0).y == sp0.y
  check "spawnPos determinism: spawnPos 0 == spawnPos 0 (z)" $
    (B.spawnPos 0).z == sp0.z
  check "spawnPos determinism: spawnPos 1 == spawnPos 1 (x)" $
    (B.spawnPos 1).x == sp1.x
  check "spawnPos determinism: spawnPos 3 == spawnPos 3 (z)" $
    (B.spawnPos 3).z == sp3.z

  -- (b) FINITE: spawnPos 0 components are valid Numbers (not NaN).
  check "spawnPos 0 x is finite (not NaN)" $
    sp0.x == sp0.x
  check "spawnPos 0 y is finite (not NaN)" $
    sp0.y == sp0.y
  check "spawnPos 0 z is finite (not NaN)" $
    sp0.z == sp0.z

  -- (c) GENUINELY 3D over i=0..5: z-values are not all equal AND y-values are not
  --     all equal. A collinear y=z=0 spawn (the old behaviour) would fail both.
  check "spawnPos: z-values over i=0..5 are not all the same (3D spread in z)" $
    any (\z -> not (approxEq z (fromMaybe 0.0 (index spawnZs 0)))) spawnZs
  check "spawnPos: y-values over i=0..5 are not all the same (3D spread in y)" $
    any (\y -> not (approxEq y (fromMaybe 0.0 (index spawnYs 0)))) spawnYs

  -- (d) PAULI FLOOR after build: spawn 5 H atoms via spawnPos (indices 0..4),
  --     then assert the minimum pairwise centre distance >= absoluteMin - 1e-6.
  check "spawnPos: 5-atom world has all pairs >= absoluteMin - 1e-6 (Pauli floor)" $
    minPairDist spawn5 >= B.absoluteMin - 1.0e-6

  log "all M1 3D spawn (Builder.spawnPos) properties hold."
