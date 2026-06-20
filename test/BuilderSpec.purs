module Test.BuilderSpec where

import Prelude

import Data.Array (all, any, filter, index, length)
import Data.Foldable (maximum, minimum, sum)
import Data.Int (toNumber)
import Data.Maybe (Maybe(..), fromMaybe, isJust)
import Data.Number (abs, sqrt)
import Effect (Effect)
import Effect.Console (log)
import Atom as Atom
import Chem (valence)
import Builder as B
import Test.Util (approxEq, check, testProjection)

builderSpec :: Effect Unit
builderSpec = do
  -- ───── Builder + Chem (molecule builder) ───────────────────────────
  log "builder + chem properties:"

  -- Chem.valence: standard main-group valences, transition-metal default,
  -- and clamp-safety at the edges (no crash, defined Ints).
  check "valence H(1) = 1" $ valence 1 == 1
  check "valence C(6) = 4" $ valence 6 == 4
  check "valence N(7) = 3" $ valence 7 == 3
  check "valence O(8) = 2" $ valence 8 == 2
  check "valence He(2) = 0" $ valence 2 == 0
  check "valence Ne(10) = 0" $ valence 10 == 0
  check "valence Fe(26) = 2 (transition-metal default)" $ valence 26 == 2
  -- Clamp-safe: out-of-range Z returns a defined Int (compare to itself ⇒ total).
  check "valence 0 is defined (clamp-safe)" $ valence 0 == valence 0
  check "valence 999 is defined (clamp-safe)" $ valence 999 == valence 999

  -- Builder immutability + append: ids are fresh and monotonic, prior atoms
  -- are untouched, and updates return new records.
  let
    posA = { x: 0.0, y: 0.0, z: 0.0 }
    posB = { x: 1000.0, y: 0.0, z: 0.0 } -- far from posA: no bond
    posC = { x: 500.0, y: 0.0, z: 0.0 }
    b0 = B.emptyBuilder
    b1 = B.addAtom 1 posA b0
    b2 = B.addAtom 1 posB b1
    atomAt st i = index st.atoms i
    idOf st i = map _.id (atomAt st i)
    posOf st i = map _.pos (atomAt st i)

  check "emptyBuilder has no atoms" $ length b0.atoms == 0
  check "emptyBuilder has no bonds" $ length b0.bonds == 0
  check "addAtom: one atom after first add" $ length b1.atoms == 1
  check "addAtom: nextId advances past first id" $ b1.nextId > b0.nextId
  check "addAtom: two atoms after second add" $ length b2.atoms == 2
  check "addAtom: the two atoms have DISTINCT ids" $
    idOf b2 0 /= idOf b2 1 && isJust (idOf b2 0) && isJust (idOf b2 1)
  check "addAtom: first atom unchanged by second add" $
    idOf b2 0 == idOf b1 0 && posOf b2 0 == posOf b1 0
  check "addAtom: nextId advances again" $ b2.nextId > b1.nextId

  -- moveAtom updates only the targeted atom's pos and returns a new record.
  let
    id0 = fromMaybe (-1) (idOf b2 0)
    b2moved = B.moveAtom id0 posC b2
  check "moveAtom: updates only the targeted atom's pos" $
    posOf b2moved 0 == Just posC && posOf b2moved 1 == posOf b2 1
  check "moveAtom: count unchanged" $ length b2moved.atoms == length b2.atoms

  -- clear resets to the empty builder.
  check "clear returns emptyBuilder" $ B.clear b2 == B.emptyBuilder

  -- recomputeBonds: valence + bond/break thresholds + hysteresis.
  let
    -- Model constants (read, never hardcode the magnitudes).
    near = B.bondThreshold * 0.5 -- comfortably inside bonding range
    far = B.breakThreshold * 2.0 -- comfortably beyond breaking range
    mid = (B.bondThreshold + B.breakThreshold) / 2.0 -- hysteresis band

    -- Two H atoms within bondThreshold ⇒ exactly one bond.
    p0 = { x: 0.0, y: 0.0, z: 0.0 }
    p1 = { x: near, y: 0.0, z: 0.0 }
    twoH = B.addAtom 1 p1 (B.addAtom 1 p0 B.emptyBuilder)

    -- A 3rd H placed very close to a bonded (valence-full) H gains no 2nd bond.
    p2 = { x: near * 1.01, y: 0.0, z: 0.0 }
    threeH = B.addAtom 1 p2 twoH
    degreeOf st aid =
      length (filter (\bd -> bd.a == aid || bd.b == aid) st.bonds)

    -- One O with two H within bondThreshold ⇒ O has degree 2.
    oId = fromMaybe (-1) (map _.id (index ((B.addAtom 8 p0 B.emptyBuilder)).atoms 0))
    oWith2H =
      B.addAtom 1 { x: 0.0, y: near, z: 0.0 }
        ( B.addAtom 1 { x: near, y: 0.0, z: 0.0 }
            (B.addAtom 8 p0 B.emptyBuilder)
        )

  check "thresholds ordered: bondThreshold < breakThreshold" $
    B.bondThreshold < B.breakThreshold
  check "recomputeBonds: two close H ⇒ 1 bond" $ length twoH.bonds == 1
  check "recomputeBonds: valence-full H gains no 2nd bond (degree stays 1)" $
    let
      firstHId = fromMaybe (-1) (map _.id (index twoH.atoms 0))
    in
      degreeOf threeH firstHId == 1
  check "recomputeBonds: O + 2H ⇒ O degree 2" $
    degreeOf oWith2H oId == 2

  -- Breaking: move a bonded pair past breakThreshold ⇒ bond removed.
  let
    movedFarId = fromMaybe (-1) (map _.id (index twoH.atoms 1))
    broken = B.moveAtom movedFarId { x: far, y: 0.0, z: 0.0 } twoH
  check "recomputeBonds: pair beyond breakThreshold ⇒ bond removed" $
    length broken.bonds < length twoH.bonds

  -- Hysteresis: an already-bonded pair sitting at the mid distance STAYS bonded,
  -- but a FRESH (unbonded) pair at that same mid distance does NOT bond.
  let
    midMovedId = fromMaybe (-1) (map _.id (index twoH.atoms 1))
    midExisting = B.recomputeBonds (B.moveAtom midMovedId { x: mid, y: 0.0, z: 0.0 } twoH)
    midFresh =
      B.addAtom 1 { x: mid, y: 0.0, z: 0.0 } (B.addAtom 1 p0 B.emptyBuilder)
  check "hysteresis: existing bond at mid distance stays bonded" $
    length midExisting.bonds == 1
  check "hysteresis: fresh pair at mid distance does NOT bond" $
    length midFresh.bonds == 0

  -- molecules (connected components) + formulaOf (Unicode subscripts).
  let
    twoHComps = B.molecules twoH
    h2oComps = B.molecules oWith2H
    -- Two separate H₂ pairs, far apart from each other.
    pairA = B.addAtom 1 { x: near, y: 0.0, z: 0.0 } (B.addAtom 1 p0 B.emptyBuilder)
    twoPairs =
      B.addAtom 1 { x: far + near, y: 0.0, z: 0.0 }
        (B.addAtom 1 { x: far, y: 0.0, z: 0.0 } pairA)
    lone = B.addAtom 1 p0 B.emptyBuilder
    firstComp st = fromMaybe [] (index (B.molecules st) 0)

  check "molecules: two bonded H ⇒ 1 component of size 2" $
    length twoHComps == 1 && map length twoHComps == [ 2 ]
  check "formulaOf: bonded H₂ component ⇒ \"H₂\"" $
    B.formulaOf twoH (firstComp twoH) == "H₂"
  check "molecules: O + 2H bonded ⇒ 1 component of size 3" $
    length h2oComps == 1 && map length h2oComps == [ 3 ]
  check "formulaOf: H₂O component ⇒ \"H₂O\"" $
    B.formulaOf oWith2H (firstComp oWith2H) == "H₂O"
  check "molecules: two separate H₂ pairs ⇒ 2 components" $
    length (B.molecules twoPairs) == 2
  check "molecules: a lone atom ⇒ its own singleton component" $
    length (B.molecules lone) == 1 && map length (B.molecules lone) == [ 1 ]

  -- ───── Builder molecule-move: componentOf + moveMolecule (rigid) ─────
  log "builder molecule-move (componentOf + moveMolecule) properties:"
  let
    -- H₂O: O (id oId) bonded to two H ⇒ one 3-atom component.
    h2oAtomIds = map _.id oWith2H.atoms
    compOfO = B.componentOf oWith2H oId
    loneId = fromMaybe (-1) (map _.id (index lone.atoms 0))

    -- Move the whole H₂O molecule by dragging the O anchor to a new position.
    oOldPos = fromMaybe { x: 0.0, y: 0.0, z: 0.0 } (map _.pos (B.atomById oWith2H oId))
    target = { x: oOldPos.x + 500.0, y: oOldPos.y - 300.0, z: oOldPos.z + 120.0 }
    dx = target.x - oOldPos.x
    dy = target.y - oOldPos.y
    dz = target.z - oOldPos.z
    movedMol = B.moveMolecule oId target oWith2H

    posById st aid = map _.pos (B.atomById st aid)
    shiftedBy st0 st1 aid =
      case posById st0 aid, posById st1 aid of
        Just a, Just b ->
          approxEq (b.x - a.x) dx && approxEq (b.y - a.y) dy && approxEq (b.z - a.z) dz
        _, _ -> false

  -- componentOf returns the full connected component (all 3 ids of H₂O).
  check "componentOf: H₂O O-anchor ⇒ all 3 atom ids" $
    length compOfO == 3 && all (\i -> any (_ == i) compOfO) h2oAtomIds
  -- componentOf of a lone atom is just itself.
  check "componentOf: lone atom ⇒ singleton [itself]" $
    B.componentOf lone loneId == [ loneId ]
  -- The anchor lands exactly at the target.
  check "moveMolecule: anchor (O) lands at the target position" $
    case posById movedMol oId of
      Just p -> approxEq p.x target.x && approxEq p.y target.y && approxEq p.z target.z
      Nothing -> false
  -- EVERY atom in the component shifts by the SAME delta (rigid translation).
  check "moveMolecule: every component atom shifts by the same delta" $
    all (shiftedBy oWith2H movedMol) compOfO
  -- The atom count is preserved and the internal bonds survive (rigid move ⇒
  -- intra-component distances unchanged ⇒ same bond count).
  check "moveMolecule: atom count unchanged" $
    length movedMol.atoms == length oWith2H.atoms
  check "moveMolecule: internal bonds preserved (same bond count)" $
    length movedMol.bonds == length oWith2H.bonds
  -- A singleton component: moveMolecule == moveAtom (same resulting position).
  check "moveMolecule: singleton ⇒ same final pos as moveAtom" $
    let
      mm = B.moveMolecule loneId { x: 700.0, y: 0.0, z: 0.0 } lone
      ma = B.moveAtom loneId { x: 700.0, y: 0.0, z: 0.0 } lone
    in
      posById mm loneId == posById ma loneId
  -- moveAtom still moves ONLY the named atom (the other H₂O atoms stay put).
  check "moveAtom: moving O leaves the two H positions unchanged" $
    let
      movedO = B.moveAtom oId target oWith2H
      others = filter (_ /= oId) compOfO
      unchanged aid = posById movedO aid == posById oWith2H aid
    in
      all unchanged others

  log "all builder molecule-move properties hold."

  -- ───── Builder lone/bonding electrons (electron conservation) ────────
  let
    -- Lone fixtures (single, unbonded atoms).
    loneH = B.addAtom 1 p0 B.emptyBuilder
    loneC = B.addAtom 6 p0 B.emptyBuilder
    loneO = B.addAtom 8 p0 B.emptyBuilder
    -- Atom ids for the lone / bonded fixtures.
    loneHId = fromMaybe (-1) (map _.id (index loneH.atoms 0))
    loneCId = fromMaybe (-1) (map _.id (index loneC.atoms 0))
    loneOId = fromMaybe (-1) (map _.id (index loneO.atoms 0))
    -- The two bonded H from the twoH fixture (within bondThreshold ⇒ 1 bond).
    twoH_id0 = fromMaybe (-1) (map _.id (index twoH.atoms 0))
    twoH_id1 = fromMaybe (-1) (map _.id (index twoH.atoms 1))
    -- O + 2H all within range (H₂O): O has degree 2, each H degree 1.
    h2oOId = fromMaybe (-1) (map _.id (index oWith2H.atoms 0))
    h2oH1Id = fromMaybe (-1) (map _.id (index oWith2H.atoms 1))
    h2oH2Id = fromMaybe (-1) (map _.id (index oWith2H.atoms 2))

    -- Σ over atoms of Chem.valence z — the total electron budget the bonding +
    -- lone clouds must conserve. Computed from the state, never hardcoded.
    electronSum st = sum (map (\a -> a.z) st.atoms)
    -- Mean of an array of V3 (origin for empty).
    meanV ps =
      let
        n = max 1 (length ps)
        sx = sum (map _.x ps)
        sy = sum (map _.y ps)
        sz = sum (map _.z ps)
      in
        { x: sx / toNumber n, y: sy / toNumber n, z: sz / toNumber n }
    -- The single bond's midpoint in the twoH world, computed from the two atom
    -- centres (for symmetric H at ±x within bondThreshold this is ≈ origin).
    twoHMid = fromMaybe { x: 0.0, y: 0.0, z: 0.0 } (index (B.bondMidpoints twoH) 0)

  -- degreeOf: incident bond count for an atom id.
  check "degreeOf: two bonded H ⇒ each H degree 1" $
    B.degreeOf twoH twoH_id0 == 1 && B.degreeOf twoH twoH_id1 == 1
  check "degreeOf: a lone H ⇒ degree 0" $ B.degreeOf loneH loneHId == 0
  check "degreeOf: H₂O O ⇒ degree 2" $ B.degreeOf oWith2H h2oOId == 2

  -- loneCountOf: max 0 (valence z − degreeOf) for that id (0 if id unknown).
  check "loneCountOf: bonded H ⇒ 0 (electrons 1 − degree 1)" $
    B.loneCountOf twoH twoH_id0 == 0 && B.loneCountOf twoH twoH_id1 == 0
  check "loneCountOf: lone H ⇒ 1 (electrons 1 − degree 0)" $
    B.loneCountOf loneH loneHId == 1
  check "loneCountOf: lone Carbon ⇒ 6 (electrons 6 − degree 0)" $
    B.loneCountOf loneC loneCId == 6
  check "loneCountOf: lone Oxygen ⇒ 8 (electrons 8 − degree 0)" $
    B.loneCountOf loneO loneOId == 8
  check "loneCountOf: H₂O O ⇒ 6 (electrons 8 − degree 2)" $
    B.loneCountOf oWith2H h2oOId == 6
  check "loneCountOf: H₂O bonded H ⇒ 0 each" $
    B.loneCountOf oWith2H h2oH1Id == 0 && B.loneCountOf oWith2H h2oH2Id == 0
  check "loneCountOf: unknown id ⇒ 0" $ B.loneCountOf twoH (-999) == 0

  -- bondElectronPositions: 2 shared electrons per bond, mean ≈ bond midpoint,
  -- frame-animated; empty world ⇒ no shared electrons.
  check "bondElectronPositions: twoH (1 bond) ⇒ 2 electrons" $
    length (B.bondElectronPositions twoH 0.0) == 2
  check "bondElectronPositions: mean ≈ bond midpoint (twoH)" $
    let
      m = meanV (B.bondElectronPositions twoH 0.0)
    in
      approxEq m.x twoHMid.x && approxEq m.y twoHMid.y && approxEq m.z twoHMid.z
  check "bondElectronPositions: frame-animated (0 ≠ 60)" $
    B.bondElectronPositions twoH 0.0 /= B.bondElectronPositions twoH 60.0
  check "bondElectronPositions: empty world ⇒ 0" $
    length (B.bondElectronPositions B.emptyBuilder 0.0) == 0

  -- loneElectronPositions: loneCountOf electrons per atom; frame-animated and
  -- deterministic for a fixed frame.
  check "loneElectronPositions: lone Carbon ⇒ 6" $
    length (B.loneElectronPositions loneC 0.0) == 6
  check "loneElectronPositions: lone H ⇒ 1" $
    length (B.loneElectronPositions loneH 0.0) == 1
  check "loneElectronPositions: two bonded H ⇒ 0 (both lone counts 0)" $
    length (B.loneElectronPositions twoH 0.0) == 0
  check "loneElectronPositions: deterministic for a fixed frame" $
    B.loneElectronPositions loneC 12.0 == B.loneElectronPositions loneC 12.0

  -- ELECTRON CONSERVATION: bonding + lone electrons == Σ valence, for empty (0),
  -- two-H/H₂ (2), lone Carbon (6), and H₂O O+2H (10 = 4 shared + 6 O lone).
  check "conservation: empty ⇒ bond+lone == Σz (0)" $
    length (B.bondElectronPositions B.emptyBuilder 0.0)
      + length (B.loneElectronPositions B.emptyBuilder 0.0)
      == electronSum B.emptyBuilder
  check "conservation: two-H/H₂ ⇒ bond+lone == Σz (2)" $
    length (B.bondElectronPositions twoH 0.0)
      + length (B.loneElectronPositions twoH 0.0)
      == electronSum twoH
  check "conservation: lone Carbon ⇒ bond+lone == Σz (6)" $
    length (B.bondElectronPositions loneC 0.0)
      + length (B.loneElectronPositions loneC 0.0)
      == electronSum loneC
  check "conservation: H₂O O+2H ⇒ bond+lone == Σz (10)" $
    length (B.bondElectronPositions oWith2H 0.0)
      + length (B.loneElectronPositions oWith2H 0.0)
      == electronSum oWith2H

  -- ───── Builder core/valence lone-electron split (shells) ────────────
  let
    -- Centre of the lone fixtures (single atom placed at the origin p0).
    atomCentre = p0
    radialR cx cy p = sqrt ((p.x - cx) * (p.x - cx) + (p.y - cy) * (p.y - cy))

    -- Bonded Carbon: a Carbon with 4 H all within bondThreshold ⇒ degree 4.
    -- Place the 4 H around the central C, each comfortably inside bonding range
    -- (and spread on different axes so they don't over-bond to each other).
    cCentre = { x: 0.0, y: 0.0, z: 0.0 }
    boundC =
      B.addAtom 1 { x: 0.0, y: 0.0, z: -near }
        ( B.addAtom 1 { x: 0.0, y: 0.0, z: near }
            ( B.addAtom 1 { x: 0.0, y: near, z: 0.0 }
                ( B.addAtom 1 { x: near, y: 0.0, z: 0.0 }
                    (B.addAtom 6 cCentre B.emptyBuilder)
                )
            )
        )
    boundCId = fromMaybe (-1) (map _.id (index boundC.atoms 0))

  -- valenceShellOf == last (electronShells z): C→4, O→6, H→1, Ne→8.
  check "valenceShellOf C(6) = 4" $ B.valenceShellOf 6 == 4
  check "valenceShellOf O(8) = 6" $ B.valenceShellOf 8 == 6
  check "valenceShellOf H(1) = 1" $ B.valenceShellOf 1 == 1
  check "valenceShellOf Ne(10) = 8" $ B.valenceShellOf 10 == 8

  -- Free-atom split: a free atom has degree 0, so valenceLone == valenceShellOf z
  -- and coreLone == z − valenceShellOf z.
  check "free Carbon split ⇒ 2 core electrons" $
    length (B.coreLoneElectronPositions loneC 0.0) == 2
  check "free Carbon split ⇒ 4 valence electrons" $
    length (B.valenceLoneElectronPositions loneC 0.0) == 4
  check "free Oxygen split ⇒ 2 core electrons" $
    length (B.coreLoneElectronPositions loneO 0.0) == 2
  check "free Oxygen split ⇒ 6 valence electrons" $
    length (B.valenceLoneElectronPositions loneO 0.0) == 6
  check "free Hydrogen split ⇒ 0 core electrons" $
    length (B.coreLoneElectronPositions loneH 0.0) == 0
  check "free Hydrogen split ⇒ 1 valence electron" $
    length (B.valenceLoneElectronPositions loneH 0.0) == 1

  -- Bonded Carbon (degree 4): its 4 valence electrons went into bonds, so the
  -- remaining 2 lone are CORE (the 1s pair) — 0 valence lone.
  check "bonded Carbon has degree 4 (4 H within range)" $
    B.degreeOf boundC boundCId == 4
  check "bonded Carbon split ⇒ 2 core electrons" $
    length (B.coreLoneElectronPositions boundC 0.0) == 2
  check "bonded Carbon split ⇒ 0 valence electrons" $
    length (B.valenceLoneElectronPositions boundC 0.0) == 0

  -- Radius ordering: every valence electron is FARTHER from the atom centre than
  -- every core electron (valence ring radius strictly larger than any core ring).
  check "free Carbon: min valence radius > max core radius" $
    let
      coreRs = map (radialR atomCentre.x atomCentre.y) (B.coreLoneElectronPositions loneC 0.0)
      valRs = map (radialR atomCentre.x atomCentre.y) (B.valenceLoneElectronPositions loneC 0.0)
    in
      fromMaybe 0.0 (minimum valRs) > fromMaybe 0.0 (maximum coreRs)

  -- Conservation across the split: core + valence + bonding == Σ z over atoms.
  check "split conservation: free Carbon ⇒ core+val+bond == Σz (6 = 2+4+0)" $
    length (B.coreLoneElectronPositions loneC 0.0)
      + length (B.valenceLoneElectronPositions loneC 0.0)
      + length (B.bondElectronPositions loneC 0.0)
      == electronSum loneC
  check "split conservation: two-H/H₂ ⇒ core+val+bond == Σz (2 = 0+0+2)" $
    length (B.coreLoneElectronPositions twoH 0.0)
      + length (B.valenceLoneElectronPositions twoH 0.0)
      + length (B.bondElectronPositions twoH 0.0)
      == electronSum twoH

  -- Split equals whole: core <> valence == loneElectronPositions (same array,
  -- since the implementer defines lone = core <> valence).
  check "split equals whole: core <> valence == loneElectronPositions (free Carbon)" $
    B.coreLoneElectronPositions loneC 0.0 <> B.valenceLoneElectronPositions loneC 0.0
      == B.loneElectronPositions loneC 0.0

  -- pick / unproject round-trip.
  let
    canvas = { w: 800.0, h: 600.0 }
    proj = testProjection canvas.w canvas.h
    refPos = { x: 40.0, y: -25.0, z: 30.0 } -- in front of the camera
    px = B.projectToScreen proj canvas refPos
    back = B.unprojectAtDepth proj canvas { x: px.x, y: px.y } refPos
    closeV a c = approxClose a.x c.x && approxClose a.y c.y && approxClose a.z c.z
    approxClose a c = abs (a - c) < 1.0e-3
  check "projectToScreen: pixel lies within the canvas bounds" $
    px.x >= 0.0 && px.x <= canvas.w && px.y >= 0.0 && px.y <= canvas.h
  check "pick round-trip: unproject(project(pos)) ≈ pos at same depth" $
    closeV back refPos

  log "all builder + chem properties hold."

  -- ───── Builder atom visuals: atomicRadius / symbolOf / bondSegments ──
  log "Builder atom visuals (atomicRadius / symbolOf / bondSegments) properties:"

  -- atomicRadius: positive for several Z.
  check "atomicRadius 1 > 0" $ Atom.atomicRadius 1 > 0.0
  check "atomicRadius 6 > 0" $ Atom.atomicRadius 6 > 0.0
  check "atomicRadius 8 > 0" $ Atom.atomicRadius 8 > 0.0
  check "atomicRadius 36 > 0" $ Atom.atomicRadius 36 > 0.0

  -- atomicRadius: element-varying, Hydrogen is the smallest (H < C, H < O).
  check "atomicRadius: Hydrogen < Carbon (H < C)" $
    Atom.atomicRadius 1 < Atom.atomicRadius 6
  check "atomicRadius: Hydrogen < Oxygen (H < O)" $
    Atom.atomicRadius 1 < Atom.atomicRadius 8

  -- atomicRadius: clamp-safe (Z<1 clamps to 1, Z>36 clamps to 36).
  check "atomicRadius: Z 0 clamps to Z 1" $
    Atom.atomicRadius 0 == Atom.atomicRadius 1
  check "atomicRadius: Z 999 clamps to Z 36" $
    Atom.atomicRadius 999 == Atom.atomicRadius 36

  -- symbolOf: element symbol lookup.
  check "symbolOf 1 == H" $ Atom.symbolOf 1 == "H"
  check "symbolOf 6 == C" $ Atom.symbolOf 6 == "C"
  check "symbolOf 8 == O" $ Atom.symbolOf 8 == "O"

  -- symbolOf: clamp-safe (never crashes at the edges).
  check "symbolOf: Z 0 clamps to Z 1" $ Atom.symbolOf 0 == Atom.symbolOf 1
  check "symbolOf: Z 999 clamps to Z 36" $ Atom.symbolOf 999 == Atom.symbolOf 36

  -- bondSegments: one segment per bond, endpoints = the bonded atoms' centres.
  let
    segNear = B.bondThreshold * 0.5
    segP0 = { x: 0.0, y: 0.0, z: 0.0 }
    segP1 = { x: segNear, y: 0.0, z: 0.0 }
    -- Two H within bondThreshold ⇒ exactly one bond (same shape as the twoH
    -- fixture used by the existing Builder tests).
    segTwoH = B.addAtom 1 segP1 (B.addAtom 1 segP0 B.emptyBuilder)
    segs = B.bondSegments segTwoH
    -- Two H far apart ⇒ no bond ⇒ no segments.
    segFar = { x: B.breakThreshold * 2.0, y: 0.0, z: 0.0 }
    segNoBond = B.addAtom 1 segFar (B.addAtom 1 segP0 B.emptyBuilder)
    -- The two atom centres of the bonded fixture.
    segCentre0 = (fromMaybe { id: -1, z: 0, pos: { x: 0.0, y: 0.0, z: 0.0 } } (index segTwoH.atoms 0)).pos
    segCentre1 = (fromMaybe { id: -1, z: 0, pos: { x: 0.0, y: 0.0, z: 0.0 } } (index segTwoH.atoms 1)).pos
    vEq u v = approxEq u.x v.x && approxEq u.y v.y && approxEq u.z v.z
    -- Order-independent: {seg.a, seg.b} equals {centre0, centre1}.
    segMatches s =
      (vEq s.a segCentre0 && vEq s.b segCentre1)
        || (vEq s.a segCentre1 && vEq s.b segCentre0)

  check "bondSegments: one segment per bond (length == bonds)" $
    length segs == length segTwoH.bonds
  check "bondSegments: bonded fixture has ≥1 bond" $
    length segTwoH.bonds >= 1
  check "bondSegments: the single segment's endpoints are the two atom centres" $
    length segs == 1 && all segMatches segs
  check "bondSegments: empty builder ⇒ no segments" $
    length (B.bondSegments B.emptyBuilder) == 0
  check "bondSegments: two atoms with no bond ⇒ no segments" $
    length (B.bondSegments segNoBond) == 0

  log "all Builder atom visuals properties hold."
