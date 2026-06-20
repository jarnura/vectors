module Test.AtomSpec where

import Prelude

import Data.Array (all, any, concat, index, length, mapWithIndex, nub, range, zipWith)
import Data.Foldable (maximum, minimum, sum)
import Data.Maybe (fromMaybe, isNothing)
import Data.Number (pi, sqrt)
import Effect (Effect)
import Effect.Console (log)
import FRP.Loop (emptyInput)
import Atom (clampElectron, configString, electronPositions, electronPositionsByShell, electronPositionsByShell2D, electronPositionsBySubshell, electronPositionsBySubshell2D, electronShells, elementName, elementOf, fillSubshells, nucleusRadius, nucleons, shellRadius, shellRings, subshellCap, subshellInclination, subshellRadius)
import Meshes (orbitRing, orbitRingFlat)
import Test.Util (approxEq, check, epsilon)

atomSpec :: Effect Unit
atomSpec = do
  -- ───── Atom model + nucleus (atomos M3) ─────────────────────────────
  log "atom model properties:"

  let
    cEl = elementOf 6
    oEl = elementOf 8
    heEl = elementOf 2
    hEl = elementOf 1

  -- Element table: protons = Z, neutrons (common isotope), electrons = Z.
  check "Carbon = 6p 6n 6e" $ cEl.protons == 6 && cEl.neutrons == 6 && cEl.electrons == 6
  check "Oxygen = 8p 8n 8e" $ oEl.protons == 8 && oEl.neutrons == 8 && oEl.electrons == 8
  check "Helium = 2p 2n 2e" $ heEl.protons == 2 && heEl.neutrons == 2 && heEl.electrons == 2
  check "Hydrogen = 1p 0n 1e" $ hEl.protons == 1 && hEl.neutrons == 0 && hEl.electrons == 1

  -- Electron shells fill 2, 8, 8, …
  check "Carbon shells = [2,4]" $ electronShells 6 == [ 2, 4 ]
  check "Oxygen shells = [2,6]" $ electronShells 8 == [ 2, 6 ]
  check "Helium shells = [2]" $ electronShells 2 == [ 2 ]
  check "shells sum to electron count (C)" $ sum (electronShells 6) == 6

  -- Out-of-range Z clamps into the supported table (no crash).
  check "Z is clamped to a valid element" $ (elementOf 999).protons >= 1

  -- The nucleus is a cluster of protons+neutrons spheres within the nucleus radius.
  let cNuc = nucleons cEl
  check "Carbon nucleus has 12 nucleons" $ length cNuc == 12
  check "nucleons sit within the nucleus radius" $
    all (\n -> sqrt (n.pos.x * n.pos.x + n.pos.y * n.pos.y + n.pos.z * n.pos.z) <= nucleusRadius + epsilon) cNuc

  log "all atom model properties hold."

  -- ───── Sub-shell (orbital) model + Madelung filling (subshells M1) ───
  log "subshell model properties:"

  -- Subshell capacities follow 4ℓ+2: s=2, p=6, d=10, f=14, g=18.
  check "subshellCap s/p/d/f/g = 2/6/10/14/18" $
    subshellCap 0 == 2
      && subshellCap 1 == 6
      && subshellCap 2 == 10
      && subshellCap 3 == 14
      && subshellCap 4 == 18

  -- Per-shell totals are DERIVED from a Madelung-ordered subshell fill: low-Z
  -- anchors are unchanged, but transition metals diverge (4s fills before 3d,
  -- yet 3d's electrons belong to shell n=3 — Sc is [2,8,9,2], not [2,8,8,3]).
  check "Helium shells = [2]" $ electronShells 2 == [ 2 ]
  check "Neon shells = [2,8]" $ electronShells 10 == [ 2, 8 ]
  check "Argon shells = [2,8,8]" $ electronShells 18 == [ 2, 8, 8 ]
  check "Potassium (19) shells = [2,8,8,1]" $ electronShells 19 == [ 2, 8, 8, 1 ]
  check "Scandium (21) shells = [2,8,9,2]" $ electronShells 21 == [ 2, 8, 9, 2 ]
  check "Iron (26) shells = [2,8,14,2]" $ electronShells 26 == [ 2, 8, 14, 2 ]
  check "Zinc (30) shells = [2,8,18,2]" $ electronShells 30 == [ 2, 8, 18, 2 ]
  check "Krypton (36) shells = [2,8,18,8]" $ electronShells 36 == [ 2, 8, 18, 8 ]

  -- Shells always sum to the (clamped) electron count.
  check "shells sum to Z (Fe 26)" $ sum (electronShells 26) == 26
  check "shells sum to Z (Kr 36)" $ sum (electronShells 36) == 36
  check "shells sum clamps low (0 → 1)" $ sum (electronShells 0) == 1
  check "shells sum clamps high (999 → 36)" $ sum (electronShells 999) == 36

  -- Every filled subshell respects its 4ℓ+2 cap (no overfilling).
  check "Kr subshells respect 4ℓ+2 caps" $
    all (\ss -> ss.count <= subshellCap ss.l && ss.count > 0) (fillSubshells 36)

  -- fillSubshells is clamp-safe and total at the edges.
  check "fillSubshells 0 is empty" $ length (fillSubshells 0) == 0
  check "fillSubshells totals to clamped Z (Kr)" $
    sum (map _.count (fillSubshells 36)) == 36

  -- Human-readable configuration string in standard (n,l)-sorted order. This is
  -- the exact text payload shown by the atomos orbital-info overlay.
  check "configString 1 = Hydrogen config" $ configString 1 == "1s1"
  check "configString 6 = Carbon config" $ configString 6 == "1s2 2s2 2p2"
  check "configString 36 = Krypton config" $
    configString 36 == "1s2 2s2 2p6 3s2 3p6 3d10 4s2 4p6"

  log "all subshell model properties hold."

  -- ───── Electrons / Bohr orbits (atomos M4) ──────────────────────────
  log "electron orbit properties:"

  let
    el0 = electronPositions cEl 0.0
    el1 = electronPositions cEl 60.0
    dist p = sqrt (p.x * p.x + p.y * p.y + p.z * p.z)
    e0 = fromMaybe { x: 0.0, y: 0.0, z: 0.0 } (index el0 0)
    e0' = fromMaybe { x: 0.0, y: 0.0, z: 0.0 } (index el1 0)

  -- One electron per electron in the atom (Carbon → 6).
  check "electron count = Z (Carbon 6)" $ length el0 == 6

  -- Shells get strictly larger radii outward (shellRadius unchanged).
  check "shell radii strictly increase" $
    shellRadius 0 < shellRadius 1 && shellRadius 1 < shellRadius 2

  -- The orbit advances with the frame (electrons move).
  check "electron orbit advances with frame" $
    not (approxEq e0.x e0'.x && approxEq e0.z e0'.z)

  log "all electron orbit properties hold."

  -- ───── Distinct sub-shell orbital rendering (subshells M3) ──────────
  log "subshell orbital rendering properties:"

  let
    -- Every distance an electron of element `z` may legitimately ride on: the
    -- radius of one of its filled subshells.
    subRadii z = map (\ss -> subshellRadius ss.n ss.l) (fillSubshells (elementOf z).electrons)
    onSomeSubRadius z p = any (\r -> approxEq (dist p) r) (subRadii z)
    feEl = elementOf 26
    krEl = elementOf 36
    fePos = electronPositions feEl 0.0
    krPos = electronPositions krEl 0.0

  -- Electron count equals Z across the extended range.
  check "electron count = Z (Iron 26)" $ length fePos == 26
  check "electron count = Z (Krypton 36)" $ length krPos == 36

  -- Sub-shell radii grow with the principal shell n.
  check "subshell radii grow with n (s subshells)" $
    subshellRadius 1 0 < subshellRadius 2 0 && subshellRadius 2 0 < subshellRadius 3 0

  -- Within a shell, distinct subshells (s/p/d) ride distinct radii — orbitals
  -- are visually separated, not collapsed onto one ring.
  check "subshells within a shell are separated (3s<3p<3d)" $
    subshellRadius 3 0 < subshellRadius 3 1 && subshellRadius 3 1 < subshellRadius 3 2

  -- Krypton's filled subshells all ride distinct radii (no two coincide).
  check "Kr filled subshells have distinct radii" $
    let rs = subRadii 36 in length (nub rs) == length rs

  -- Every electron lands exactly on one of its element's subshell radii.
  check "every Carbon electron lies on a subshell radius" $ all (onSomeSubRadius 6) el0
  check "every Krypton electron lies on a subshell radius" $ all (onSomeSubRadius 36) krPos

  log "all subshell orbital rendering properties hold."

  -- ───── Orbital ring lines (orbital-lines M1) ────────────────────────
  log "orbital ring line properties:"

  -- Inclination is a shared, pure function of (n, l): the SAME tilt the
  -- electrons ride on, so the ring line traces the electron path exactly.
  check "subshellInclination 1 0 = pi/5" $
    approxEq (subshellInclination 1 0) (pi / 5.0)
  check "subshellInclination 3 2 = 3·pi/5 + 2·pi/9" $
    approxEq (subshellInclination 3 2) (3.0 * (pi / 5.0) + 2.0 * (pi / 9.0))

  let
    ringSeg = 64
    ringR = subshellRadius 2 1
    ringIncl = subshellInclination 2 1
    ring = orbitRing ringSeg ringR ringIncl
    vAt j = fromMaybe 0.0 (index ring.vertices j)
    vDist j = sqrt (vAt (3 * j) * vAt (3 * j) + vAt (3 * j + 1) * vAt (3 * j + 1) + vAt (3 * j + 2) * vAt (3 * j + 2))

  -- A ring is `segments` vertices (3 floats each) drawn as a closed GL_LINES loop.
  check "orbitRing has `segments` vertices" $ length ring.vertices == ringSeg * 3
  check "orbitRing is a closed loop (2·segments indices)" $ length ring.indices == 2 * ringSeg

  -- Every ring vertex lies exactly on the subshell radius.
  check "every ring vertex lies on the subshell radius" $
    all (\j -> approxEq (vDist j) ringR) (range 0 (ringSeg - 1))

  -- The ring's first vertex (θ=0) is {r, 0, 0} — the electron-path start.
  check "ring vertex 0 is {r,0,0}" $
    approxEq (vAt 0) ringR && approxEq (vAt 1) 0.0 && approxEq (vAt 2) 0.0

  log "all orbital ring line properties hold."

  -- ───── Electron positions grouped by sub-shell (colours M1) ─────────
  log "grouped electron position properties:"

  let
    cElc = elementOf 6
    groups0 = electronPositionsBySubshell cElc 0.0

  -- The grouped positions flatten back to the original flat positions.
  check "concat of sub-shell groups == electronPositions (Carbon)" $
    concat groups0 == electronPositions cElc 0.0

  -- One group per filled sub-shell, each holding that sub-shell's electrons.
  check "group count == number of filled sub-shells (Carbon)" $
    length groups0 == length (fillSubshells cElc.electrons)
  check "group lengths == sub-shell electron counts (Carbon)" $
    map length groups0 == map _.count (fillSubshells cElc.electrons)

  -- Clamp-safe for the full range.
  check "grouped positions total to Z (Krypton 36)" $
    length (concat (electronPositionsBySubshell (elementOf 36) 0.0)) == 36

  log "all grouped electron position properties hold."

  -- ───── atomos 2D flat geometry (flat-2d M1) ─────────────────────────
  log "atomos 2D flat geometry properties:"

  let
    cFlat = elementOf 6
    krFlat = elementOf 36
    flatDist p = sqrt (p.x * p.x + p.y * p.y)
    -- group radius == subshellRadius of the i-th filled sub-shell
    flatGroups el f = electronPositionsBySubshell2D el f
    flatAll el f = concat (flatGroups el f)
    subshellsOf el = fillSubshells el.electrons

  -- Group count == number of filled sub-shells (same grouping as 3D variant).
  check "2D group count == filled sub-shells (Carbon)" $
    length (flatGroups cFlat 0.0) == length (subshellsOf cFlat)
  check "2D group count == filled sub-shells (Krypton)" $
    length (flatGroups krFlat 0.0) == length (subshellsOf krFlat)

  -- Each group holds that sub-shell's electron count.
  check "2D group lengths == sub-shell counts (Carbon)" $
    map length (flatGroups cFlat 0.0) == map _.count (subshellsOf cFlat)
  check "2D group lengths == sub-shell counts (Krypton)" $
    map length (flatGroups krFlat 0.0) == map _.count (subshellsOf krFlat)

  -- Total electrons == Z.
  check "2D positions total to Z (Krypton 36)" $
    length (flatAll krFlat 0.0) == 36

  -- Flat: every position sits in the XY plane (z ≈ 0).
  check "2D Carbon electrons are flat (z≈0)" $
    all (\p -> approxEq p.z 0.0) (flatAll cFlat 0.0)
  check "2D Krypton electrons are flat (z≈0)" $
    all (\p -> approxEq p.z 0.0) (flatAll krFlat 60.0)

  -- Every electron rides on its sub-shell radius (per group, all at the same r,
  -- and r equals the corresponding sub-shell radius from fillSubshells).
  check "2D groups ride their sub-shell radius (Carbon)" $
    let
      gs = flatGroups cFlat 0.0
      sss = subshellsOf cFlat
    in
      all identity $ mapWithIndex
        ( \i grp ->
            let
              r = subshellRadius (fromMaybe { n: 0, l: 0, label: "", count: 0 } (index sss i)).n
                (fromMaybe { n: 0, l: 0, label: "", count: 0 } (index sss i)).l
            in
              all (\p -> approxEq (flatDist p) r) grp
        )
        gs
  check "2D groups ride their sub-shell radius (Krypton)" $
    let
      gs = flatGroups krFlat 0.0
      sss = subshellsOf krFlat
    in
      all identity $ mapWithIndex
        ( \i grp ->
            let
              r = subshellRadius (fromMaybe { n: 0, l: 0, label: "", count: 0 } (index sss i)).n
                (fromMaybe { n: 0, l: 0, label: "", count: 0 } (index sss i)).l
            in
              all (\p -> approxEq (flatDist p) r) grp
        )
        gs

  -- The 2D positions DIFFER from the inclined 3D positions: pick an outer
  -- sub-shell (nonzero inclination) and assert a y-coordinate differs.
  check "2D positions differ from 3D (some y differs)" $
    let
      d3 = electronPositions krFlat 0.0
      d2 = flatAll krFlat 0.0
    in
      any identity $ zipWith (\a b -> not (approxEq a.y b.y)) d2 d3

  -- The orbit advances with the frame.
  check "2D orbit advances with frame" $
    flatAll krFlat 0.0 /= flatAll krFlat 60.0

  log "all atomos 2D flat geometry properties hold."

  -- ───── Flat orbital ring lines (flat-2d M2) ─────────────────────────
  log "flat orbital ring line properties:"

  let
    fRingSeg = 64
    fRingR = subshellRadius 2 1
    fRing = orbitRingFlat fRingSeg fRingR
    fvAt j = fromMaybe 0.0 (index fRing.vertices j)
    fvDist j = sqrt (fvAt (3 * j) * fvAt (3 * j) + fvAt (3 * j + 1) * fvAt (3 * j + 1))
    fvZ j = fvAt (3 * j + 2)

  check "orbitRingFlat has `segments` vertices" $ length fRing.vertices == fRingSeg * 3
  check "orbitRingFlat is a closed loop (2·segments indices)" $
    length fRing.indices == 2 * fRingSeg
  check "every flat ring vertex is in the XY plane (z≈0)" $
    all (\j -> approxEq (fvZ j) 0.0) (range 0 (fRingSeg - 1))
  check "every flat ring vertex lies on radius r" $
    all (\j -> approxEq (fvDist j) fRingR) (range 0 (fRingSeg - 1))
  check "flat ring vertex 0 is {r,0,0}" $
    approxEq (fvAt 0) fRingR && approxEq (fvAt 1) 0.0 && approxEq (fvAt 2) 0.0

  log "all flat orbital ring line properties hold."

  -- ───── Element selector input (atomos M5) ───────────────────────────
  log "element input properties:"

  -- The Input has an element channel, empty until the selector is used.
  check "emptyInput.element is Nothing" $ isNothing emptyInput.element

  -- Selecting a different Z yields a different atom (pure mapping).
  check "selecting O (8) differs from C (6)" $
    (elementOf 8).protons /= (elementOf 6).protons

  log "all element input properties hold."

  -- ───── Element names for overlay label (overlay-text M1) ────────────
  log "element name properties:"

  check "elementName 1 = Hydrogen" $ elementName 1 == "Hydrogen"
  check "elementName 2 = Helium" $ elementName 2 == "Helium"
  check "elementName 6 = Carbon" $ elementName 6 == "Carbon"
  check "elementName 8 = Oxygen" $ elementName 8 == "Oxygen"

  log "all element name properties hold."

  -- ───── Element table extended to Krypton, Z=1..36 (subshells M2) ─────
  log "extended element table properties:"

  -- Names across the new range (period 2/3/4 anchors up to Krypton).
  check "elementName 10 = Neon" $ elementName 10 == "Neon"
  check "elementName 18 = Argon" $ elementName 18 == "Argon"
  check "elementName 19 = Potassium" $ elementName 19 == "Potassium"
  check "elementName 26 = Iron" $ elementName 26 == "Iron"
  check "elementName 36 = Krypton" $ elementName 36 == "Krypton"

  -- Symbols come from the same table (via elementOf).
  check "symbol 19 = K" $ (elementOf 19).symbol == "K"
  check "symbol 26 = Fe" $ (elementOf 26).symbol == "Fe"
  check "symbol 36 = Kr" $ (elementOf 36).symbol == "Kr"

  -- Proton/electron count = Z across the new range; neutrons anchored.
  check "Iron = 26p 30n 26e" $
    let fe = elementOf 26 in fe.protons == 26 && fe.neutrons == 30 && fe.electrons == 26
  check "Krypton = 36p 48n 36e" $
    let kr = elementOf 36 in kr.protons == 36 && kr.neutrons == 48 && kr.electrons == 36

  -- Clamp now spans 1..36: low clamps to Hydrogen, high clamps to Krypton.
  check "clampZ low: elementOf 0 → Hydrogen" $
    (elementOf 0).protons == 1 && elementName 0 == "Hydrogen"
  check "clampZ high: elementName 999 → Krypton" $ elementName 999 == "Krypton"
  check "clampZ high: elementOf 999 → 36 protons" $ (elementOf 999).protons == 36

  log "all extended element table properties hold."

  -- ───── Atom shell-collapsed positions (atomos shell/sub-shell toggle M1) ─
  log "atom shell-collapsed position properties:"

  let
    -- Elements under test (all via elementOf so clamping is exercised).
    cSh = elementOf 6 -- Carbon  Z=6  shells=[2,4]   => 2 groups
    oSh = elementOf 8 -- Oxygen  Z=8  shells=[2,6]   => 2 groups
    neSh = elementOf 10 -- Neon    Z=10 shells=[2,8]   => 2 groups
    krSh = elementOf 36 -- Krypton Z=36 shells=[2,8,18,8] => 4 groups

    -- Flatten a grouped array to a flat count.
    totalEls gs = sum (map length gs)

    -- Per-element: total in shell-collapsed grouping (3D and 2D).
    cShTotal3D = totalEls (electronPositionsByShell cSh 0.0)
    oShTotal3D = totalEls (electronPositionsByShell oSh 0.0)
    neShTotal3D = totalEls (electronPositionsByShell neSh 0.0)
    krShTotal3D = totalEls (electronPositionsByShell krSh 0.0)

    cShTotal2D = totalEls (electronPositionsByShell2D cSh 0.0)
    oShTotal2D = totalEls (electronPositionsByShell2D oSh 0.0)
    neShTotal2D = totalEls (electronPositionsByShell2D neSh 0.0)
    krShTotal2D = totalEls (electronPositionsByShell2D krSh 0.0)

    -- Reference totals from the existing sub-shell grouping (already tested green).
    cSubTotal = totalEls (electronPositionsBySubshell cSh 0.0)
    oSubTotal = totalEls (electronPositionsBySubshell oSh 0.0)
    neSubTotal = totalEls (electronPositionsBySubshell neSh 0.0)
    krSubTotal = totalEls (electronPositionsBySubshell krSh 0.0)

  -- 1. Electron-count conservation (3D): total == clampElectron z for each element.
  check "shellCollapsed 3D: Carbon total == clampElectron 6" $
    cShTotal3D == clampElectron 6
  check "shellCollapsed 3D: Oxygen total == clampElectron 8" $
    oShTotal3D == clampElectron 8
  check "shellCollapsed 3D: Neon total == clampElectron 10" $
    neShTotal3D == clampElectron 10
  check "shellCollapsed 3D: Krypton total == clampElectron 36" $
    krShTotal3D == clampElectron 36

  -- 1b. Total matches the sub-shell grouping total (both represent same electrons).
  check "shellCollapsed 3D: Carbon total == subshell total" $
    cShTotal3D == cSubTotal
  check "shellCollapsed 3D: Oxygen total == subshell total" $
    oShTotal3D == oSubTotal
  check "shellCollapsed 3D: Neon total == subshell total" $
    neShTotal3D == neSubTotal
  check "shellCollapsed 3D: Krypton total == subshell total" $
    krShTotal3D == krSubTotal

  -- 1c. Electron-count conservation (2D): same totals as 3D and clampElectron.
  check "shellCollapsed 2D: Carbon total == clampElectron 6" $
    cShTotal2D == clampElectron 6
  check "shellCollapsed 2D: Oxygen total == clampElectron 8" $
    oShTotal2D == clampElectron 8
  check "shellCollapsed 2D: Neon total == clampElectron 10" $
    neShTotal2D == clampElectron 10
  check "shellCollapsed 2D: Krypton total == clampElectron 36" $
    krShTotal2D == clampElectron 36

  -- 2. Group count == length (electronShells z) for each element (one group per
  --    occupied principal shell).
  check "shellCollapsed 3D: group count == length electronShells (Carbon)" $
    length (electronPositionsByShell cSh 0.0) == length (electronShells 6)
  check "shellCollapsed 3D: group count == length electronShells (Oxygen)" $
    length (electronPositionsByShell oSh 0.0) == length (electronShells 8)
  check "shellCollapsed 3D: group count == length electronShells (Neon)" $
    length (electronPositionsByShell neSh 0.0) == length (electronShells 10)
  check "shellCollapsed 3D: group count == length electronShells (Krypton)" $
    length (electronPositionsByShell krSh 0.0) == length (electronShells 36)

  check "shellCollapsed 2D: group count == length electronShells (Carbon)" $
    length (electronPositionsByShell2D cSh 0.0) == length (electronShells 6)
  check "shellCollapsed 2D: group count == length electronShells (Krypton)" $
    length (electronPositionsByShell2D krSh 0.0) == length (electronShells 36)

  -- 3. Shell-only group count <= sub-shell group count; Carbon strictly fewer.
  check "shellCollapsed: shell group count <= subshell group count (Carbon)" $
    length (electronPositionsByShell cSh 0.0) <= length (electronPositionsBySubshell cSh 0.0)
  check "shellCollapsed: Carbon strictly fewer shell groups than sub-shell groups (2 < 3)" $
    length (electronPositionsByShell cSh 0.0) == 2
      && length (electronPositionsBySubshell cSh 0.0) == 3
  check "shellCollapsed: Krypton shell groups <= subshell groups (4 <= 8)" $
    length (electronPositionsByShell krSh 0.0) <= length (electronPositionsBySubshell krSh 0.0)

  -- 4. shellRings for Carbon: 2 entries with strictly increasing radii, each
  --    radius == shellRadius (n - 1) for its n.
  let
    cRings = shellRings cSh
    cRingCount = length cRings
    cRing0 = fromMaybe { n: 0, radius: 0.0 } (index cRings 0)
    cRing1 = fromMaybe { n: 0, radius: 0.0 } (index cRings 1)

  check "shellRings: Carbon has 2 entries (K and L shells)" $
    cRingCount == 2
  check "shellRings: Carbon entry 0 n == 1 (K shell)" $
    cRing0.n == 1
  check "shellRings: Carbon entry 1 n == 2 (L shell)" $
    cRing1.n == 2
  check "shellRings: Carbon entry 0 radius == shellRadius 0" $
    approxEq cRing0.radius (shellRadius 0)
  check "shellRings: Carbon entry 1 radius == shellRadius 1" $
    approxEq cRing1.radius (shellRadius 1)
  check "shellRings: Carbon radii strictly increasing" $
    cRing0.radius < cRing1.radius

  -- 4b. shellRings for Krypton: 4 entries, all radii == shellRadius (n-1).
  let
    krRings = shellRings krSh

  check "shellRings: Krypton has 4 entries" $
    length krRings == 4
  check "shellRings: Krypton entry radii match shellRadius (n-1)" $
    all (\r -> approxEq r.radius (shellRadius (r.n - 1))) krRings

  -- 5. electronPositionsByShell2D Carbon: z ≈ 0 for every electron, and the
  --    per-group radial distance from origin equals shellRadius (n-1) where n is
  --    the group's principal shell index (1-based).
  let
    cFlat2DSh = electronPositionsByShell2D cSh 0.0
    allCFlat2DSh = concat cFlat2DSh
    xyDist p = sqrt (p.x * p.x + p.y * p.y)

    -- The groups correspond to electronShells 6 = [2,4].  Group index 0 → n=1,
    -- group index 1 → n=2.  Each electron in group i should ride shellRadius (i).
    cFlat2DShGroup0 = fromMaybe [] (index cFlat2DSh 0)
    cFlat2DShGroup1 = fromMaybe [] (index cFlat2DSh 1)

  check "shellCollapsed 2D: every Carbon electron has z ≈ 0" $
    all (\p -> approxEq p.z 0.0) allCFlat2DSh
  check "shellCollapsed 2D: Carbon group 0 (n=1) rides shellRadius 0" $
    all (\p -> approxEq (xyDist p) (shellRadius 0)) cFlat2DShGroup0
  check "shellCollapsed 2D: Carbon group 1 (n=2) rides shellRadius 1" $
    all (\p -> approxEq (xyDist p) (shellRadius 1)) cFlat2DShGroup1

  log "all atom shell-collapsed position properties hold."
