module Test.Main where

import Prelude

import Data.Array (all, any, concat, filter, find, index, length, mapWithIndex, nub, range, zipWith)
import Data.Foldable (foldl, maximum, minimum, sum)
import Data.Int (toNumber)
import Data.Maybe (Maybe(..), fromMaybe, isJust, isNothing)
import FRP.Loop (emptyInput)
import Data.Number (abs, pi, sqrt, tan)
import Effect (Effect)
import Effect.Console (log)
import Effect.Exception (throw)
import Math.Matrix as M

import Main (applyDragStrength, applySubshellView, applyValenceOnly, initialState)
import Atom (clampElectron, configString, electronPositions, electronPositionsByShell, electronPositionsByShell2D, electronPositionsBySubshell, electronPositionsBySubshell2D, electronShells, elementName, elementOf, fillSubshells, nucleusRadius, nucleons, shellRadius, shellRings, subshellCap, subshellInclination, subshellRadius)
import Atom as Atom
import Chem (bondEnergy, valence)
import Builder as B
import Camera as Cam
import Layer as Layer
import Molecule (bondLength, moleculeOf, molecules, moleculeNucleons, sharedElectronPositions)
import Palette (shellColor, subshellColor)
import Meshes (groundPlane, gridFloor, orbitRing, orbitRingFlat, sphere)
import Scene (Scene(..), nextScene, sceneTitle)
import Starfield (starPositions)
import Vector (rotateX, rotateY, rotateZ)
import World (groundTransform, groundY, groundExtent, gridDivisions, skyColor)

-- Tolerance for floating-point matrix equality.
-- sin/cos roundtrip at 360° produces error on the order of 1e-15;
-- 1e-10 leaves comfortable headroom for chained multiplications.
epsilon :: Number
epsilon = 1.0e-10

approxEq :: Number -> Number -> Boolean
approxEq a b = abs (a - b) < epsilon

approxEqMatrix :: M.Matrix Number -> M.Matrix Number -> Boolean
approxEqMatrix m1 m2 =
  let
    v1 = M.toVector m1
    v2 = M.toVector m2
  in
    all identity (zipWith approxEq v1 v2)

identity4 :: M.Matrix Number
identity4 = fromMaybe (M.zeros 4 4) $ M.fromArray 4 4
  [ 1.0
  , 0.0
  , 0.0
  , 0.0
  , 0.0
  , 1.0
  , 0.0
  , 0.0
  , 0.0
  , 0.0
  , 1.0
  , 0.0
  , 0.0
  , 0.0
  , 0.0
  , 1.0
  ]

check :: String -> Boolean -> Effect Unit
check name cond =
  if cond then log $ "  ok  " <> name
  else throw $ "FAIL " <> name

main :: Effect Unit
main = do
  log "rotation matrix properties:"

  -- Zero rotation is identity
  check "rotateX 0° = I" $ approxEqMatrix (rotateX 0.0) identity4
  check "rotateY 0° = I" $ approxEqMatrix (rotateY 0.0) identity4
  check "rotateZ 0° = I" $ approxEqMatrix (rotateZ 0.0) identity4

  -- Full rotation returns to identity
  check "rotateX 360° ≈ I" $ approxEqMatrix (rotateX 360.0) identity4
  check "rotateY 360° ≈ I" $ approxEqMatrix (rotateY 360.0) identity4
  check "rotateZ 360° ≈ I" $ approxEqMatrix (rotateZ 360.0) identity4

  -- Inverse rotation: rotate by θ then by -θ ≈ identity
  check "rotateX θ · rotateX -θ ≈ I" $ approxEqMatrix
    (M.multiply (rotateX 47.5) (rotateX (-47.5)))
    identity4
  check "rotateY θ · rotateY -θ ≈ I" $ approxEqMatrix
    (M.multiply (rotateY 47.5) (rotateY (-47.5)))
    identity4
  check "rotateZ θ · rotateZ -θ ≈ I" $ approxEqMatrix
    (M.multiply (rotateZ 47.5) (rotateZ (-47.5)))
    identity4

  -- Composition by angle addition: rotateX a · rotateX b ≈ rotateX (a+b)
  check "rotateX 90° · rotateX 90° ≈ rotateX 180°" $ approxEqMatrix
    (M.multiply (rotateX 90.0) (rotateX 90.0))
    (rotateX 180.0)
  check "rotateY 30° · rotateY 60° ≈ rotateY 90°" $ approxEqMatrix
    (M.multiply (rotateY 30.0) (rotateY 60.0))
    (rotateY 90.0)
  check "rotateZ 45° · rotateZ 45° ≈ rotateZ 90°" $ approxEqMatrix
    (M.multiply (rotateZ 45.0) (rotateZ 45.0))
    (rotateZ 90.0)

  -- 180° + 180° round-trip
  check "rotateX 180° · rotateX 180° ≈ I" $ approxEqMatrix
    (M.multiply (rotateX 180.0) (rotateX 180.0))
    identity4
  check "rotateY 180° · rotateY 180° ≈ I" $ approxEqMatrix
    (M.multiply (rotateY 180.0) (rotateY 180.0))
    identity4
  check "rotateZ 180° · rotateZ 180° ≈ I" $ approxEqMatrix
    (M.multiply (rotateZ 180.0) (rotateZ 180.0))
    identity4

  log "all rotation properties hold."

  -- ───── World backdrop geometry (M1) ─────────────────────────────────
  log "world geometry properties:"

  let
    gp = groundPlane 800.0
    gpYs = everyNth 3 1 gp.vertices
    gpXs = everyNth 3 0 gp.vertices
    gpZs = everyNth 3 2 gp.vertices

  -- A ground plane is one quad: 4 vertices (12 floats), 4 normals, 6 indices.
  check "groundPlane has 4 vertices (12 floats)" $ length gp.vertices == 12
  check "groundPlane has 4 normals (12 floats)" $ length gp.normals == 12
  check "groundPlane has 6 indices (2 triangles)" $ length gp.indices == 6

  -- The quad is flat at local Y = 0 (the model matrix places it in the world).
  check "groundPlane is flat at Y=0" $ all (\y -> approxEq y 0.0) gpYs

  -- All normals point straight up (+Y).
  check "groundPlane normals all (0,1,0)" $
    all (\i -> let n = nth gp.normals i in approxEq n.x 0.0 && approxEq n.y 1.0 && approxEq n.z 0.0)
      [ 0, 1, 2, 3 ]

  -- Corners span exactly [-e, +e] in X and Z.
  check "groundPlane X spans ±extent" $
    approxEq (fromMaybe 0.0 (minimum gpXs)) (-800.0) && approxEq (fromMaybe 0.0 (maximum gpXs)) 800.0
  check "groundPlane Z spans ±extent" $
    approxEq (fromMaybe 0.0 (minimum gpZs)) (-800.0) && approxEq (fromMaybe 0.0 (maximum gpZs)) 800.0

  -- Winding is CCW seen from above: cross((v1-v0),(v2-v0)) points +Y.
  check "groundPlane first triangle wound CCW (normal +Y)" $
    let
      v0 = nth gp.vertices 0
      v1 = nth gp.vertices 1
      v2 = nth gp.vertices 2
      ax = v1.x - v0.x
      az = v1.z - v0.z
      bx = v2.x - v0.x
      bz = v2.z - v0.z
      ny = az * bx - ax * bz -- +Y component of (a × b)
    in
      ny > 0.0

  -- Grid floor: n divisions ⇒ 2*(n+1) line segments ⇒ 4*(n+1) index entries.
  let
    gf = gridFloor 800.0 8
    gfYs = everyNth 3 1 gf.vertices
  check "gridFloor 8 has 2*(n+1)=18 segments (36 indices)" $ length gf.indices == 36
  check "gridFloor vertices flat at Y=0" $ all (\y -> approxEq y 0.0) gfYs
  check "gridFloor stays within ±extent" $
    all (\v -> abs v <= 800.0 + epsilon) gf.vertices

  log "all world geometry properties hold."

  -- ───── World placement (M2) ─────────────────────────────────────────
  log "world placement properties:"

  -- The ground's model matrix is a constant translation (no State input),
  -- so it never moves when the cube rotates.
  check "groundTransform = translate(0, groundY, 0)" $
    approxEqMatrix groundTransform (M.translate 0.0 groundY 0.0)

  -- The plane sits at the main cube's base (half-extent 100 ⇒ base at Y=-100).
  check "groundY places plane at cube base (-100)" $ approxEq groundY (-100.0)

  -- Extent stays within the far-plane budget (camera 1000 back, far 2000 ⇒ ≤ ~900).
  check "groundExtent within far-plane budget (≤ 900)" $ groundExtent <= 900.0

  log "all world placement properties hold."

  -- ───── Grid floor wiring (M3) ───────────────────────────────────────
  log "grid wiring properties:"

  -- The grid the app actually renders (extent × divisions).
  let
    appGrid = gridFloor groundExtent gridDivisions
    appGridVerts = (length appGrid.vertices) / 3

  -- n divisions ⇒ 2*(n+1) line segments ⇒ 4*(n+1) index entries.
  check "app grid has 2*(n+1) segments" $
    length appGrid.indices == 4 * (gridDivisions + 1)

  -- Every index addresses a real vertex (no out-of-range index).
  check "app grid indices in range" $
    all (\i -> i >= 0 && i < appGridVerts) appGrid.indices

  -- Vertex count stays under the Uint16 index-buffer cap (65536).
  check "app grid within Uint16 cap" $ appGridVerts < 65536

  log "all grid wiring properties hold."

  -- ───── Sky backdrop (M4) ────────────────────────────────────────────
  log "sky backdrop properties:"

  let inUnit v = v >= 0.0 && v <= 1.0

  -- Every channel is a valid normalized color component.
  check "skyColor channels in [0,1]" $
    inUnit skyColor.r && inUnit skyColor.g && inUnit skyColor.b && inUnit skyColor.a

  -- Regression guard: the backdrop is no longer plain white.
  check "skyColor is not white" $
    not (approxEq skyColor.r 1.0 && approxEq skyColor.g 1.0 && approxEq skyColor.b 1.0)

  -- A sky reads blue-dominant (blue ≥ red and ≥ green).
  check "skyColor is blue-dominant" $
    skyColor.b >= skyColor.r && skyColor.b >= skyColor.g

  log "all sky backdrop properties hold."

  -- ───── Shear matrix (shear-button M1) ───────────────────────────────
  log "shear matrix properties:"

  -- Zero shear is the identity.
  check "shear 0 = I" $ approxEqMatrix (M.shear 0.0) identity4

  -- The shear matrix differs from identity only at entry [0][1] = k.
  check "shear k has entry [0][1] = k" $
    approxEq (fromMaybe 0.0 (index (M.toVector (M.shear 0.7)) 1)) 0.7

  -- Shearing the +Y basis vector moves it by k in +X: (0,1,0,1) ↦ (k,1,0,1).
  check "shear k shears +Y into +X" $
    let
      sheared = M.toVector (M.multiply (M.shear 0.7) (M.fromColumn [ 0.0, 1.0, 0.0, 1.0 ]))
    in
      approxEq (fromMaybe 0.0 (index sheared 0)) 0.7

  log "all shear matrix properties hold."

  -- ───── Shear input wiring (shear-button M2) ─────────────────────────
  log "shear input properties:"

  -- The extended Input has a shear channel, empty by default (no click yet).
  check "emptyInput.shear is Nothing" $ isNothing emptyInput.shear

  log "all shear input properties hold."

  -- ───── UV sphere mesh (atomos M1) ───────────────────────────────────
  log "sphere mesh properties:"

  let
    latSeg = 12
    longSeg = 12
    sp = sphere latSeg longSeg 50.0
    spVerts = length sp.vertices / 3
    mag v = sqrt (v.x * v.x + v.y * v.y + v.z * v.z)

  -- A UV sphere has (lat+1)*(long+1) vertices, equally many normal floats.
  check "sphere vertex count = (lat+1)(long+1)" $
    length sp.vertices == (latSeg + 1) * (longSeg + 1) * 3
  check "sphere normals count matches vertices" $
    length sp.normals == length sp.vertices

  -- Two triangles per quad ⇒ lat*long*6 indices.
  check "sphere index count = lat*long*6" $
    length sp.indices == latSeg * longSeg * 6

  -- Every vertex lies on the sphere of the given radius.
  check "sphere vertices lie on radius" $
    all (\vi -> approxEq (mag (nth sp.vertices vi)) 50.0) (range 0 (spVerts - 1))

  -- Every normal is unit length.
  check "sphere normals are unit length" $
    all (\vi -> approxEq (mag (nth sp.normals vi)) 1.0) (range 0 (spVerts - 1))

  -- Every index addresses a real vertex.
  check "sphere indices in range" $
    all (\i -> i >= 0 && i < spVerts) sp.indices

  log "all sphere mesh properties hold."

  -- ───── Scene switch + starfield (atomos M2) ─────────────────────────
  log "scene + starfield properties:"

  -- The on-screen switch cycles CubePoc → Atomos → Molecule → Builder → CubePoc.
  check "nextScene cycles CubePoc -> Atomos -> Molecule -> Builder -> CubePoc" $
    nextScene CubePoc == Atomos
      && nextScene Atomos == Molecule
      && nextScene Molecule == Builder
      && nextScene Builder == CubePoc

  -- The starfield is a non-empty, deterministic set of points.
  check "starfield has stars" $ length starPositions > 0

  -- Stars sit far out (a backdrop), beyond the atom's region.
  check "stars are far out (>800)" $
    all (\p -> sqrt (p.x * p.x + p.y * p.y + p.z * p.z) > 800.0) starPositions

  log "all scene + starfield properties hold."

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

  -- ───── Shell / sub-shell colour palette (colours M1) ────────────────
  log "shell/sub-shell palette properties:"

  let
    csum c = c.r + c.g + c.b
    -- Which RGB channel dominates (0=r, 1=g, 2=b) — the colour's "hue family".
    dominant c
      | c.r >= c.g && c.r >= c.b = 0
      | c.g >= c.b = 1
      | otherwise = 2

  -- Each principal shell gets a distinct colour.
  check "shell colours are distinct (1≠2≠3≠4)" $
    shellColor 1 /= shellColor 2
      && shellColor 2 /= shellColor 3
      && shellColor 3 /= shellColor 4
      && shellColor 1 /= shellColor 3

  -- A sub-shell's ℓ=0 is exactly the shell base colour.
  check "subshellColor n 0 == shellColor n (base)" $
    subshellColor 2 0 == shellColor 2 && subshellColor 4 0 == shellColor 4

  -- Higher ℓ is lighter (blended toward white → larger channel sum).
  check "sub-shells get lighter with ℓ" $
    csum (subshellColor 3 0) < csum (subshellColor 3 1)
      && csum (subshellColor 3 1) < csum (subshellColor 3 2)

  -- Same shell colour, just lighter: the dominant channel (hue) is preserved.
  check "sub-shell keeps the shell hue" $
    dominant (subshellColor 1 1) == dominant (shellColor 1)
      && dominant (subshellColor 4 2) == dominant (shellColor 4)

  -- Channels stay within [0,1] even at high ℓ.
  check "sub-shell colour channels clamp to [0,1]" $
    let c = subshellColor 1 3 in c.r <= 1.0 && c.g <= 1.0 && c.b <= 1.0 && c.r >= 0.0

  log "all shell/sub-shell palette properties hold."

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

  -- ───── Scene titles for the banner (overlay-text M2) ────────────────
  log "scene title properties:"

  check "sceneTitle CubePoc = Cube POC" $ sceneTitle CubePoc == "Cube POC"
  check "sceneTitle Atomos = atomos" $ sceneTitle Atomos == "atomos"

  log "all scene title properties hold."

  -- ───── Molecule scene wiring (molecule-platform M2) ─────────────────
  -- The molecule scene joins the on-screen switch, so the toggle cycles
  -- CubePoc → Atomos → Molecule → Builder → CubePoc.
  log "molecule scene wiring properties:"

  -- The switch passes through every scene.
  check "nextScene CubePoc = Atomos" $ nextScene CubePoc == Atomos
  check "nextScene Atomos = Molecule" $ nextScene Atomos == Molecule

  -- The molecule scene has a non-empty banner title (the implementer matches
  -- this exact string in Scene.sceneTitle).
  check "sceneTitle Molecule = molecule" $ sceneTitle Molecule == "molecule"

  log "all molecule scene wiring properties hold."

  -- ───── Builder scene wiring (molecule-builder M2) ───────────────────
  -- The builder scene joins the on-screen switch as a fourth scene, so the
  -- toggle now cycles CubePoc → Atomos → Molecule → Builder → CubePoc.
  log "builder scene wiring properties:"

  -- The switch passes Molecule → Builder, and Builder closes the 4-cycle back
  -- to CubePoc.
  check "nextScene Molecule = Builder" $ nextScene Molecule == Builder
  check "nextScene Builder = CubePoc" $ nextScene Builder == CubePoc

  -- The builder scene has a non-empty banner title (the implementer matches
  -- this exact string in Scene.sceneTitle); lowercase like 'atomos'/'molecule'.
  check "sceneTitle Builder is non-empty" $ sceneTitle Builder /= ""
  check "sceneTitle Builder = builder" $ sceneTitle Builder == "builder"

  log "all builder scene wiring properties hold."

  -- ───── Molecule model (H₂ slice) ────────────────────────────────────
  log "molecule model properties:"

  let
    h2 = moleculeOf 0
    halfLen = bondLength / 2.0
    -- Read each atom's center; default to origin so a missing atom fails loudly.
    centerAt i = (fromMaybe { element: 0, center: { x: 0.0, y: 0.0, z: 0.0 } } (index h2.atoms i)).center
    a0c = centerAt 0
    a1c = centerAt 1
    bond0 = fromMaybe { a: -1, b: -1, order: 0, shared: 0 } (index h2.bonds 0)
    sharedX f = map _.x (sharedElectronPositions h2 f)
    meanOf xs = sum xs / toNumber (max 1 (length xs))

  -- H₂ has two hydrogen atoms.
  check "moleculeOf 0 has 2 atoms" $ length h2.atoms == 2
  check "moleculeOf 0 atoms are both Hydrogen (element 1)" $
    all (\m -> m.element == 1) h2.atoms

  -- A single covalent bond between atom 0 and atom 1, sharing 2 electrons.
  check "moleculeOf 0 has 1 bond" $ length h2.bonds == 1
  check "moleculeOf 0 bond == {a:0,b:1,order:1,shared:2}" $
    bond0.a == 0 && bond0.b == 1 && bond0.order == 1 && bond0.shared == 2

  -- Atoms sit symmetric on the x axis at ±bondLength/2, with y=z=0.
  check "H₂ atom 0 center x ≈ -(bondLength/2)" $ approxEq a0c.x (-halfLen)
  check "H₂ atom 1 center x ≈ +(bondLength/2)" $ approxEq a1c.x halfLen
  check "H₂ atom centers y ≈ 0" $ approxEq a0c.y 0.0 && approxEq a1c.y 0.0
  check "H₂ atom centers z ≈ 0" $ approxEq a0c.z 0.0 && approxEq a1c.z 0.0

  -- Properties: non-empty, includes the formula row plus covalent/length/energy/
  -- shared rows (≥4), with at least one row about shared/bonding electrons.
  check "H₂ properties are non-empty" $ length h2.properties > 0
  check "H₂ properties contain {label:Formula, value:H₂}" $
    any (\p -> p.label == "Formula" && p.value == "H₂") h2.properties
  check "H₂ has the formula + covalent/length/energy/shared rows (≥4)" $
    length h2.properties >= 4
  check "H₂ properties mention shared electrons" $
    isJust (find (\p -> p.label == "Shared") h2.properties)

  -- Shared electrons sit in the internuclear overlap region, centred near x=0.
  check "H₂ shared electrons count == 2" $
    length (sharedElectronPositions h2 0.0) == 2
  check "H₂ shared electrons lie within [-half,+half] in x" $
    all (\p -> p.x >= -halfLen - 1.0e-6 && p.x <= halfLen + 1.0e-6)
      (sharedElectronPositions h2 0.0)
  check "H₂ shared electrons mean x ≈ 0" $
    abs (meanOf (sharedX 0.0)) < 1.0
  check "H₂ shared electrons are frame-animated" $
    sharedElectronPositions h2 0.0 /= sharedElectronPositions h2 60.0

  -- Nucleons: one proton per H, placed at each atom's center.
  let
    h2Nuc = moleculeNucleons h2
    nucXs = map (\n -> n.pos.x) h2Nuc
  check "H₂ has 2 nucleons" $ length h2Nuc == 2
  check "H₂ nucleons are all protons" $ all (\n -> n.kind == Atom.Proton) h2Nuc
  check "H₂ nucleon x's are ≈ ±bondLength/2" $
    any (\x -> approxEq x (-halfLen)) nucXs && any (\x -> approxEq x halfLen) nucXs

  -- Extension seam: registry is non-empty; moleculeOf is total/clamped.
  check "molecules registry is non-empty" $ length molecules >= 1
  check "moleculeOf (-1) clamps to first molecule" $
    length (moleculeOf (-1)).atoms >= 1
  check "moleculeOf 9999 clamps (atoms non-empty)" $
    length (moleculeOf 9999).atoms >= 1

  log "all molecule model properties hold."

  -- ───── Builder + Chem (molecule builder) ───────────────────────────
  -- RED: Chem.purs and Builder.purs do not exist yet. The implementer builds
  -- `Chem.valence :: Int -> Int` (clamp-safe valence table for Z=1..36) and the
  -- pure `Builder` world model (place/move atoms, recompute bonds with valence +
  -- hysteresis, connected-component molecules, Unicode formulae, and pure
  -- pick/unproject helpers for cursor→world placement).
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
  -- RED until Builder.purs adds the pure helpers `componentOf` (the connected
  -- component containing an atom id, reusing the molecules/flood logic) and
  -- `moveMolecule` (translate the WHOLE component of the anchor atom rigidly so
  -- the anchor lands at the target — delta applied to every component atom —
  -- then recomputeBonds). Single-click+drag uses moveMolecule (whole molecule);
  -- double-click+drag keeps moveAtom (one atom).
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
  -- RED until Builder.purs adds the pure helpers degreeOf/loneCountOf and the
  -- frame-animated bondElectronPositions/loneElectronPositions. The key
  -- invariant under test is ELECTRON CONSERVATION: per atom, the bonding
  -- electrons (2 per incident bond, shared) plus its lone electrons always sum
  -- to Chem.valence z. So across the whole world:
  --   length bondElectronPositions + length loneElectronPositions
  --     == Σ over atoms of valence z.
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
  -- RED until Builder.purs adds the pure helpers valenceShellOf,
  -- coreLoneElectronPositions, valenceLoneElectronPositions. They split an
  -- atom's lone electrons into CORE (inner, always-lone shells) and VALENCE
  -- (outermost shell). Model:
  --   valenceShellOf z = last (Atom.electronShells z)
  --   coreCount(atom)  = z − valenceShellOf z      (inner shells, always lone)
  --   valenceLone(atom)= max 0 (valenceShellOf z − degree)
  -- so coreCount + valenceLone == loneCountOf (= z − degree), and
  --   loneElectronPositions == coreLoneElectronPositions <> valenceLoneElectronPositions.
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

  -- pick / unproject round-trip. Build a simple, pure perspective×camera
  -- projection here from Math.Matrix (matching Main.perspectiveProjection's
  -- shape: a perspective matrix composed with a -cameraDistance translation),
  -- rather than importing the unexported Main.perspectiveProjection — keeps the
  -- test free of WebGL/Effect deps. projectToScreen maps a world V3 to a pixel;
  -- unprojectAtDepth inverts it onto the plane at the reference atom's depth.
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

  -- ───── Camera (zoom projection) ─────────────────────────────────────
  -- RED: Camera.purs does not exist yet. The implementer builds the pure,
  -- zoom-aware projection module (imports Math.Matrix only):
  --   * projection zoom w h — the SAME perspective matrix as
  --     Main.perspectiveProjection w h, EXCEPT the camera translation uses an
  --     effective distance cameraDistance / zoom (zoom 1.0 == today's matrix).
  --   * clampZoom — clamp to [minZoom=0.2, maxZoom=5.0].
  --   * applyZoomStep currentZoom wheelDeltaY — multiplicative, clamped: NEGATIVE
  --     deltaY (wheel up) zooms IN (larger zoom), POSITIVE zooms OUT (smaller).
  --   * minZoom / maxZoom constants.
  log "camera zoom projection properties:"

  -- clampZoom: bounds are returned exactly; in-range passes through.
  check "clampZoom below min ⇒ minZoom" $ Cam.clampZoom (-1.0) == Cam.minZoom
  check "clampZoom above max ⇒ maxZoom" $ Cam.clampZoom 100.0 == Cam.maxZoom
  check "clampZoom in-range (1.0) ⇒ 1.0" $ approxEq (Cam.clampZoom 1.0) 1.0

  -- applyZoomStep direction: negative deltaY zooms IN (larger), positive OUT.
  check "applyZoomStep: wheel up (−Δ) zooms IN (>1.0)" $
    Cam.applyZoomStep 1.0 (-100.0) > 1.0
  check "applyZoomStep: wheel down (+Δ) zooms OUT (<1.0)" $
    Cam.applyZoomStep 1.0 100.0 < 1.0

  -- applyZoomStep clamps at the bounds (already-max zoom-in stays max, etc.).
  check "applyZoomStep: already-max zoom-in stays maxZoom" $
    Cam.applyZoomStep Cam.maxZoom (-1000.0) == Cam.maxZoom
  check "applyZoomStep: already-min zoom-out stays minZoom" $
    Cam.applyZoomStep Cam.minZoom 1000.0 == Cam.minZoom

  -- Repeated stepping stays within [minZoom, maxZoom] (folded over many steps).
  let
    zoomInMany = foldZoom (-200.0) 1.0 (range 1 20)
    zoomOutMany = foldZoom 200.0 1.0 (range 1 20)
  check "applyZoomStep: many zoom-in steps stay ≤ maxZoom" $
    zoomInMany <= Cam.maxZoom
  check "applyZoomStep: many zoom-out steps stay ≥ minZoom" $
    zoomOutMany >= Cam.minZoom

  -- determinism: same inputs ⇒ same output.
  check "applyZoomStep is deterministic" $
    Cam.applyZoomStep 1.0 50.0 == Cam.applyZoomStep 1.0 50.0

  -- projection baseline: zoom 1.0 equals the existing testProjection ENTRY-BY-ENTRY.
  check "projection 1.0 == testProjection (entry-by-entry)" $
    approxEqMatrix (Cam.projection 1.0 800.0 600.0) (testProjection 800.0 600.0)

  -- projection differs at zoom 2.0 (camera distance halved ⇒ translation differs).
  check "projection 2.0 differs from projection 1.0 (some entry)" $
    not (approxEqMatrix (Cam.projection 2.0 800.0 600.0) (Cam.projection 1.0 800.0 600.0))

  -- Zoom-out frustum: at minZoom the focal point (world origin) must stay INSIDE
  -- the frustum (NDC z within [-1, 1]). Regression for the bug where the fixed
  -- far plane (2000) clipped the scene once the camera pulled back past it
  -- (effective distance cameraDistance / minZoom = 5000 > 2000 ⇒ atoms vanished).
  let
    projMin = Cam.projection Cam.minZoom 800.0 600.0
    originClip = M.toVector $ M.multiply projMin
      (fromMaybe (M.zeros 4 1) (M.fromArray 4 1 [ 0.0, 0.0, 0.0, 1.0 ]))
    originNdcZ = fromMaybe 0.0 (index originClip 2) / fromMaybe 0.0 (index originClip 3)
  check "projection minZoom: origin stays in front of the far plane (ndc z ≤ 1)" $
    originNdcZ <= 1.0
  check "projection minZoom: origin stays behind the near plane (ndc z ≥ -1)" $
    originNdcZ >= -1.0

  -- Control-panel zoom buttons: the #zoom-in / #zoom-out buttons reuse the
  -- existing applyZoomStep by pushing a FIXED synthetic wheel delta,
  -- Cam.buttonZoomDelta. RED until Camera.purs exports that positive constant.
  -- + zooms IN (push −buttonZoomDelta), − zooms OUT (push +buttonZoomDelta);
  -- both clamp at the bounds, exactly like the wheel.
  check "buttonZoomDelta is a positive magnitude" $
    Cam.buttonZoomDelta > 0.0
  check "zoom-in button (−Δ) increases zoom" $
    Cam.applyZoomStep 1.0 (negate Cam.buttonZoomDelta) > 1.0
  check "zoom-out button (+Δ) decreases zoom" $
    Cam.applyZoomStep 1.0 Cam.buttonZoomDelta < 1.0
  check "zoom-in button clamps at maxZoom" $
    Cam.applyZoomStep Cam.maxZoom (negate Cam.buttonZoomDelta) <= Cam.maxZoom
  check "zoom-out button clamps at minZoom" $
    Cam.applyZoomStep Cam.minZoom Cam.buttonZoomDelta >= Cam.minZoom

  log "all camera zoom projection properties hold."

  -- ───── Builder valence-only toggle (valence-only M2) ────────────────
  -- RED until Main gains a `valenceOnly :: Boolean` field (init false) and the
  -- pure `applyValenceOnly :: Boolean -> State -> State` toggle, mirroring
  -- applyToggle2D: `applyValenceOnly true s = s { valenceOnly = not s.valenceOnly }`,
  -- `applyValenceOnly false s = s` (identity).
  log "builder valence-only toggle properties:"

  -- true flips the field from its false init.
  check "applyValenceOnly true flips false→true" $
    (applyValenceOnly true initialState).valenceOnly == true
  -- A double-toggle returns to the original value.
  check "applyValenceOnly true twice returns to false" $
    (applyValenceOnly true (applyValenceOnly true initialState)).valenceOnly == false
  -- false is identity on the field.
  check "applyValenceOnly false leaves valenceOnly = false" $
    (applyValenceOnly false initialState).valenceOnly == false
  -- false is a no-op: other fields (e.g. view2D) are untouched.
  check "applyValenceOnly false leaves view2D unchanged" $
    (applyValenceOnly false initialState).view2D == initialState.view2D

  log "all builder valence-only toggle properties hold."

  -- ───── Atomos sub-shell view toggle (atomos shell/sub-shell toggle M2) ─────
  -- RED: Main does not yet export `applySubshellView` and `State.subshellView`
  -- does not exist. This test will fail to compile until the implementer adds:
  --   * `subshellView :: Boolean` to State (initialised to `true` in initialState)
  --   * `applySubshellView :: Boolean -> State -> State` where
  --       `applySubshellView true s  = s { subshellView = not s.subshellView }`
  --       `applySubshellView false s = s`  (identity)
  -- and exports `applySubshellView` from Main.
  log "atomos sub-shell view toggle properties:"

  -- Default: subshellView starts as true (sub-shell view is on by default).
  check "initialState.subshellView == true (default is sub-shell view)" $
    initialState.subshellView == true

  -- true flips the field from its true init (true → false).
  check "applySubshellView true flips true→false" $
    (applySubshellView true initialState).subshellView == false

  -- A double-toggle returns to the original value (false → true via not).
  check "applySubshellView true twice returns to true" $
    (applySubshellView true (applySubshellView true initialState)).subshellView == true

  -- false is identity on the field (unchanged from the default true).
  check "applySubshellView false leaves subshellView = true" $
    (applySubshellView false initialState).subshellView == true

  -- false is a no-op: other fields (e.g. view2D) are untouched.
  check "applySubshellView false leaves view2D unchanged" $
    (applySubshellView false initialState).view2D == initialState.view2D

  log "all atomos sub-shell view toggle properties hold."

  -- ───── Atom shell-collapsed positions (atomos shell/sub-shell toggle M1) ─
  -- RED: Atom.purs does not yet export electronPositionsByShell,
  -- electronPositionsByShell2D, or shellRings. These tests will fail to
  -- compile until the implementer adds those three exports.
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

  -- ───── Continuous level-of-detail (Layer smoothstep/layerBlend/easeDetail) ─
  -- RED: src/Layer.purs is being replaced — the discrete scale-ladder API
  -- (ScaleLayer/layerUp/layerDown/applyLayerZoom/…) gives way to a CONTINUOUS
  -- LOD model. These tests reference functions that DO NOT EXIST YET, so they
  -- fail to compile (unknown value / not exported) until the implementer adds:
  --   * `smoothstep :: Number -> Number -> Number -> Number` — a clamped Hermite
  --     interpolation (edge0 edge1 x), 0 below edge0, 1 above edge1, smooth in
  --     between (the classic 3t²−2t³).
  --   * detail-band constants `detailLo` / `detailHi :: Number`, bracketing the
  --     zoom range where the ball→detailed crossfade happens, strictly inside the
  --     camera bounds: Camera.minZoom < detailLo < detailHi <= 1.0.
  --   * `layerBlend :: Number -> Number` — maps a zoom factor to a detail level
  --     in [0,1] (0 = zoomed-out "balls", 1 = full detail), monotonic
  --     non-decreasing, saturating at both ends of the camera zoom range.
  --   * `easeDetail :: Number -> Number -> Number` — one frame of exponential
  --     smoothing of a current detail value toward a target, a contraction that
  --     converges to the target (fixed point at equality, stays in range).
  log "continuous LOD (Layer smoothstep/layerBlend/easeDetail) properties:"

  -- smoothstep: clamped Hermite. Below edge0 → 0, above edge1 → 1, symmetric
  -- midpoint → 0.5, and strictly increasing through the band.
  check "smoothstep below edge0 == 0.0" $
    Layer.smoothstep 0.4 0.85 0.3 == 0.0
  check "smoothstep above edge1 == 1.0" $
    Layer.smoothstep 0.4 0.85 0.9 == 1.0
  check "smoothstep midpoint of [0,1] == 0.5" $
    approxEq (Layer.smoothstep 0.0 1.0 0.5) 0.5
  check "smoothstep is monotonic increasing (0.25 < 0.75)" $
    Layer.smoothstep 0.0 1.0 0.25 < Layer.smoothstep 0.0 1.0 0.75

  -- Detail-band invariant: the crossfade band sits strictly inside the camera
  -- zoom bounds and is ordered, terminating at or before the Builder default
  -- zoom (1.0):  Camera.minZoom < detailLo < detailHi <= 1.0.
  check "detail band invariant: minZoom < detailLo" $
    Cam.minZoom < Layer.detailLo
  check "detail band invariant: detailLo < detailHi" $
    Layer.detailLo < Layer.detailHi
  check "detail band invariant: detailHi <= 1.0" $
    Layer.detailHi <= 1.0

  -- layerBlend: zoom → detail in [0,1]. Fully zoomed OUT (minZoom) → 0 (balls),
  -- the Builder default zoom (1.0) and fully zoomed IN (maxZoom) → 1 (full
  -- detail), monotonic non-decreasing on samples.
  check "layerBlend at minZoom == 0.0 (zoomed out -> balls)" $
    approxEq (Layer.layerBlend Cam.minZoom) 0.0
  check "layerBlend at 1.0 == 1.0 (Builder default -> full detail)" $
    approxEq (Layer.layerBlend 1.0) 1.0
  check "layerBlend at maxZoom == 1.0 (fully zoomed in)" $
    approxEq (Layer.layerBlend Cam.maxZoom) 1.0
  check "layerBlend monotonic non-decreasing (0.5 <= 1.0)" $
    Layer.layerBlend 0.5 <= Layer.layerBlend 1.0
  check "layerBlend monotonic non-decreasing (minZoom <= 0.6)" $
    Layer.layerBlend Cam.minZoom <= Layer.layerBlend 0.6

  -- easeDetail: one frame of smoothing current → target. Fixed point at
  -- equality, moves strictly toward the target without overshoot, stays in range,
  -- and iterating it converges to the target.
  let
    easedConverged = foldl (\c _ -> Layer.easeDetail c 1.0) 0.0 (range 1 60)

  check "easeDetail fixed point: easeDetail 1.0 1.0 == 1.0" $
    approxEq (Layer.easeDetail 1.0 1.0) 1.0
  check "easeDetail moves toward target (0.0 -> 1.0 lands strictly between)" $
    Layer.easeDetail 0.0 1.0 > 0.0 && Layer.easeDetail 0.0 1.0 < 1.0
  check "easeDetail stays in range (no overshoot either direction)" $
    Layer.easeDetail 0.0 1.0 <= 1.0 && Layer.easeDetail 1.0 0.0 >= 0.0
  check "easeDetail converges to target after ~60 frames (within 1e-3)" $
    abs (easedConverged - 1.0) < 1.0e-3

  log "all continuous LOD properties hold."

  -- ───── Builder atom visuals: atomicRadius / symbolOf / bondSegments ──
  -- RED: these three pure helpers do NOT exist yet.
  --   * Atom.atomicRadius :: Int -> Number — per-element relative atom size,
  --     positive and element-varying (Hydrogen smallest), clamp-safe (Z<1→1,
  --     Z>36→36).
  --   * Atom.symbolOf :: Int -> String — clamp-safe element symbol lookup.
  --   * Builder.bondSegments :: BuilderState -> Array { a :: V3, b :: V3 } —
  --     one segment per bond, endpoints being the two bonded atom centres.
  -- They fail to compile (unknown value / not exported) until implemented.
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

  -- ───── M1: minSeparation + resolveOverlaps (constraint model) ────────
  -- RED: Builder does not yet export `minSeparation` or `resolveOverlaps`.
  -- These tests MUST fail to compile until the implementer adds those two pure
  -- exports to src/Builder.purs. Do NOT implement them before the tests pass RED.
  --
  -- Constants (documented in the milestone spec):
  --   contactFactor = 55.0
  --   absoluteMin   = 130.0
  --   floorCeil     = 165.0  (= bondThreshold 180.0 − floorMargin 15.0)
  --   clampFloor x  = max absoluteMin (min floorCeil x)
  --   minSeparation z1 z2 = clampFloor (contactFactor * (atomicRadius z1 + atomicRadius z2))
  --   resolveOverlaps anchors state = bounded relaxation (relaxPasses=10)
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
  -- GREEN as of M2: addAtom / moveAtom / moveMolecule each run resolveOverlaps
  -- (Pauli-exclusion separation) BEFORE recomputeBonds, with anchors of
  -- pre-existing ids / [aid] / the moved component respectively. These tests
  -- were RED (coincident pairs stayed coincident) until that wiring landed.
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
    -- Build a bonded H-H molecule: two H at distance 150 (< bondThreshold,
    -- > floor). Place a lone C far enough away to not bond. Move the whole
    -- H-H molecule so its anchor lands exactly on the C's position. The
    -- anchor lands at the C exactly (1e-10). The internal pair distance
    -- (within the H-H component) is preserved (1e-6, rigid). Every cross pair
    -- (H × C) ends up >= their floor - m2GeoTol.
    -- Use explicit resolveOverlaps to set up bonded H-H cleanly.
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
    -- Clear, addAtom H at origin, addAtom H at distance 50 (< floor ~130).
    -- After addAtom wiring, constraint separates them to >= floor but < bondThreshold,
    -- so exactly one bond forms and molecules has ONE 2-atom component.
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
    -- Repeat scenario (a) twice independently. Final positions must be identical
    -- (1e-10). Uses the same C-then-H-at-same-pos fixture.
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

  -- ───── M1: Chem.bondEnergy + Builder.pullBonds + Builder.moveAtomWith ───────
  --
  -- RED: `Chem.bondEnergy`, `Builder.pullBonds`, and `Builder.moveAtomWith` do
  -- NOT exist yet.  These tests MUST fail to compile (UnknownName / not exported)
  -- until the implementer adds those three pure exports.  Do NOT implement them
  -- before the tests pass RED.
  --
  -- Agreed design:
  --   Chem.bondEnergy :: Int -> Int -> Number
  --     normalised single-bond energy (kJ/mol ÷ 100), clamp-safe (clampZ both
  --     args), symmetric.
  --     Tabulated pairs (unordered):
  --       H-H  4.36, C-C  3.46, C-H  4.13, O-H  4.63, N-H  3.91,
  --       C-O  3.58, C-N  3.05, O-O  1.46, N-N  1.63, F-F  1.55,
  --       H-F  5.65, Cl-Cl 2.42, H-Cl 4.31, Br-Br 1.90
  --     Homonuclear anchors: Si-Si 2.22, P-P 2.01, S-S 2.66, Se-Se 1.72, B-B 2.93
  --     Fallback: tabulated cross pair → use exact table entry;
  --       untabulated but BOTH homonuclear anchors present → sqrt(E(a,a)·E(b,b));
  --       untabulated homonuclear → 1.5; everything else → 2.5.
  --
  --   Builder.pullBonds :: Number -> Int -> BuilderState -> BuilderState
  --     (strength, draggedAid): 10 fixed passes; bonds stretched past
  --     breakThreshold (230) with bondEnergy >= strength pull the NON-dragged
  --     endpoint along the bond axis to restLen = max(minSeparation za zb, 160.0);
  --     strength-beaten bonds left stretched; dragged atom never moved.
  --
  --   Builder.moveAtomWith :: Number -> Int -> V3 -> BuilderState -> BuilderState
  --     = set pos → pullBonds → resolveOverlaps [aid] → recomputeBonds.
  --     moveAtom ≡ moveAtomWith 1e18 (byte-compatible legacy).
  --
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
  --     If the implementation tabulates differently these assertions name the contract.
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
  --     C(6)+Si(14): C-C=3.46, Si-Si=2.22 → sqrt(3.46*2.22) ≈ 2.7716...
  --     (If C-Si is in the table the implementation may tabulate it directly;
  --     this test pins the geometric-mean value as the agreed contract.)
  let
    cSiGeoMean = sqrt (3.46 * 2.22)
  check "bondEnergy C-Si (6,14) == sqrt(3.46*2.22) within 1e-10 (geo-mean fallback)" $
    approxEq (bondEnergy 6 14) cSiGeoMean

  -- FALLBACK — two untabulated-homonuclear elements → geometric mean of 1.5·1.5 = 1.5.
  --   Neon (Z=10, noble gas) and Argon (Z=18, noble gas): neither has a tabulated
  --   homonuclear anchor, so homoDefault 1.5 applies to both → sqrt(1.5*1.5) = 1.5.
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
  --     H at x=0, O at x=150 (bonds H-O), H at x=300 (bonds O-H); H-H at 300 apart
  --     (beyond bondThreshold 180) so no H-H bond.
  --     Drag END H (id0) to x=-400 with strength 3.0: O-H energies are 4.63 >= 3.0
  --     so BOTH bonds survive and all three atoms end up displaced from their
  --     original positions by more than 1.0 world unit.
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
  --     Exactly 1 bond remains and it connects the second O and the H.
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
  --     moveAtomWith 1e18 gives identical positions+bonds to moveAtom (equivalence).
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

  -- ───── M2: applyDragStrength + dragStrength state field ──────────────
  -- RED: `State.dragStrength` and `applyDragStrength` do NOT exist yet.
  -- These tests MUST fail to compile (unknown value / not exported) until the
  -- implementer adds:
  --   * `dragStrength :: Number` to the State record, initialised to 3.0
  --   * `applyDragStrength :: Maybe Number -> State -> State`
  --       applyDragStrength Nothing s  = s
  --       applyDragStrength (Just d) s = s { dragStrength = d }
  -- and exports `applyDragStrength` from Main.
  --
  -- Additionally, two extra moveAtomWith scenario tests pin short-stretch
  -- (O-H dragged within breakThreshold, bond survives but partner IS moved)
  -- and strength-0 (NOTHING breaks — every bond energy >= 0.0, so every
  -- stretched bond holds and pulls its partner along; slider at 0 can't break).
  log "M2 drag-strength wiring (applyDragStrength / State.dragStrength) properties:"

  -- Default: dragStrength starts at 3.0.
  check "initialState.dragStrength == 3.0" $
    initialState.dragStrength == 3.0

  -- Nothing is identity on all fields.
  check "applyDragStrength Nothing leaves dragStrength unchanged" $
    (applyDragStrength Nothing initialState).dragStrength == initialState.dragStrength

  -- Just d updates dragStrength to d.
  check "applyDragStrength (Just 5.0) sets dragStrength = 5.0" $
    (applyDragStrength (Just 5.0) initialState).dragStrength == 5.0
  check "applyDragStrength (Just 0.0) sets dragStrength = 0.0" $
    (applyDragStrength (Just 0.0) initialState).dragStrength == 0.0

  -- Nothing does not mutate other fields (e.g. view2D).
  check "applyDragStrength Nothing leaves view2D unchanged" $
    (applyDragStrength Nothing initialState).view2D == initialState.view2D

  -- Just d does not mutate other fields (e.g. zoom).
  check "applyDragStrength (Just 7.0) leaves zoom unchanged" $
    (applyDragStrength (Just 7.0) initialState).zoom == initialState.zoom

  log "all M2 drag-strength wiring properties hold."

  -- ───── M2: short-stretch O-H scenario (within breakThreshold) ────────
  -- O-H bonded at 150; drag O only to x=250 (still within breakThreshold 230
  -- when H is pulled back — the bond SURVIVES and H's position changes). Uses
  -- strength 3.0 (< O-H 4.63, so bond is strong relative to drag → pullBonds
  -- tugs H). The final O-H distance should be <= breakThreshold + 1e-6 AND
  -- H must have moved from its original position.
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
  -- O-O bonded at 150; drag one O to x=600 with strength 0.0.
  -- pullBonds HOLDS a stretched bond when bondEnergy >= strength; O-O is
  -- 1.46 >= 0.0, so even the WEAKEST bond resists a zero-strength drag: the
  -- partner is pulled along the bond axis to restLen (~160) from the dragged
  -- atom's NEW position, recomputeBonds keeps the pair (160 <= 230), and the
  -- bond SURVIVES. Slider at 0 ⇒ nothing can ever be ripped apart.
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
  -- RED: `Builder.spawnPos` does NOT exist yet.  These tests MUST fail to compile
  -- (UnknownName / not exported) until the implementer adds:
  --   spawnPos :: Int -> V3
  -- a deterministic, total 3D position distributor based on the golden-angle /
  -- Fibonacci shell: azimuth = i * goldenAngle (~2.39996 rad), elevation via
  -- acos(1 - 2*(i+0.5)/k) for some k, radius growing slowly with index.
  -- Successive atoms must differ in x, y AND z (no collinear y=z=0 spawn).
  -- Do NOT implement spawnPos before the tests pass RED.
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
  --     A NaN value does NOT equal itself; a finite value does.
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
  --     This verifies that addAtom's resolveOverlaps wiring holds after 3D spawn.
  check "spawnPos: 5-atom world has all pairs >= absoluteMin - 1e-6 (Pauli floor)" $
    minPairDist spawn5 >= B.absoluteMin - 1.0e-6

  log "all M1 3D spawn (Builder.spawnPos) properties hold."

  -- ───── Camera.orbit + Camera.viewProjection (M2 genuinely-3D) ──────────
  -- RED: `Camera.orbit` and `Camera.viewProjection` are NOT exported from
  -- Camera.purs yet. These tests MUST fail to compile (UnknownName / not in
  -- scope) until the M2 implementer adds those two exports. Do NOT implement
  -- them before the tests pass RED.
  --
  -- Agreed design:
  --   orbit :: Number -> Number -> Matrix Number
  --     = rotateY yaw `multiply` rotateX pitch
  --     where yaw and pitch are in RADIANS (unlike the existing rotateY/rotateX
  --     which take degrees; orbit will convert internally or do its own
  --     rotation from scratch — the contract tested here is the observable
  --     matrix, not the implementation).
  --
  --   viewProjection :: { yaw :: Number, pitch :: Number }
  --                  -> Number -> Number -> Number -> Matrix Number
  --     = (projection zoom w h) `multiply` (orbit yaw pitch)
  --     MUST be byte-identical to (projection zoom w h) when yaw = pitch = 0.
  log "Camera.orbit + Camera.viewProjection (M2 3D) properties:"

  -- (a) IDENTITY: orbit 0.0 0.0 is the 4x4 identity matrix (tol 1e-10).
  check "orbit 0.0 0.0 == M.identity (entry-by-entry, 1e-10)" $
    approxEqMatrix (Cam.orbit 0.0 0.0) M.identity

  -- (b) BYTE-IDENTICAL REGRESSION GUARD: viewProjection {yaw:0,pitch:0} z w h
  --     must equal projection z w h for at least two (z,w,h) triples.
  check "viewProjection {0,0} 1.0 800.0 600.0 == projection 1.0 800.0 600.0 (1e-10)" $
    approxEqMatrix
      (Cam.viewProjection { yaw: 0.0, pitch: 0.0 } 1.0 800.0 600.0)
      (Cam.projection 1.0 800.0 600.0)
  check "viewProjection {0,0} 2.0 1024.0 768.0 == projection 2.0 1024.0 768.0 (1e-10)" $
    approxEqMatrix
      (Cam.viewProjection { yaw: 0.0, pitch: 0.0 } 2.0 1024.0 768.0)
      (Cam.projection 2.0 1024.0 768.0)

  -- (c) ORBIT IS REAL: a non-zero yaw makes the matrix differ from identity and
  --     makes viewProjection differ from plain projection (orbit actually rotates).
  check "orbit 0.5 0.0 /= M.identity (non-zero yaw is not identity)" $
    not (approxEqMatrix (Cam.orbit 0.5 0.0) M.identity)
  check "viewProjection {yaw:0.5,pitch:0} 1.0 800.0 600.0 /= projection 1.0 800.0 600.0" $
    not
      ( approxEqMatrix
          (Cam.viewProjection { yaw: 0.5, pitch: 0.0 } 1.0 800.0 600.0)
          (Cam.projection 1.0 800.0 600.0)
      )

  -- (d) AXIS MAP: orbit (pi/2) 0.0 == rotateY(pi/2) as a pure rotation matrix.
  --     rotateY takes DEGREES, so rotateY(90°) should equal orbit(pi/2, 0).
  --     For a 90° Y-rotation the standard matrix entries are:
  --       [0][0] = cos(pi/2) ≈ 0    [0][2] = sin(pi/2) ≈ 1
  --       [2][0] = -sin(pi/2) ≈ -1  [2][2] = cos(pi/2) ≈ 0
  --     We read these entries from M.toVector (row-major, 4 columns):
  --       entry [0][0] = index 0,  entry [0][2] = index 2
  --       entry [2][0] = index 8,  entry [2][2] = index 10
  let
    orbitHalfPi = Cam.orbit (pi / 2.0) 0.0
    orbitVec = M.toVector orbitHalfPi
    orbitEntry r c = fromMaybe 999.0 (index orbitVec (r * 4 + c))

  check "orbit (pi/2) 0.0 entry [0][0] ≈ 0.0 (cos 90°)" $
    approxEq (orbitEntry 0 0) 0.0
  check "orbit (pi/2) 0.0 entry [0][2] ≈ 1.0 (sin 90°)" $
    approxEq (orbitEntry 0 2) 1.0
  check "orbit (pi/2) 0.0 entry [2][0] ≈ -1.0 (-sin 90°)" $
    approxEq (orbitEntry 2 0) (-1.0)
  check "orbit (pi/2) 0.0 entry [2][2] ≈ 0.0 (cos 90°)" $
    approxEq (orbitEntry 2 2) 0.0
  -- Y-axis is unchanged by a Y-rotation: entry [1][1] == 1.0.
  check "orbit (pi/2) 0.0 entry [1][1] == 1.0 (Y axis unaffected)" $
    approxEq (orbitEntry 1 1) 1.0

  log "all Camera.orbit + Camera.viewProjection (M2 3D) properties hold."

  -- ───── M3: Matrix.transpose + Builder.unprojectAtDepthFull + Camera.clampPitch ─
  --
  -- RED: `Math.Matrix.transpose`, `Builder.unprojectAtDepthFull`, `Camera.clampPitch`,
  -- and `Camera.maxPitch` do NOT exist yet. These tests MUST fail to compile
  -- (UnknownName / not exported) until the implementer adds those four exports.
  -- Do NOT implement them before the tests pass RED.
  --
  -- Agreed design:
  --   Math.Matrix.transpose :: Matrix Number -> Matrix Number
  --     Standard 4x4 row-major transpose: entry (i,j) of output = entry (j,i) of input.
  --
  --   Builder.unprojectAtDepthFull
  --     :: Matrix Number -> Matrix Number
  --     -> { w :: Number, h :: Number }
  --     -> { x :: Number, y :: Number }
  --     -> V3
  --     -> V3
  --     args: (orbit, projection, canvas, px, worldRef)
  --     Recipe (orbit is orthogonal, so orbitᵀ = orbit⁻¹):
  --       viewRef   = mulVec orbit worldRef
  --       viewPoint = unprojectAtDepth projection canvas px viewRef
  --       worldPoint = mulVec (Matrix.transpose orbit) viewPoint
  --
  --   Camera.maxPitch :: Number   (≈ 85° in radians = 1.4835)
  --   Camera.clampPitch :: Number -> Number
  --     Clamps pitch radians to ±maxPitch.
  log "M3 (transpose / unprojectAtDepthFull / clampPitch) properties:"

  -- ── (A) Matrix.transpose ─────────────────────────────────────────────────────
  let
    -- A known non-symmetric 4×4 matrix with distinct entries at each position.
    -- Row-major layout: entries are [ row0col0, row0col1, ..., row3col3 ].
    testM = fromMaybe (M.zeros 4 4) $ M.fromArray 4 4
      [ 1.0
      , 2.0
      , 3.0
      , 4.0
      , 5.0
      , 6.0
      , 7.0
      , 8.0
      , 9.0
      , 10.0
      , 11.0
      , 12.0
      , 13.0
      , 14.0
      , 15.0
      , 16.0
      ]
    testMT = M.transpose testM
    -- entry (i,j) from a 4x4 row-major matrix vector.
    entryM m_ i_ j_ = fromMaybe 0.0 (index (M.toVector m_) (i_ * 4 + j_))

  -- transpose swaps (i,j) ↔ (j,i) for a few representative off-diagonal entries.
  check "M3 transpose: entry (0,1) of transpose == entry (1,0) of original (2.0 -> 5.0)" $
    approxEq (entryM testMT 0 1) (entryM testM 1 0)
  check "M3 transpose: entry (1,0) of transpose == entry (0,1) of original (5.0 -> 2.0)" $
    approxEq (entryM testMT 1 0) (entryM testM 0 1)
  check "M3 transpose: entry (0,2) of transpose == entry (2,0) of original (3.0 -> 9.0)" $
    approxEq (entryM testMT 0 2) (entryM testM 2 0)
  check "M3 transpose: entry (2,3) of transpose == entry (3,2) of original (12.0 -> 15.0)" $
    approxEq (entryM testMT 2 3) (entryM testM 3 2)
  -- diagonal entries are unchanged by transpose.
  check "M3 transpose: diagonal entry (1,1) unchanged (6.0)" $
    approxEq (entryM testMT 1 1) (entryM testM 1 1)
  check "M3 transpose: diagonal entry (3,3) unchanged (16.0)" $
    approxEq (entryM testMT 3 3) (entryM testM 3 3)

  -- transpose (transpose m) == m (double-transpose is identity).
  check "M3 transpose: transpose (transpose m) == m (approxEqMatrix, 1e-10)" $
    approxEqMatrix (M.transpose (M.transpose testM)) testM

  -- transpose identity == identity.
  check "M3 transpose: transpose M.identity == M.identity (1e-10)" $
    approxEqMatrix (M.transpose M.identity) M.identity

  -- ── (B) Builder.unprojectAtDepthFull round-trip under orbit ──────────────────
  let
    -- Canvas and projection used for the full-orbit unproject tests.
    m3Canvas = { w: 800.0, h: 600.0 }
    m3Proj = Cam.projection 1.0 800.0 600.0
    -- A non-trivial orbit (yaw=0.5, pitch=0.3 radians).
    m3Orb = Cam.orbit 0.5 0.3
    -- The combined view-projection: vp = proj * orbit.
    m3Vp = Cam.viewProjection { yaw: 0.5, pitch: 0.3 } 1.0 800.0 600.0
    -- A world reference point in front of the camera (negative Z in camera space
    -- at zoom 1.0; cameraDistance is 1000 so z ~ -120 is comfortably in view).
    m3WorldRef = { x: 40.0, y: -25.0, z: -120.0 }
    -- Project the reference point to a pixel using the FULL view-projection.
    m3Px = B.projectToScreen m3Vp m3Canvas m3WorldRef
    -- Unproject with the full-orbit variant; should recover worldRef.
    m3Back = B.unprojectAtDepthFull m3Orb m3Proj m3Canvas { x: m3Px.x, y: m3Px.y } m3WorldRef
    m3ApproxClose a c = abs (a - c) < 1.0e-6

  -- (B1) Round-trip: projectToScreen then unprojectAtDepthFull recovers the world point.
  check "M3 unprojectAtDepthFull: round-trip x ≈ worldRef.x (1e-6)" $
    m3ApproxClose m3Back.x m3WorldRef.x
  check "M3 unprojectAtDepthFull: round-trip y ≈ worldRef.y (1e-6)" $
    m3ApproxClose m3Back.y m3WorldRef.y
  check "M3 unprojectAtDepthFull: round-trip z ≈ worldRef.z (1e-6)" $
    m3ApproxClose m3Back.z m3WorldRef.z

  -- (B2) Offset pixel test: unproject a shifted pixel, then re-project the result;
  --      it should reproduce the shifted pixel (the unproject lands on the correct ray).
  let
    m3PxShifted = { x: m3Px.x + 30.0, y: m3Px.y - 20.0 }
    m3Back2 = B.unprojectAtDepthFull m3Orb m3Proj m3Canvas m3PxShifted m3WorldRef
    m3PxReproj = B.projectToScreen m3Vp m3Canvas m3Back2

  check "M3 unprojectAtDepthFull offset: re-projected x ≈ shifted px.x (1e-6)" $
    m3ApproxClose m3PxReproj.x m3PxShifted.x
  check "M3 unprojectAtDepthFull offset: re-projected y ≈ shifted px.y (1e-6)" $
    m3ApproxClose m3PxReproj.y m3PxShifted.y

  -- ── (C) Equivalence at zero orbit: unprojectAtDepthFull with orbit 0 0 == unprojectAtDepth ──
  let
    -- At zero orbit, orbit = identity, so unprojectAtDepthFull must collapse to unprojectAtDepth.
    m3Orb0 = Cam.orbit 0.0 0.0
    -- Use the same canvas/proj and a reference point with positive-Z (as in the
    -- existing legacy test, refPos = { x: 40.0, y: -25.0, z: 30.0 }) so
    -- unprojectAtDepth's diagonal assumption holds.
    m3RefPos0 = { x: 40.0, y: -25.0, z: 30.0 }
    m3Px0 = B.projectToScreen m3Proj m3Canvas m3RefPos0
    m3FullAtZero = B.unprojectAtDepthFull m3Orb0 m3Proj m3Canvas { x: m3Px0.x, y: m3Px0.y } m3RefPos0
    m3Legacy = B.unprojectAtDepth m3Proj m3Canvas { x: m3Px0.x, y: m3Px0.y } m3RefPos0

  check "M3 unprojectAtDepthFull orbit=0: x ≈ unprojectAtDepth x (1e-10)" $
    approxEq m3FullAtZero.x m3Legacy.x
  check "M3 unprojectAtDepthFull orbit=0: y ≈ unprojectAtDepth y (1e-10)" $
    approxEq m3FullAtZero.y m3Legacy.y
  check "M3 unprojectAtDepthFull orbit=0: z ≈ unprojectAtDepth z (1e-10)" $
    approxEq m3FullAtZero.z m3Legacy.z

  -- ── (D) Camera.clampPitch ─────────────────────────────────────────────────────
  -- clampPitch clamps to ±maxPitch (≈ 85° in radians = 1.4835).

  -- Over-bound positive clamps to +maxPitch.
  check "M3 clampPitch (pi) == Camera.maxPitch (over-bound positive clamps to max)" $
    approxEq (Cam.clampPitch pi) Cam.maxPitch

  -- Over-bound negative clamps to -maxPitch.
  check "M3 clampPitch (-pi) == -Camera.maxPitch (over-bound negative clamps to -max)" $
    approxEq (Cam.clampPitch (negate pi)) (negate Cam.maxPitch)

  -- In-range value passes through unchanged.
  check "M3 clampPitch 0.2 == 0.2 (in-range unchanged, 1e-10)" $
    approxEq (Cam.clampPitch 0.2) 0.2

  -- maxPitch is close to 85° in radians (1.4835 ± 0.01).
  check "M3 Camera.maxPitch ≈ 1.4835 (85° in radians, within 0.01)" $
    abs (Cam.maxPitch - 1.4835) < 0.01

  -- clampPitch at exactly ±maxPitch is a fixed point.
  check "M3 clampPitch maxPitch == maxPitch (fixed point at boundary)" $
    approxEq (Cam.clampPitch Cam.maxPitch) Cam.maxPitch
  check "M3 clampPitch (-maxPitch) == -maxPitch (fixed point at -boundary)" $
    approxEq (Cam.clampPitch (negate Cam.maxPitch)) (negate Cam.maxPitch)

  -- Zero is in-range (passes through).
  check "M3 clampPitch 0.0 == 0.0 (zero is in-range)" $
    approxEq (Cam.clampPitch 0.0) 0.0

  log "all M3 (transpose / unprojectAtDepthFull / clampPitch) properties hold."

-- Fold applyZoomStep repeatedly with a fixed wheel delta, starting from `start`.
-- Used to assert repeated stepping stays clamped within [minZoom, maxZoom].
foldZoom :: Number -> Number -> Array Int -> Number
foldZoom delta start = foldl (\z _ -> Cam.applyZoomStep z delta) start

-- A pure perspective×camera projection for the pick/unproject round-trip test.
-- Mirrors the shape of Main.perspectiveProjection (perspective matrix composed
-- with a translation by -cameraDistance along -Z), built from Math.Matrix so the
-- test stays free of WebGL/Effect dependencies. fov = pi/3, near 1, far 2000,
-- camera 1000 back — the same constants the app uses.
testProjection :: Number -> Number -> M.Matrix Number
testProjection w h =
  let
    fov = pi / 3.0
    near = 1.0
    far = 2000.0
    cameraDistance = 1000.0
    aspect = w / h
    f = 1.0 / tan (fov / 2.0)
    p = fromMaybe (M.zeros 4 4) $ M.fromArray 4 4
      [ f / aspect
      , 0.0
      , 0.0
      , 0.0
      , 0.0
      , f
      , 0.0
      , 0.0
      , 0.0
      , 0.0
      , (far + near) / (near - far)
      , (2.0 * far * near) / (near - far)
      , 0.0
      , 0.0
      , -1.0
      , 0.0
      ]
  in
    M.multiply p (M.translate 0.0 0.0 (-cameraDistance))

-- Extract every Nth element starting at `start` (used to pluck x/y/z columns).
everyNth :: Int -> Int -> Array Number -> Array Number
everyNth step start xs =
  map (\t -> t.v)
    $ filter (\t -> (t.i - start) `mod` step == 0 && t.i >= start)
    $ mapWithIndex (\i v -> { i, v }) xs

-- Read a vec3 at vertex index `vi` (positions are packed [x,y,z, x,y,z, ...]).
nth :: Array Number -> Int -> { x :: Number, y :: Number, z :: Number }
nth xs vi =
  { x: at (vi * 3)
  , y: at (vi * 3 + 1)
  , z: at (vi * 3 + 2)
  }
  where
  at i = fromMaybe 0.0 (index xs i)
