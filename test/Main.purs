module Test.Main where

import Prelude

import Data.Array (all, any, concat, filter, find, index, length, mapWithIndex, nub, range, zipWith)
import Data.Foldable (maximum, minimum, sum)
import Data.Int (toNumber)
import Data.Maybe (Maybe(..), fromMaybe, isJust, isNothing)
import FRP.Loop (emptyInput)
import Data.Number (abs, pi, sqrt, tan)
import Effect (Effect)
import Effect.Console (log)
import Effect.Exception (throw)
import Math.Matrix as M

import Atom (configString, electronPositions, electronPositionsBySubshell, electronPositionsBySubshell2D, electronShells, elementName, elementOf, fillSubshells, nucleusRadius, nucleons, shellRadius, subshellCap, subshellInclination, subshellRadius)
import Atom as Atom
import Chem (valence)
import Builder as B
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
  -- toggle now cycles CubePoc → Atomos → Molecule → Builder → CubePoc. RED
  -- until Scene.purs adds the `Builder` constructor + nextScene/sceneTitle
  -- cases.
  log "builder scene wiring properties:"

  -- The switch is now a 4-cycle through every scene; Molecule → Builder (not
  -- straight back to CubePoc), and Builder closes the loop to CubePoc.
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
