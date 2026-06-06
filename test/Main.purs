module Test.Main where

import Prelude

import Data.Array (all, any, filter, index, length, mapWithIndex, nub, range, sortBy, zipWith)
import Data.Foldable (maximum, minimum, sum)
import Data.Maybe (fromMaybe, isNothing)
import FRP.Loop (emptyInput)
import Data.Number (abs, pi, sqrt)
import Effect (Effect)
import Effect.Console (log)
import Effect.Exception (throw)
import Math.Matrix as M

import Atom (configString, electronPositions, electronShells, elementName, elementOf, fillSubshells, nucleusRadius, nucleons, shellRadius, subshellCap, subshellInclination, subshellRadius)
import Orbital (OrbShape(..), electronSites, occBrightness, orbitalAxis, orbitalViewportRadius, orbitalsFor, zEff, meanRadius, rScale)
import Meshes (angularValue, groundPlane, gridFloor, orbitRing, orbitalMesh, sphere)
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

  -- The on-screen switch toggles between the two scenes.
  check "nextScene toggles CubePoc <-> Atomos" $
    nextScene CubePoc == Atomos && nextScene Atomos == CubePoc

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

  -- ───── QM orbital model: occupancy + Slater Z_eff + radii (qm M1) ────
  log "QM orbital model properties:"

  let
    -- Occupancies of the (n,l) sub-shell's real orbitals, sorted high→low.
    occOf z nn ll =
      sortBy (\a b -> compare b a)
        (map _.occ (filter (\o -> o.n == nn && o.l == ll) (orbitalsFor z)))

  -- Real orbitals per sub-shell: s→1, p→3, d→5. Neon (1s 2s 2p) = 5 orbitals.
  check "Neon has 5 real orbitals (1s,2s,2p×3)" $ length (orbitalsFor 10) == 5
  check "2p sub-shell has 3 real orbitals (Carbon)" $
    length (filter (\o -> o.n == 2 && o.l == 1) (orbitalsFor 6)) == 3
  check "3d sub-shell has 5 real orbitals (Iron)" $
    length (filter (\o -> o.n == 3 && o.l == 2) (orbitalsFor 26)) == 5

  -- Hund + Pauli occupancy anchors (singly fill degenerate orbitals first).
  check "Nitrogen 2p occupancy = [1,1,1] (Hund)" $ occOf 7 2 1 == [ 1, 1, 1 ]
  check "Oxygen 2p occupancy = [2,1,1] (one pair)" $ occOf 8 2 1 == [ 2, 1, 1 ]
  check "Neon 2p occupancy = [2,2,2] (full)" $ occOf 10 2 1 == [ 2, 2, 2 ]
  check "Iron 3d occupancy = [2,1,1,1,1] (Hund)" $ occOf 26 3 2 == [ 2, 1, 1, 1, 1 ]

  -- Occupancies always sum to the (clamped) electron count.
  check "orbital occupancies sum to Z (Nitrogen 7)" $
    sum (map _.occ (orbitalsFor 7)) == 7
  check "orbital occupancies sum to Z (Krypton 36)" $
    sum (map _.occ (orbitalsFor 36)) == 36

  -- The 2p orbitals are the three real p shapes Px/Py/Pz.
  check "2p orbitals are Px,Py,Pz" $
    nub (map _.kind (filter (\o -> o.n == 2 && o.l == 1) (orbitalsFor 10)))
      == [ Px, Py, Pz ]

  -- Slater's rules: Carbon Z_eff anchors (textbook values).
  check "Carbon 2p Z_eff = 3.25 (Slater)" $ approxEq (zEff 6 2 1) 3.25
  check "Carbon 1s Z_eff = 5.70 (Slater)" $ approxEq (zEff 6 1 0) 5.70

  -- Mean radius (n²/Z_eff, unnormalized) shrinks as Z grows: He 1s ≫ Ne 1s.
  check "1s mean radius shrinks with Z (He > Ne)" $
    meanRadius 2 1 0 > meanRadius 10 1 0

  -- Normalized rScale grows with n within an element (Argon 1s < 2s < 3s).
  check "rScale grows with n within an atom (Ar 1s<2s<3s)" $
    rScale 18 1 0 < rScale 18 2 0 && rScale 18 2 0 < rScale 18 3 0

  log "all QM orbital model properties hold."

  -- ───── QM orbital-shape geometry (qm M2) ────────────────────────────
  log "QM orbital shape geometry properties:"

  -- angularValue is the real-orbital angular function f(d) for a unit direction.
  check "angularValue S = 1 (spherical) anywhere" $
    approxEq (angularValue S 0.3 0.4 0.5) 1.0
  check "angularValue Pz: +1 on +z axis, 0 on +x" $
    approxEq (angularValue Pz 0.0 0.0 1.0) 1.0 && approxEq (angularValue Pz 1.0 0.0 0.0) 0.0
  check "angularValue Px: +1 on +x axis, 0 on +z" $
    approxEq (angularValue Px 1.0 0.0 0.0) 1.0 && approxEq (angularValue Px 0.0 0.0 1.0) 0.0
  check "angularValue Dz2 = 2 on +z axis, -1 on +x" $
    approxEq (angularValue Dz2 0.0 0.0 1.0) 2.0 && approxEq (angularValue Dz2 1.0 0.0 0.0) (-1.0)

  let
    latSeg = 12
    longSeg = 12
    base = 100.0
    sMesh = orbitalMesh latSeg longSeg base S
    pzMesh = orbitalMesh latSeg longSeg base Pz
    pxMesh = orbitalMesh latSeg longSeg base Px
    dxyMesh = orbitalMesh latSeg longSeg base Dx2y2
    nVerts = (latSeg + 1) * (longSeg + 1)
    comp arr j c = fromMaybe 0.0 (index arr (3 * j + c))
    maxAbs spec c =
      fromMaybe 0.0 (maximum (map (\j -> abs (comp spec.vertices j c)) (range 0 (nVerts - 1))))
    vDist spec j = sqrt (comp spec.vertices j 0 * comp spec.vertices j 0 + comp spec.vertices j 1 * comp spec.vertices j 1 + comp spec.vertices j 2 * comp spec.vertices j 2)
    nLen spec j = sqrt (comp spec.normals j 0 * comp spec.normals j 0 + comp spec.normals j 1 * comp spec.normals j 1 + comp spec.normals j 2 * comp spec.normals j 2)

  -- Mesh validity (same invariants as the UV sphere).
  check "orbitalMesh vertex/normal counts match the UV grid" $
    length sMesh.vertices == nVerts * 3 && length sMesh.normals == nVerts * 3
  check "orbitalMesh index count = lat·long·6" $
    length pzMesh.indices == latSeg * longSeg * 6

  -- The s orbital is a sphere of radius `base`.
  check "S orbital is a sphere of radius base" $
    all (\j -> approxEq (vDist sMesh j) base) (range 0 (nVerts - 1))

  -- pz is a dumbbell along z; px along x.
  check "Pz orbital extends along z (dumbbell)" $
    maxAbs pzMesh 2 > maxAbs pzMesh 0 && maxAbs pzMesh 2 > maxAbs pzMesh 1
  check "Px orbital extends along x (dumbbell)" $
    maxAbs pxMesh 0 > maxAbs pxMesh 1 && maxAbs pxMesh 0 > maxAbs pxMesh 2

  -- dx²−y² has four lobes along ±x and ±y, ~nothing along z.
  check "Dx2y2 lobes along x and y, not z" $
    maxAbs dxyMesh 0 > maxAbs dxyMesh 2 && maxAbs dxyMesh 1 > maxAbs dxyMesh 2

  -- Recomputed normals are unit length (no NaN at angular nodes).
  check "Pz orbital normals are unit length" $
    all (\j -> approxEq (nLen pzMesh j) 1.0) (range 0 (nVerts - 1))
  check "Dx2y2 orbital normals are unit length" $
    all (\j -> approxEq (nLen dxyMesh j) 1.0) (range 0 (nVerts - 1))

  log "all QM orbital shape geometry properties hold."

  -- ───── Hund occupancy brightness (qm M4) ────────────────────────────
  log "Hund occupancy brightness properties:"

  -- A paired orbital (occ 2) renders brighter than a singly-occupied one
  -- (occ 1, Hund), which is brighter than an empty one — so Hund's rule reads
  -- visually. Empty orbitals are not drawn at all (brightness 0).
  check "occBrightness increases with occupancy (0<1<2)" $
    occBrightness 0 < occBrightness 1 && occBrightness 1 < occBrightness 2
  check "occBrightness 0 is dark (unrendered)" $ approxEq (occBrightness 0) 0.0
  check "occBrightness 1 is half-brightness (Hund dim)" $ approxEq (occBrightness 1) 0.5
  check "occBrightness 2 is full brightness" $ approxEq (occBrightness 2) 1.0

  log "all Hund occupancy brightness properties hold."

  -- ───── Electron particle sites (electron-particles M1) ──────────────
  log "electron particle model properties:"

  let
    z0 = { x: 0.0, y: 0.0, z: 0.0 }
    d3 v = sqrt (v.x * v.x + v.y * v.y + v.z * v.z)

  -- One discrete particle per electron: the count equals the (clamped) Z.
  check "electronSites count == Z (H 1)" $ length (electronSites 1) == 1
  check "electronSites count == Z (C 6)" $ length (electronSites 6) == 6
  check "electronSites count == Z (Fe 26)" $ length (electronSites 26) == 26
  check "electronSites count == Z (Kr 36)" $ length (electronSites 36) == 36

  -- Hydrogen's lone electron sits on the outermost orbital radius.
  check "Hydrogen electron at the outermost radius (~380)" $
    approxEq (d3 (fromMaybe z0 (index (electronSites 1) 0))) orbitalViewportRadius

  -- Lobe-tip axes are unit vectors, with the canonical p directions.
  check "orbitalAxis Px/Py/Pz are the +x/+y/+z units" $
    orbitalAxis Px == { x: 1.0, y: 0.0, z: 0.0 }
      && orbitalAxis Py == { x: 0.0, y: 1.0, z: 0.0 }
      && orbitalAxis Pz == { x: 0.0, y: 0.0, z: 1.0 }
  check "every orbitalAxis is a unit vector" $
    all (\sh -> approxEq (d3 (orbitalAxis sh)) 1.0)
      [ S, Px, Py, Pz, Dz2, Dxz, Dyz, Dx2y2, Dxy ]

  -- A paired orbital (Helium 1s²) contributes an antipodal ± pair.
  check "Helium 1s² gives an antipodal electron pair" $
    let
      ss = electronSites 2
      a = fromMaybe z0 (index ss 0)
      b = fromMaybe z0 (index ss 1)
    in
      length ss == 2
        && approxEq (a.x + b.x) 0.0
        && approxEq (a.y + b.y) 0.0
        && approxEq (a.z + b.z) 0.0
        && approxEq (d3 a) (d3 b)

  -- Out-of-range Z is clamped (no crash).
  check "electronSites clamps high (999 → 36)" $ length (electronSites 999) == 36
  check "electronSites clamps low (0 → 1)" $ length (electronSites 0) == 1

  log "all electron particle model properties hold."

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
