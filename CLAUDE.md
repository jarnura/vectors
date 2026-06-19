# vectors

A PureScript + WebGL2 3D graphics demo — a vertical-sliced
**"learn matter" learning platform** (atoms → chemistry → properties of matter) —
with four **scenes**, toggled by an on-screen switch (`nextScene` 4-cycles):
- **Cube POC** — a solid-lit main cube + orbiting satellite inside a world
  backdrop (green ground, wireframe grid, sky-blue horizon), with
  mouse/keyboard rotation and a **Shear** control.
- **atomos** — a 3D atom visualizer in near-black space with a starfield: a
  nucleus of proton (red) + neutron (gray) spheres and electrons on animated
  orbits. The atom is **configurable by element** (Z = 1..36, H…Kr) via a
  selector. Electrons fill **sub-shells** in Madelung/Aufbau order (each capped
  at 4ℓ+2), so e.g. Krypton resolves to `1s² 2s² 2p⁶ 3s² 3p⁶ 4s² 3d¹⁰ 4p⁶`. Each
  sub-shell draws a **thin orbital ring line**, and **discrete electrons** (bright
  spheres) orbit on those rings (animated by frame). Rings + electrons are
  **colour-coded by shell** — each principal shell n a distinct colour, with
  sub-shells the same hue but **lighter** outward (see `Palette`). An
  **orbital-info overlay** shows the live electron configuration. A **2D
  toggle** (`#view-2d` checkbox) flattens the atom into a Bohr diagram —
  orbital rings become concentric circles in the XY plane facing the camera,
  electrons ride those flat circles, and the nucleus flattens to the plane;
  unchecking restores the tilted 3D orbital system (sub-shell colours kept in
  both views). A **Sub-shells toggle** (`#subshell-view` checkbox, checked by
  default) switches between the sub-shell ring view (one ring per filled
  sub-shell, sub-shell colours via `Palette.subshellColor`) and a shell-only
  view (one ring per principal shell n, collapsed from all sub-shells, coloured
  by `Palette.shellColor` n); the toggle composes with the 2D view (4 combos).
  (Idealized Madelung; Cr/Cu anomalies not modeled.)
- **Molecule** — a 3D **H₂ molecule** in the same near-black space: two hydrogen
  nuclei whose covalent bond is rendered as a **shared electron pair** sitting in
  the internuclear overlap (no stick), frame-animated. All controls are **anime.js**
  driven: a **#bond-btn** ("Form bond") runs an anime.js bond-formation animation
  that draws the two atoms together (tweening `State.bondProgress`), and an animated
  **#molecule-info details panel** is populated **data-driven** from the molecule's
  properties. The `Molecule` model is **open-ended** — a molecule registry of
  records (atoms/bonds/properties) — so H₂ is just the first entry of a growing set.
  This is **slice 1** of the chemistry layer (a fixed H₂ molecule).
- **Builder** — **slice 2**: an interactive **molecule-builder sandbox**. **Atoms spawn distributed in 3D** via the pure `Builder.spawnPos` (golden-angle/Fibonacci shell layout, latitude band cycles every 12 atoms so spheres never collapse to poles, radius grows with atom count), replacing the old collinear line. From a modern, **glassy (backdrop-blur) anime.js-animated control bar** you **Add** atoms of any element (Z = 1..36) and **Clear** the world. **Dragging EMPTY SPACE orbits the camera** (yaw/pitch via `Camera.orbit/viewProjection`, pitch clamped to `Camera.maxPitch`; the Builder render + pointer pick share the single orbit transform `Main.projectionFor`); you can also click on-screen **orbit buttons** (`#orbit-up/#orbit-down/#orbit-left/#orbit-right` + a `#orbit-reset` button) in the controls drawer to orbit the camera by a fixed step per click (~10° per click, via `Camera.buttonOrbitDelta`) — the on-screen analogue of the orbit-drag, mirroring the existing `#zoom-in`/`#zoom-out` buttons; all orbit controls are **Builder-only** (orbit has yaw+pitch DOF only, no roll, so this is the complete control set). **dragging an ATOM moves it through TRUE world depth** (a **double-click on an atom then drag** uses `Builder.unprojectAtDepthFull`, unrotating through the orbit so atom motion stays orthogonal to view axes). A **single-click + drag moves the WHOLE connected molecule** (every bonded atom translates rigidly together — relative geometry + internal bonds preserved, via `Builder.moveMolecule` over the atom's connected component), while a **double-click on an atom then drag moves just that one atom** (`Builder.moveAtomWith`, detected by the native mouse click count, `event.detail ≥ 2`); a lone atom moves the same either way. **Bonds now connect atoms across depth** — the bond endpoints adapt to the 3D orbit. A **#drag-strength slider** (0–10, default 3) in the drawer tunes how hard the drag pulls: double-click + drag with high strength can tear bonds apart (each bond resists via its `Chem.bondEnergy`); low strength (↓0) holds all bonds, tugging the whole connected component along; at 0 nothing rips, at 10 all bonds yield. Single-click + drag (whole molecule via `Builder.moveMolecule`) is unaffected. The Builder background is **plain near-black space (no starfield)** — only atomos and Molecule keep their starfields. **Atoms obey a Pauli-exclusion-inspired no-overlap constraint**: no two atom centres ever sit closer than their per-element-pair contact floor (via pure `minSeparation`, derived from summed `Atom.atomicRadius`), enforced by `Builder.resolveOverlaps` before bonding; the constraint is anchor-aware (dragged atoms/molecules win, others yield) and deterministic (idempotent on valid states, no drag jitter). The shared covalent pair remains the one allowed overlap, so auto-bonding still works. When atoms come near each other they **auto-bond, valence-aware** (H=1, C=4, N=3, O=2, … via `Chem.valence`) with **break hysteresis** (bonds form under `bondThreshold`, break only past the wider `breakThreshold`). Connected atoms become a **molecule** with a derived Hill-style Unicode formula (e.g. `H₂O`). Each placed atom renders its **real per-element nucleus** (via `Atom.nucleons`, so e.g. Carbon/Oxygen show a denser/larger nucleus than Hydrogen). Atoms also render at **different sizes by element** — the atomic-layer ball is scaled by the pure `Atom.atomicRadius` (normalised Cordero covalent radii: Hydrogen smallest, Carbon/Oxygen larger) — and each atom shows its **atomic symbol** (H, C, O, …, `Atom.symbolOf`) as an **HTML overlay label** (the `Labels` FFI, projected per atom, fading with the ball). A covalent bond shows a **shared electron pair** in the bond, while each atom carries only its **lone electrons** (valence − bonded degree), so the total electron count is conserved (`Σ valence`). Electrons are **colour-coded by role**: an atom's **valence** electrons (its outermost shell plus the shared bonding pair) render in a distinct **amber**, vs its **core** (inner-shell) electrons which stay **blue** — the valence shell is taken from the element configuration (`Atom.electronShells`, last shell), so e.g. Carbon (shells `[2,4]`) shows 2 blue core electrons on the inner ring and 4 amber valence electrons on the outer ring. A **"Valence only" toggle** (`#valence-only` checkbox) hides the core (inner-shell, blue) electrons, leaving only the valence electrons (amber outer-shell lone + the amber shared bonding pair) — Builder-only; other scenes unaffected. The dynamic world lives in a pure model (`Builder`) behind a single shared `Ref` source of truth (`BuilderApi`), also exposed as a `window.__builder` test API (extended with `setOrbit`/`getOrbit` for camera control). The orbit angles are kept in a separate `Ref` owned by `Main` (camera state, not part of the pure model) and exposed via `OrbitApi`. The controls live in a **left drawer**, opened by a `#panel-toggle` icon (top-left, below the scene title) that slides the glassy panel in from the left via anime.js on click and back out on a second click. **Zoom drives a smooth level-of-detail (LOD)** on each placed atom: zoom **OUT** and each atom collapses to a single **element-coloured ball**; zoom **IN** and it blooms into its real nucleus (`Atom.nucleons`) + electrons. The in-between is a **continuous, frame-eased cross-fade** — even the `#zoom-in`/`#zoom-out` button steps animate smoothly (no abrupt swap, no zoom reset; drag/interaction preserved). It **reuses the existing zoom input** (mouse wheel + zoom buttons → `Input.zoomDelta`) — no new channel; the detail factor is a pure function of zoom (`Layer.layerBlend`), eased per frame (`State.detail`). Bonds stay visible in both layers: the **atomic (zoomed-out) layer** draws each bond as a **connecting line** between atom-balls (a pre-built `Meshes.unitBeam` per `Builder.bondSegments`, fading as you zoom into the sub-atomic layer), while the **sub-atomic layer** keeps the **shared electron-pair** bond spheres. The "Valence only" toggle composes with the LOD.

Each fundamental particle is a sphere. Perspective projection + canvas-resize
throughout. **Zoom** works across all scenes — scroll out to pull the camera back
(e.g. to see many molecules at once), scroll in for detail (a `zoom` factor scales
the effective camera distance via the pure `Camera` module). Besides the **mouse
wheel**, the `#controls` panel has on-screen **`#zoom-in` ("+") / `#zoom-out` ("−")
buttons** that drive the same zoom: a click pushes a fixed synthetic wheel delta
(`Camera.buttonZoomDelta`) into the same `Input.zoomDelta` channel, so both share
`Main.applyZoom` → `Camera.applyZoomStep` (clamped to `minZoom`/`maxZoom`). + zooms
in, − zooms out.

## Commands

| Command | What it does |
|---------|--------------|
| `npm run build` | `spago bundle` → browser bundle at `dist/index.js` |
| `npm test` | `spago test` → runs `Test.Main` |
| `npm run dev` | bundle, then `node dev-server.js` (static server on `0.0.0.0:47474`) |
| `npm run e2e` | Playwright canvas-verification E2E suite (boots the dev server) |
| `npm run docker:build` / `npm run docker:run` | Build + run the static site as a local Docker container (multi-stage glibc-Node builder → nginx:alpine runtime, on `localhost:8080`) |

Toolchain: spago + purs 0.15.16 + esbuild, driven via npm. PureScript deps are
declared in `spago.yaml`; npm holds the dev toolchain, Playwright, and
**animejs** (used via the `Text` and `Controls` FFIs for HTML overlay text and
molecule controls only — never WebGL).

## Architecture

Entry: `index.html` loads `dist/index.js` (the esbuild bundle, gitignored),
which boots the `Main` module.

Module map (under `src/`):

| Module | Files | Role |
|--------|-------|------|
| `Main` | `Main.purs` | Entry point; wires canvas, renderer, loop, input; builds per-scene `Entity` lists and selects on `State.scene`. `EntityMesh = Solid \| Wire` dispatch. **Builder orbit**: `projectionFor s w h` returns `Camera.viewProjection` (orbit-aware) for Builder scene, plain `Camera.projection` for others; `applyOrbit delta o` mutates an orbit Ref by `{yaw,pitch}` delta (pitch clamped); dragging empty space in Builder calls `applyOrbit` (via Input channel fed by `installOrbitDrag` in FRP.Loop), the on-screen `#orbit-*` buttons are wired (Builder-scene-gated) to `applyOrbit` on the shared orbit Ref (`#orbit-reset` writes `{yaw:0, pitch:0}`), dragging atoms calls `unprojectAtDepthFull` with the orbit transform. The draw loop re-projects when **zoom, size, or orbit** changes (sizeRef gate now tracks yaw/pitch). Uses `Camera.projection` (non-Builder) or `projectionFor` (Builder) for the perspective projection; the draw loop re-projects when **zoom or size** changes (`applyZoom` folds wheel input via `Camera.applyZoomStep`). Molecule scene renders two nuclei + the shared electron pair model-driven; `#bond-btn` runs the anime.js bond animation (drawing atoms together via `State.bondProgress`); `updateOverlay` shows `#molecule-info` only in the molecule scene. Atomos render selects sub-shell vs shell-only ring geometry on `State.subshellView` × `State.view2D` (4 combos; both geometries built once at init); pure `applySubshellView` composed into step. Builder scene renders placed atoms + bonds (instanced/reused meshes built once at init, placed each frame from the shared `Ref`); `installBuilderPick` does a 3D pointer pick + drag (scene-gated to Builder so cube rotation is unaffected; picks and drags both go through `projectionFor s w h` so orbit-aware) → live valence-aware re-bonding; the pick latches a `whole` flag from the mouse click count (`event.detail < 2` ⇒ single-click ⇒ drag the **whole molecule** via `Builder.moveMolecule`; `event.detail ≥ 2` ⇒ double-click ⇒ drag a **single atom** via `Builder.moveAtomWith s.dragStrength`, reading the live strength from `State.dragStrength`; dragging empty space does NOT pick/drag, instead orbits the camera via `applyOrbit`). When `State.valenceOnly`, the Builder render drops the core-electron group (Builder-only). Builder zoom LOD: `State.detail` (a Number) is eased each frame toward `Layer.layerBlend s.zoom` by a pure `applyDetail` composed right after `applyZoom`; the Builder render branch cross-fades each atom — the element ball scaled by `(1 − detail)` (and by `Atom.atomicRadius` per element) ⇄ the nucleus+electrons interpolated out from the atom centre and radius-scaled by `detail`. In the atomic layer it also draws **bond lines** (a reused `Meshes.unitBeam` placed per `Builder.bondSegments`, fading with `(1 − detail)`) and **syncs per-atom symbol labels** in the draw loop (`syncAtomLabels` → `Labels.syncAtomLabels`, each label projected via `Camera.projection` + `Builder.projectToScreen`, text = `Atom.symbolOf`, opacity `(1 − detail)`; cleared on leaving Builder). Wires `Controls.installPanelToggle` for the left-drawer controls panel |
| `Camera` | `Camera.purs` | Pure **camera** module (imports `Math.Matrix` only; no `Effect`/WebGL): single source of truth for the camera constants (`fov`, `cameraDistance`, `clipNear`, `clipFar`) + `minZoom`/`maxZoom` (0.2–5.0). `projection zoom w h` builds a zoom-aware perspective projection (effective camera distance = `cameraDistance/zoom`; byte-identical to the old projection at zoom 1.0); `clampZoom` clamps to range; `applyZoomStep` applies a multiplicative wheel step; `buttonZoomDelta` is the fixed synthetic wheel-delta magnitude the on-screen `#zoom-in`/`#zoom-out` buttons push through `applyZoomStep`. **Builder orbit**: `orbit yaw pitch` builds a view rotation (yaw about Y + pitch about X, both in radians; `orbit 0 0` is byte-identical to identity); `viewProjection {yaw,pitch} zoom w h` is projection composed with orbit (collapses to plain projection at zero orbit); `maxPitch` / `clampPitch` constrain pitch to ±≈85° so the camera never flips over the poles; `buttonOrbitDelta` is the fixed per-click orbit step the on-screen `#orbit-up/#orbit-down/#orbit-left/#orbit-right` buttons push through `Main.applyOrbit`, mirroring `buttonZoomDelta`. Replaces the old `Main.perspectiveProjection` |
| `Graphics.GL` | `GL.purs` + `GL.js` | WebGL2 FFI: renderer, meshes, colors, clear color, draw calls |
| `Math.Matrix` | `Math/Matrix.purs` | Matrix linear algebra (multiply, projection, `translate`, `scale`, `shear`, `transpose`, etc.) |
| `Vector` | `Vector.purs` | Rotation matrices (`rotateX/Y/Z`) and vector ops |
| `Meshes` | `Meshes.purs` | Geometry specs: cubes, world (`groundPlane`, `gridFloor`), `sphere` (particles/stars), `orbitRing` (thin per-sub-shell orbital ring line), `orbitRingFlat` (flat XY-plane ring for the 2D Bohr view), and `unitBeam` (a unit-length beam mesh placed per `Builder.bondSegments` for the atomic-layer bond lines) |
| `World` | `World.purs` | Static world-backdrop constants/transforms (`groundTransform`, `gridTransform`, `skyColor`) |
| `Scene` | `Scene.purs` | `Scene = CubePoc \| Atomos \| Molecule \| Builder` (4 scenes), `nextScene` (4-cycles), `sceneTitle`, atomos `spaceColor` |
| `Layer` | `Layer.purs` | Pure **continuous level-of-detail (LOD)** model for the Builder zoom cross-fade (no `Effect`/WebGL; imports only Prelude): `smoothstep` (clamped Hermite) + a detail band `detailLo`/`detailHi` + `layerBlend :: Number -> Number` (zoom → detail factor d in [0,1], 0 = atom balls, 1 = sub-atomic detail) + `easeDetail` (one frame of exponential smoothing toward a target) + `detailName`. No discrete ladder |
| `Atom` | `Atom.purs` | Element table (Z=1..36, H…Kr) + Madelung sub-shell filling (`fillSubshells`/`subshellCap`/`configString`, per-shell totals via `electronShells`) + nucleon cluster + `electronPositions` (discrete electrons on per-sub-shell orbital rings, `subshellRadius`/`subshellInclination`; `electronPositionsBySubshell2D` gives flat XY-plane positions for the 2D Bohr view); `electronPositionsByShell` / `electronPositionsByShell2D` give shell-collapsed positions (all electrons on their principal shell ring, 3D + flat); `shellRings` returns occupied principal shell rings with radii; `clampElectron` exported for electron count clamping; `symbolOf` (element symbol string) + `atomicRadius` (normalised Cordero covalent radius per Z — Builder per-element atom sizing) |
| `Molecule` | `Molecule.purs` | Pure, open-ended molecule model: records `Bond {a,b,order,shared}` / `MolAtom {element,center}` / `Property {label,value}` / `Molecule {name,formula,atoms,bonds,properties}`; a `molecules` registry + total/clamp-safe `moleculeOf`; `bondLength`; `sharedElectronPositions` (the shared covalent pair in the internuclear overlap, frame-animated); `moleculeNucleons` (reuses `Atom.nucleons` per atom, translated). Imports `Atom` only |
| `Chem` | `Chem.purs` | Pure, clamp-safe **valence table** `valence :: Int -> Int` for Z = 1..36 (H…Kr). Main-group elements use their common covalent valence; transition metals (Sc…Zn) use a documented flat default of 2. Used to cap bond formation in `Builder`. Also a **normalised single-bond energy table** `bondEnergy :: Int -> Int -> Number` (kJ/mol ÷ 100) for common pairs (O–H 4.63, H–H 4.36, C–H 4.13, C–C 3.46 … weak O–O 1.46, N–N 1.63, F–F 1.55); untabulated pairs use geometric-mean fallback, with documented teaching simplifications. Consumed by `Builder.pullBonds` to decide whether a stretched bond holds (energy ≥ drag strength) or breaks |
| `Builder` | `Builder.purs` | Pure **dynamic world model** for the builder sandbox: `PlacedAtom {id,z,pos}` / `BBond {a,b}` / `BuilderState {atoms,bonds,nextId,picked}`; **spawn**: `spawnPos i` generates a deterministic 3D position (golden-angle / Fibonacci shell, latitude band cycles every 12 atoms to avoid collapse at poles, radius ∝ sqrt i) for the atom at insertion index i; constraint operations `addAtom`/`moveAtom`/`moveMolecule` (each calls `resolveOverlaps` BEFORE `recomputeBonds`, enforcing the Pauli-exclusion no-overlap constraint — addAtom: the new atom yields; moveAtom: the dragged atom wins; moveMolecule: the dragged component wins) + `clear`; `recomputeBonds` (proximity + valence-capped via `Chem` + break hysteresis, `bondThreshold`/`breakThreshold`); strength-aware variant `moveAtomWith :: Number -> Int -> V3 -> BuilderState -> BuilderState` (used by single-atom drag) → `pullBonds strength` (`pullPasses` = 10 Gauss-Seidel passes, bounded tension so bonds whose `bondEnergy ≥ strength` hold and tug partners along; `pullRestLen` = max(minSeparation, slack-adjusted bondThreshold) so Pauli floor is never violated) → `resolveOverlaps` → `recomputeBonds`; legacy `moveAtom = moveAtomWith strengthInfinity` (∞ strength ⇒ no bond can hold ⇒ every stretched bond breaks, the legacy distance-only behaviour); `minSeparation` / `resolveOverlaps` (per-element-pair contact floor from `Atom.atomicRadius`, bounded relaxation solver with anchor-aware deterministic separation, idempotent on valid states); `molecules` (connected components) + `componentOf` + `moveMolecule` (rigidly translate the whole component of an anchor atom so the anchor lands at a target — same delta for every component atom, internal bonds preserved — then `recomputeBonds`; a lone atom reduces to `moveAtom`) + `formulaOf` (Hill-style Unicode formula); `bondMidpoints`; electron helpers `degreeOf`/`loneCountOf` + `bondElectronPositions`/`loneElectronPositions` (shared pair in each bond and remaining lone electrons on each atom, electron count conserved); valence/core split helpers `valenceShellOf` + `coreLoneElectronPositions`/`valenceLoneElectronPositions` (for amber/blue colour split); grouped electron helpers `coreLoneElectronGroups`/`valenceLoneElectronGroups`/`bondElectronGroups` (consumed by LOD bloom cross-fade); `bondSegments` (per-bond endpoint pairs for atomic-layer bond lines); pure `projectToScreen`/`unprojectAtDepth` pick/unproject helpers; **true-depth drag**: `unprojectAtDepthFull orbit proj canvas px worldRef` unprojects a pixel through an orbit-aware camera (converts world coords to view space via the orbit matrix, unprojects in view space, rotates back via orbit transpose so motion stays orthogonal to view axes). No WebGL/`Effect` |
| `BuilderApi` | `BuilderApi.purs` + `BuilderApi.js` | The **single shared `Ref BuilderState` source of truth** the renderer, the in-app Add/Clear buttons, the mouse drag, and a `window.__builder` test API (`addAtom`/`moveAtom`/`moveAtomWith`/`moveMolecule`/`getAtoms`/`getBonds`/`getMolecules`/`clear`) all read/write. Each mutation fires an `onChange` callback for eager re-render. `installBuilderControls` wires `#add-btn`/`#clear-btn`. DOM-only FFI, never WebGL |
| `OrbitApi` | `OrbitApi.purs` + `OrbitApi.js` | The **Builder camera-orbit seam** extending `window.__builder` with `setOrbit(yaw, pitch)` / `getOrbit()` methods over a single shared orbit `Ref` (held by `Main`, kept outside the pure model). Pitch is clamped on write by `Camera.clampPitch`. The E2E seam reads/writes the same `Ref` the renderer and the orbit-drag use (single source of truth). DOM-only FFI, never WebGL |
| `Controls` | `Controls.purs` + `Controls.js` | DOM-only **anime.js** controls FFI (imports only animejs; never WebGL): `renderInfoPanel` (data-driven `#molecule-info` rows + anime.js stagger reveal), `installBondButton`, `runBondAnimation` (tweens a JS value → `State.bondProgress`), `animateControlBarIn` (glassy control-bar entrance animation), `installPanelToggle` (a `#panel-toggle` icon that slides the `#controls` panel in/out as a left drawer via anime.js; closed drawer is `pointer-events:none` + off-screen so it never blocks the canvas), `installButtonPulse` (click-pulse bounce on a button by id). Sibling to `Text` |
| `Palette` | `Palette.purs` | Shell/sub-shell colours: `shellColor n` (distinct per shell) + `subshellColor n l` (shell hue, lighter by ℓ). Pure |
| `Starfield` | `Starfield.purs` | Deterministic Fibonacci-sphere star positions for the atomos backdrop |
| `Text` | `Text.purs` + `Text.js` | anime.js **HTML overlay-text** FFI (`scrambleInto`/`setVisible`) — DOM only, never WebGL. Drives the atomos element label, scene-title banner, and orbital-info (electron-configuration) overlay. (`Controls` is the sibling control FFI for the molecule scene, `Labels` the sibling per-atom overlay FFI for the Builder scene.) |
| `Labels` | `Labels.purs` + `Labels.js` | DOM-only **per-atom overlay-label** FFI (`syncAtomLabels`/`clearAtomLabels`) — HTML overlay text, never WebGL. Reconciles a pool of `.atom-label` divs in the `#atom-labels` container (one per atom id) against projected positions/opacity for the Builder atomic-symbol labels; `clearAtomLabels` empties it when leaving Builder. Sibling to `Text`/`Controls` |
| `FRP.Loop` | `FRP/Loop.purs` + `FRP/Loop.js` | rAF loop + input plumbing (keyboard, mouse, shear button, scene toggle, element selector, `#view-2d` checkbox via `installView2DToggle`, the `#valence-only` checkbox via `installValenceOnlyToggle` → `Input.toggleValenceOnly`, the `#subshell-view` checkbox via `installSubshellViewToggle` → `Input.toggleSubshellView`, the **`#drag-strength` slider** via `installDragStrengthSlider` → `Input.dragStrength` (a `Maybe Number` channel feeding `Main.applyDragStrength`), and a `bondProgress` channel via `installBondButton` → `Input`). Also a canvas-scoped **wheel FFI** (`installWheelListener`: `wheel` with `{passive:false}` + `preventDefault`, feeding `Input.zoomDelta`), the on-screen **zoom buttons FFI** (`installZoomButtons`: `#zoom-in`/`#zoom-out` click listeners that push ∓`Camera.buttonZoomDelta` into the same `Input.zoomDelta` channel as the wheel), the on-screen **orbit buttons FFI** (`installOrbitButtons`: `#orbit-up/#orbit-down/#orbit-left/#orbit-right` + `#orbit-reset` click listeners that fold a fixed `applyOrbit` delta into the shared orbit Ref, or reset to `{yaw:0, pitch:0}`; Builder-only), a canvas **pointer FFI** (`installCanvasPointer`: mousedown/move/up — the mousedown callback also passes the native click count `event.detail` so `Main.installBuilderPick` can tell single-click [whole molecule] from double-click [single atom]; dragging empty space in Builder feeds a `{ dx, dy }` delta into an `orbitDelta` Input channel, driving `Main.applyOrbit` yaw/pitch updates) and `installAddButton`/`installClearButton` for the Builder `#add-btn`/`#clear-btn`. Also `setBuilderDetail` (writes the per-frame eased `State.detail` to `window.__builderDetail` as a debug/E2E seam) |

State is a plain record (`transform`, `speed`, `mouseLast`, `frame`, `scene`,
`element`, `view2D`, `subshellView`, `valenceOnly`, `bondProgress`, `dragStrength`, `zoom`, `detail`, `builderYaw`, `builderPitch`, `builder`) advanced each
frame (the `Input` channel gained `toggle2D`, applied via `Main.applyToggle2D`,
`toggleSubshellView`, applied via the pure `Main.applySubshellView` (`subshellView`
initialises to `true`), `toggleValenceOnly`, applied via the pure `Main.applyValenceOnly` (`valenceOnly`
initialises to `false`), a `dragStrength` channel fed by the `#drag-strength` slider (applied via `Main.applyDragStrength`, initialises to `3.0`), a `bondProgress`
channel fed by the anime.js bond animation, an `orbitDelta` channel fed by empty-space drag in the Builder (applied via `Main.applyOrbit`, builderYaw/builderPitch initialise to `0.0`), and a `zoomDelta` channel fed by both the wheel
FFI and the on-screen `#zoom-in`/`#zoom-out` buttons (`installZoomButtons`), applied via
`Main.applyZoom` → `Camera.applyZoomStep`); updates return new records
rather than mutating. `zoom` initialises to `1.0`. `detail` (the Builder zoom-LOD
factor) initialises to `1.0`, eased each frame toward `Layer.layerBlend s.zoom` by
the pure `Main.applyDetail` composed right after `applyZoom`. `builderYaw` / `builderPitch` (orbit angles in radians) mirror a shared `Ref` in `Main` that the renderer, orbit-drag, and `OrbitApi` all read/write (camera state kept out of the pure model). The `builder` field mirrors the shared `BuilderApi` `Ref` (the Builder
world model), refreshed into the rendered state each frame and on eager
re-render. The world meshes, nucleus, and starfield use scene-/element-derived
transforms; electrons and the shared molecule pair advance with `frame`. The
on-screen `#controls` bar is a **glassy (backdrop-blur)** panel that now lives in
a **left drawer** toggled by a `#panel-toggle` icon (`Controls.installPanelToggle`,
anime.js slide-in/out); the old auto-show entrance (`animateControlBarIn`) is no
longer wired. Buttons keep their click pulses. An `#atom-labels` overlay
container (sibling to `#molecule-info`) holds the Builder per-atom symbol labels,
synced/cleared by the `Labels` FFI.

## Conventions

- Conventional commits: `feat:` / `refactor:` / `fix:` (also `test:`, `chore:`,
  `docs:`, `perf:`, `ci:`).
- Ship tests alongside behavior changes. Unit tests are hand-rolled assertions
  in `test/Main.purs` (`1e-10` tolerance) covering `Math.Matrix`, `Vector`,
  `Meshes` (world geometry), `World`, `Builder` (constraint ops + drag-strength bond dynamics + 3D spawn + orbit/unproject), and `Camera` (orbit, viewProjection, pitch clamp). Browser behavior is covered by
  Playwright canvas-verification specs in `e2e/` (run with `npm run e2e`;
  `playwright.config.js` sets `retries: 2` for SwiftShader render-timing
  robustness). The Builder scene is driven in E2E via the `window.__builder` test
  seam (`addAtom`/`moveAtom`/`moveAtomWith`/`getBonds`/`getMolecules`/`clear`/`setOrbit`/`getOrbit`), which shares the
  same `BuilderApi` `Ref` (world model) and orbit `Ref` (camera state) as the renderer so reads reflect mutations immediately;
  the Pauli-exclusion no-overlap constraint is verified in
  `builder-no-overlap.spec.js` (anchor-aware separation, deterministic resolution), drag-strength bond-breaking dynamics in `builder-drag-strength.spec.js` (6 scenarios), 3D spawn positions in `builder-3d-spawn.spec.js` (spread, determinism, Pauli floor), 3D orbit interaction in `builder-3d-orbit.spec.js` (camera rotation, true-depth drag, bonds across depth), and orbit buttons in `builder-orbit-buttons.spec.js` (directional orbit steps, reset); `Camera.buttonOrbitDelta` is covered by unit tests reusing `applyOrbit`.
  Untested: `Graphics.GL` FFI internals, `FRP.Loop` orbit drag. PureScript is
  formatted with `purs-tidy`.
- Do NOT commit build output. `dist/`, `output/`, `.spago/`, `node_modules/`
  are gitignored.
- Keep each FFI `.js` file paired with its `.purs` module, and type all FFI
  imports as `Effect` when they perform side effects.
- Prefer many small focused modules and immutable state (return new records,
  never mutate in place).

## CI / Deploy

- `.github/workflows/babysitter-ci.yml` — runs `npm ci`, `npm run build`,
  `npm test` on every PR and on push to `master` (and `workflow_dispatch`).
- `.github/workflows/jekyll-gh-pages.yml` — deploys to GitHub Pages on push to
  `master`.
- **Local Docker** (separate from the Pages deploy) — a root multi-stage
  `Dockerfile` builds the bundle in a **glibc Node builder** (`node:22-bookworm-slim`;
  installs `git` + `ca-certificates`, runs `npm ci` then `npm run build` =
  `spago bundle`) and serves the static output (`index.html` + `dist/`) from a tiny
  `nginx:1.27-alpine` runtime. `.dockerignore` trims the build context (keeping
  `package*.json`, `spago.yaml`, `spago.lock`, `src/`, `index.html`).
  `npm run docker:build` then `npm run docker:run` → `http://localhost:8080`.
  The builder **must** stay glibc — the `purescript` npm package ships a prebuilt
  glibc `purs` binary that won't run on Alpine/musl.

## Memory (Hindsight)

This project has a dedicated [Hindsight](https://github.com/vectorize-io/hindsight)
memory bank (`vectors`) holding the project's long-term context: git history
(5 epochs, 2019→present), architecture/conventions seeded from this file, and
the spago dependency list. It is exposed as an MCP server named `hindsight`.

- **Config lives at local scope**, not in the repo — the server URL + bearer
  token are in `~/.claude.json` (project-scoped `mcpServers`), so the secret is
  never committed. There is intentionally no `.mcp.json` in the tree. To check:
  `claude mcp get hindsight`. The MCP tools load automatically at session start.
- **At the start of work, `recall`** relevant context, scoped by tag, e.g.
  `recall(query="…", tags=["project:vectors"])`. Useful tags: `source:git-log`
  (provenance), `source:CLAUDE.md` (architecture/conventions, `type:directive`),
  `source:spago.yaml` (dependencies); epoch tags like `epoch:2026-webgl`.
- **At the end of durable work, `retain`** new facts with
  `tags=["project:vectors", …]` so the bank stays current. Memories tagged
  `status:uncommitted` describe in-progress WIP and go stale once committed —
  refresh them after committing.

## Babysitter

This project is onboarded for [babysitter](https://github.com/) orchestration.
The project profile lives at `.a5c/project-profile.json`.

- **Methodology:** evolutionary — incremental, refactor-friendly changes over
  big-bang rewrites.
- **Recommended skills / agents:**
  - `code-review` / code-reviewer
  - `tdd` / tdd-guide
  - `test-coverage` / test-generator
  - refactor-cleaner
  - `update-docs` / doc-updater
  - `e2e-testing` / e2e-runner
  - `build-fix` / build-error-resolver
  - architect
- **Autonomy:** autonomous for this project — proceed without prompting, but
  always break (request human confirmation) on **destructive git operations**
  and on **deploy**.
- **How to run:** `/babysitter:babysit`, or the babysitter CLI.
