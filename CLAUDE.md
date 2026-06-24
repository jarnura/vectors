# vectors

A PureScript + WebGL2 3D graphics demo — a vertical-sliced
**"learn matter" learning platform** (atoms → chemistry → properties of matter → element transformation) —
with six **scenes**, toggled by an on-screen switch (`nextScene` 6-cycles
CubePoc→Atomos→Molecule→Builder→Materials→Nuclide→CubePoc):
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
- **Builder** — **slice 2**: an interactive **molecule-builder sandbox**. **Atoms spawn distributed in 3D** via the pure `Builder.spawnPos` (golden-angle/Fibonacci shell layout, latitude band cycles every 12 atoms so spheres never collapse to poles, radius grows with atom count), replacing the old collinear line. From a modern, **glassy (backdrop-blur) anime.js-animated control bar** you **Add** atoms of any element (Z = 1..36) and **Clear** the world. **Dragging EMPTY SPACE orbits the camera** (yaw/pitch via `Camera.orbit/viewProjection`, pitch clamped to `Camera.maxPitch`; the Builder render + pointer pick share the single orbit transform `Main.projectionFor`); you can also click on-screen **orbit buttons** (`#orbit-up/#orbit-down/#orbit-left/#orbit-right` + a `#orbit-reset` button) in the controls drawer to orbit the camera by a fixed step per click (~10° per click, via `Camera.buttonOrbitDelta`) — the on-screen analogue of the orbit-drag; all orbit controls are **Builder and Materials** (orbit has yaw+pitch DOF only, no roll, so this is the complete control set). **dragging an ATOM moves it through TRUE world depth** (a **double-click on an atom then drag** uses `Builder.unprojectAtDepthFull`, unrotating through the orbit so atom motion stays orthogonal to view axes). A **single-click + drag moves the WHOLE connected molecule** (every bonded atom translates rigidly together — relative geometry + internal bonds preserved, via `Builder.moveMolecule` over the atom's connected component), while a **double-click on an atom then drag moves just that one atom** (`Builder.moveAtomWith`, detected by the native mouse click count, `event.detail ≥ 2`); a lone atom moves the same either way. **Bonds now connect atoms across depth** — the bond endpoints adapt to the 3D orbit. A **#drag-strength slider** (0–10, default 3) in the drawer tunes how hard the drag pulls: double-click + drag with high strength can tear bonds apart (each bond resists via its `Chem.bondEnergy`); low strength (↓0) holds all bonds, tugging the whole connected component along; at 0 nothing rips, at 10 all bonds yield. Single-click + drag (whole molecule via `Builder.moveMolecule`) is unaffected. A **#layer-space slider** (1.0–4.0, step 0.1, default 1.6) spreads atom CENTRES further apart (the effective world scale = `builderScale × layerSpace` applied to centre positions). Each atom's intra-atom structure (nucleus cluster + electron rings) is decoupled from this slider — it remains frozen at a constant world size (`builderScale × intraAtomLayerSpace`, where `intraAtomLayerSpace = 1.6` matches the slider default), so atoms look internally identical at every layer-space value, and the default view is byte-identical to the historic render. This decoupling gives more inter-atom spacing room without dispersing each atom's internal detail. The Builder background is **plain near-black space (no starfield)** — only atomos and Molecule keep their starfields. **Atoms obey a Pauli-exclusion-inspired no-overlap constraint**: no two atom centres ever sit closer than their per-element-pair contact floor (via pure `minSeparation`, derived from summed `Atom.atomicRadius`), enforced by `Builder.resolveOverlaps` before bonding; the constraint is anchor-aware (dragged atoms/molecules win, others yield) and deterministic (idempotent on valid states, no drag jitter). The shared covalent pair remains the one allowed overlap, so auto-bonding still works. When atoms come near each other they **auto-bond, valence-aware** (H=1, C=4, N=3, O=2, … via `Chem.valence`) with **break hysteresis** (bonds form under `bondThreshold`, break only past the wider `breakThreshold`). Connected atoms become a **molecule** with a derived Hill-style Unicode formula (e.g. `H₂O`). Each placed atom renders its **real per-element nucleus** (via `Atom.nucleons`, so e.g. Carbon/Oxygen show a denser/larger nucleus than Hydrogen). Atoms also render at **different sizes by element** — the atomic-layer ball is scaled by the pure `Atom.atomicRadius` (normalised Cordero covalent radii: Hydrogen smallest, Carbon/Oxygen larger) — and each atom shows its **atomic symbol** (H, C, O, …, `Atom.symbolOf`) as an **HTML overlay label** (the `Labels` FFI, projected per atom, fading with the ball). A covalent bond shows a **shared electron pair** in the bond, while each atom carries only its **lone electrons** (valence − bonded degree), so the total electron count is conserved (`Σ valence`). Electrons are **colour-coded by role**: an atom's **valence** electrons (its outermost shell plus the shared bonding pair) render in a distinct **amber**, vs its **core** (inner-shell) electrons which stay **blue** — the valence shell is taken from the element configuration (`Atom.electronShells`, last shell), so e.g. Carbon (shells `[2,4]`) shows 2 blue core electrons on the inner ring and 4 amber valence electrons on the outer ring. A **"Valence only" toggle** (`#valence-only` checkbox) hides the core (inner-shell, blue) electrons, leaving only the valence electrons (amber outer-shell lone + the amber shared bonding pair) — Builder and Materials; other scenes unaffected. A **"Free only" toggle** (`#free-electrons-only` checkbox) hides the shared covalent bonding-pair electrons and shows only the free (lone) electrons on each atom — Builder and Materials; composes independently with the Valence toggle (4-way: both OFF shows core+valence+bonding; valenceOnly ON hides core; freeElectronsOnly ON hides bonding; both ON shows valence lone only). The dynamic world lives in a pure model (`Builder`) behind a single shared `Ref` source of truth (`BuilderApi`), also exposed as a `window.__builder` test API (extended with `setOrbit`/`getOrbit` for camera control). The orbit angles are kept in a separate `Ref` owned by `Main` (camera state, not part of the pure model) and exposed via `OrbitApi`. The controls live in a **left drawer**, opened by a `#panel-toggle` icon (top-left, below the scene title) that slides the glassy panel in from the left via anime.js on click and back out on a second click. **Zoom drives a smooth level-of-detail (LOD)** on each placed atom: zoom **below 0.10** and each atom collapses to a single **element-coloured ball** (atom-ball layer only); zoom **0.10–0.20** and the detail smoothly cross-fades; zoom **0.20 and above** and each atom blooms into its real nucleus (`Atom.nucleons`) + electrons (sub-atomic layer fully ON). The **narrow crossfade band [0.10, 0.20]** lets users comfortably zoom OUT to survey many nucleons densely without flipping to atom-balls. The in-between is a **continuous, frame-eased cross-fade** — even the `#zoom-slider` moves smoothly (no abrupt swap, no zoom reset; drag/interaction preserved). It **reuses the existing zoom input** (mouse wheel + absolute zoom via the `#zoom-slider` → `Input.zoomSet`); the detail factor is a pure function of zoom (`Layer.layerBlend`), eased per frame (`State.detail`). Bonds stay visible in both layers: the **atomic (zoomed-out) layer** draws each bond as a **connecting line** between atom-balls (a pre-built `Meshes.unitBeam` per `Builder.bondSegments`, fading as you zoom into the sub-atomic layer), while the **sub-atomic layer** keeps the **shared electron-pair** bond spheres. The "Valence only" toggle composes with the LOD.
- **Materials** — **slice 3**: a curated crystal-structure gallery. REUSES the **entire Builder render path** (`isBuilderLike` gates orbit, zoom/LOD, `#valence-only`, `#free-electrons-only`, atom labels, and the PE overlay on both `Builder` and `Materials`). A **`#materials-cards` gallery** (anime.js, data-driven from `Lattice.structures`) shows one glassy card per curated structure; clicking a card calls `loadStructure(i)`, replacing the shared `BuilderState` Ref with the chosen crystal's pre-built geometry and firing an eager re-render. A **`#materials-info` panel** (data-driven via `Controls.renderInfoPanel "materials-info"`) shows the selected structure's Name, Formula, Hybridization, and per-structure properties (Coordination, Bond length, Note); the panel is populated on card-click, on default-load (Diamond index 0 on first entry), and on `window.__builder.loadStructure(i)` through the seam — all three paths converge on `selectStructureUi` so the card highlight and the info panel stay in sync. A 3rd material is data-only: adding a new `CuratedStructure` record to `Lattice.structures` requires zero renderer/FFI changes. The curated structures extend by DATA: **Diamond** (sp³, **64-atom 2×2×2 diamond-cubic supercell**, interior coord 4) + **Graphene** (sp², **32-atom 4×4 honeycomb supercell**, interior coord 3). Both are **finite supercell fragments** — interior atoms are fully coordinated, edge/corner atoms keep dangling bonds (a finite crystal is never fully closed). Bonds are computed by **proximity** (`Lattice.proximityBuild`: bond iff `|dist−cc| ≤ bondTol`, `cc=165`), so the bond set scales with the supercell. On entering Materials a **one-shot reframe** (`runLoop` `stateOverride` Ref) resets orbit + zooms out (`Camera.clampZoom 0.08`) so the enlarged lattice fits the frustum in the **atom-ball view** (0.08 < detailLo=0.10 → `layerBlend 0.08 == 0`); the manual Builder default view is untouched.
- **Nuclide** — **element layer**: a nuclear-physics sandbox. The scene displays a nucleus (Z protons + N neutrons, rendered as coloured spheres via `Atom.nucleons` on a Fibonacci sphere) and a **data-driven `#nuclide-info` panel** showing Symbol, Name, Z, A (mass number), **binding energy + binding-per-nucleon in MeV** (via SEMF + light-nucleus AME2020 table), **stability/decay mode** (Stable, BetaMinus, BetaPlusEC, Alpha, or Unbound), and a "Last reaction" row (products, Q-value in MeV, RELEASED/ABSORBED). **Transmutation sandbox** controls (`#nuc-add-proton`/`#nuc-remove-proton`/`#nuc-add-neutron`/`#nuc-remove-neutron`/`#nuc-reset`; manual Z/N inputs `#nuclide-z`/`#nuclide-n`) allow manual isotope/element exploration. **Named reactions** (`#react-alpha`/`#react-beta-minus`/`#react-beta-plus`/`#react-fuse`/`#react-fission`) conserve Z & A and compute **Q-values in MeV** from binding energies (positive Q = exothermic RELEASED; negative = endothermic ABSORBED). The **decay classifier** (`decayMode`) uses N/Z band heuristics + Q signs + a light-nuclide stable whitelist (He-4, D, T) to flag `Unbound` nucleon-unbound states (He-5, Li-5, Be-8, etc.). **Teaching simplifications**: SEMF is quantitatively poor for light nuclei (A < ~20) — fixed measured binding energies anchor canonical reactions (α-decay, D-T fusion). SEMF underestimates some fission Q-values (~159 measured vs ~200 SEMF for U-235) — the max-Q heuristic chooses the most exothermic breakup path from a limited set of realistic fragments. Default: Carbon-12 (Z=6, N=6, stable). The `Nuclear` module is pure (Prelude + Data.* only, Z=1..118 decoupled from Atom's 1..36). `NuclearState` (type: Nuclide + lastQ + lastFission) is shared via a `Ref` in `NuclearApi` and mirrored into `State.nuclear` each frame (like `State.builder`).

Each fundamental particle is a sphere. Perspective projection + canvas-resize
throughout. **Zoom** works across all scenes — scroll out to pull the camera back
(e.g. to see many molecules at once), scroll in for detail (a `zoom` factor scales
the effective camera distance via the pure `Camera` module). The **mouse wheel** drives
the zoom (multiplicative via `Input.zoomDelta` → `Main.applyZoom` → `Camera.applyZoomStep`),
and the `#controls` panel has an on-screen **`#zoom-slider` range input** (0.05–5.0)
that sets zoom absolutely (via `Input.zoomSet` → `Main.applyZoomSet`). The slider
is two-way: it updates with the live `State.zoom` each frame (guarded during active
drag so wheel/reframe don't fight the thumb position).

## Commands

| Command | What it does |
|---------|--------------|
| `npm run build` | `spago bundle` → browser bundle at `dist/index.js` |
| `npm test` | `spago test` → runs `Test.Main` |
| `npm run validate` | `scripts/validate-standards.mjs` → fails if any tracked `src/`/`test/`/`e2e/` file exceeds the 800-line cap (runs after `npm run build` in babysitter) |
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
| `Main` | `Main.purs` | Thin entry-point facade; wires canvas, renderer, loop, input; re-exports pure apply folds (`applyDragStrength`, `applyValenceOnly`, `applyFreeElectronsOnly`, `applySubshellView`, `applyOrbit`) from `Update`; builds per-scene `Entity` lists (via `Scene.Entities`) and selects on `State.scene`. Holds the orbit `Ref` (camera state) and coordinates Builder render branching (via `Main.Builder`). Wires `Controls.installPanelToggle` for the left-drawer controls panel |
| `Update` | `Update.purs` | The `State` record definition + pure update folds: `initialState`, `applyDragStrength`, `applyLayerSpace`, `applyValenceOnly`, `applyFreeElectronsOnly`, `applySubshellView`, `applyOrbit`, `applyZoom`, `applyZoomSet`, `applyDetail`, `applyAntibonding`, `applyElement`, `applyToggle2D`, `applyToggle`, `applyBondProgress`, `applyShear`, `applyKey`, `applyMouse`, `step`. Imported and re-exported by `Main` |
| `Scene.Entities` | `Scene/Entities.purs` | `Entity` / `EntityMesh` type definitions; per-scene entity list construction; `place` / LOD helper functions; `builderWorldPosWith`, `builderPlaceWith`, `builderDetailPlaceWith`, `builderBallPlaceWith`, `builderBondLinePlaceWith`, `builderElectronGroupEntitiesWith` — parameterized `layerSpace`-aware variants so render and pick share one effective world scale. **Key:** `builderDetailPlaceWith` decouples intra-atom offset scale from centre scale: centres scale by `builderScale × ls` (atoms spread with layerSpace), but internal offsets (nucleus cluster + electron rings) are frozen at `builderScale × intraAtomLayerSpace` (constant world size at any layerSpace); `intraAtomLayerSpace = 1.6` (the State default). |
| `Main.Builder` | `Main/Builder.purs` | Effectful Builder render branch: `projectionFor`, `installBuilderPick`, `syncAtomLabels`, `updateOverlay`. Separated from `Main` for modularity |
| `Camera` | `Camera.purs` | Pure **camera** module (imports `Math.Matrix` only; no `Effect`/WebGL): single source of truth for the camera constants (`fov`, `cameraDistance`, `clipNear`, `clipFar`) + `minZoom`/`maxZoom` (0.05–5.0). `projection zoom w h` builds a zoom-aware perspective projection (effective camera distance = `cameraDistance/zoom`; byte-identical to the old projection at zoom 1.0); `clampZoom` clamps to range; `applyZoomStep` applies a multiplicative wheel step. **Builder orbit**: `orbit yaw pitch` builds a view rotation (yaw about Y + pitch about X, both in radians; `orbit 0 0` is byte-identical to identity); `viewProjection {yaw,pitch} zoom w h` is projection composed with orbit (collapses to plain projection at zero orbit); `maxPitch` / `clampPitch` constrain pitch to ±≈85° so the camera never flips over the poles; `buttonOrbitDelta` is the fixed per-click orbit step the on-screen `#orbit-up/#orbit-down/#orbit-left/#orbit-right` buttons push through `Main.applyOrbit` (now test/doc reference only). Replaces the old `Main.perspectiveProjection` |
| `Graphics.GL` | `GL.purs` + `GL.js` | WebGL2 FFI: renderer, meshes, colors, clear color, draw calls |
| `Math.Matrix` | `Math/Matrix.purs` | Matrix linear algebra (multiply, projection, `translate`, `scale`, `shear`, `transpose`, etc.) |
| `Vector` | `Vector.purs` | Rotation matrices (`rotateX/Y/Z`) and vector ops |
| `Meshes` | `Meshes.purs` | Geometry specs: cubes, world (`groundPlane`, `gridFloor`), `sphere` (particles/stars), `orbitRing` (thin per-sub-shell orbital ring line), `orbitRingFlat` (flat XY-plane ring for the 2D Bohr view), and `unitBeam` (a unit-length beam mesh placed per `Builder.bondSegments` for the atomic-layer bond lines) |
| `World` | `World.purs` | Static world-backdrop constants/transforms (`groundTransform`, `gridTransform`, `skyColor`) |
| `Scene` | `Scene.purs` | `Scene = CubePoc \| Atomos \| Molecule \| Builder \| Materials \| Nuclide` (6 scenes), `nextScene` (6-cycles CubePoc→Atomos→Molecule→Builder→Materials→Nuclide→CubePoc), `sceneTitle`, atomos `spaceColor` |
| `Lattice` | `Lattice.purs` | Pure **curated-structure registry** for the Materials scene. Defines `CuratedStructure { name, formula, hybridization, properties, build :: BuilderState }` and the ordered `structures :: Array CuratedStructure` registry (Diamond index 0, Graphene index 1). `structureOf i` is clamp-safe. Diamond: 64-atom 2×2×2 diamond-cubic supercell (interior coord 4, sp³, C–C 165). Graphene: 32-atom 4×4 honeycomb supercell (interior coord 3, sp², planar). Bonds via `proximityBuild` (bond iff `|dist−cc| ≤ bondTol`); both are finite fragments (fully-coordinated interior, dangling edges). Adding a 3rd material is data-only (new record, zero renderer changes). No Effect, no WebGL |
| `MaterialsCards` | `MaterialsCards.purs` + `MaterialsCards.js` | DOM-only **anime.js gallery** FFI for the Materials scene. `renderMaterialsCards` builds `#materials-cards` data-driven from a `CardData` array (one card per entry, click → `onSelect i`). `showMaterialsCards` shows/hides the gallery. `highlightCard` highlights the selected card. `showMaterialsPanel` shows/hides `#materials-info`. Never WebGL |
| `Nuclear` | `Nuclear.purs` | Pure **nuclear-physics model** (Prelude + Data.* only; Z = 1..118 decoupled from Atom's 1..36): `Nuclide {z,n}`, `NuclearState`, `Mode` (Stable/BetaMinus/BetaPlusEC/Alpha/Unbound); mass-number + symbol + name accessors; SEMF `bindingEnergy` + `measuredBinding` (fixed for light nuclei D/T/He-4); `qAlpha`/`qBetaMinus`/`qBetaPlusEC`/`qElectronCapture` Q-values; `decayMode` classifier (N/Z band + Q heuristic + light-stable whitelist); operations `addProton`/`removeProton`/`addNeutron`/`removeNeutron` / `alphaDecay`/`betaMinus`/`betaPlusEC`/`fuse`/`fission` (conserving Z & A); 5 shared `applyXxx` reaction transformers. No Effect/WebGL |
| `NuclearApi` | `NuclearApi.purs` + `NuclearApi.js` | The **single shared `Ref NuclearState` source of truth** for the Nuclide scene: `window.__nuclear` test API (`setNuclide(z,n)`, `addProton`/`removeProton`/`addNeutron`/`removeNeutron`, `decayAlpha`/`decayBetaMinus`/`decayBetaPlus`, `fuseWith(z2,n2)`, `fission(za,na,zb,nb)`, `getNuclide`/`getBinding`/`getStability`/`lastQ`/`lastFission`). Each mutation fires `onChange` for eager re-render. `installNuclearApi` exports the Ref; `installNuclearControls` wires the UI buttons. DOM-only FFI, never WebGL |
| `Main.Nuclide` | `Main/Nuclide.purs` | Effectful Nuclide render branch: `nuclideNucleusEntities` (nucleus from `Atom.nucleons` on a Fibonacci sphere), `renderNuclideInfo` (data-driven `#nuclide-info` panel: Symbol/name/Z/A, binding energy, stability/decay-mode, last reaction Q-value), `installNuclearReactions` (wires the 5 named reaction buttons + fission/fuse input fields). Separated from `Main` for modularity |
| `Layer` | `Layer.purs` | Pure **continuous level-of-detail (LOD)** model for the Builder zoom cross-fade (no `Effect`/WebGL; imports only Prelude): `smoothstep` (clamped Hermite) + a detail band `detailLo=0.10`/`detailHi=0.20` + `layerBlend :: Number -> Number` (zoom → detail factor d in [0,1]; 0 = atom balls only, 1 = sub-atomic detail; crossfade band is narrow [0.10, 0.20], sub-particle layer fully ON and persistent for zoom ∈ [0.20, 5.0], letting users survey many nucleons at once) + `easeDetail` (one frame of exponential smoothing toward a target) + `detailName`. No discrete ladder |
| `Atom` | `Atom.purs` | Element table (Z=1..36, H…Kr) + Madelung sub-shell filling (`fillSubshells`/`subshellCap`/`configString`, per-shell totals via `electronShells`) + nucleon cluster + `electronPositions` (discrete electrons on per-sub-shell orbital rings, `subshellRadius`/`subshellInclination`; `electronPositionsBySubshell2D` gives flat XY-plane positions for the 2D Bohr view); `electronPositionsByShell` / `electronPositionsByShell2D` give shell-collapsed positions (all electrons on their principal shell ring, 3D + flat); `shellRings` returns occupied principal shell rings with radii; `clampElectron` exported for electron count clamping; `symbolOf` (element symbol string) + `atomicRadius` (normalised Cordero covalent radius per Z — Builder per-element atom sizing). **Nucleus (`clusterPositions`)**: Fibonacci-sphere angular distribution with cube-root radial spacing (`rad_i = nucleusPackScale · cbrt(i+0.5)`) so outer radius scales as cbrt(N), packing density is constant for every element, and the nucleus visibly grows ∝ cbrt(A) (real R ∝ A^(1/3)); light nuclei pack tight (no gaps), heavy nuclei stay distinct (not over-merged). Single calibration constant `nucleusPackScale = 13.0` tuned so typical nearest-neighbour ≈ 1 nucleon diameter (contiguous, slight overlap). |
| `Molecule` | `Molecule.purs` | Pure, open-ended molecule model: records `Bond {a,b,order,shared}` / `MolAtom {element,center}` / `Property {label,value}` / `Molecule {name,formula,atoms,bonds,properties}`; a `molecules` registry + total/clamp-safe `moleculeOf`; `bondLength`; `sharedElectronPositions` (the shared covalent pair in the internuclear overlap, frame-animated); `moleculeNucleons` (reuses `Atom.nucleons` per atom, translated). Imports `Atom` only |
| `Chem` | `Chem.purs` | Pure, clamp-safe **valence table** `valence :: Int -> Int` for Z = 1..36 (H…Kr). Main-group elements use their common covalent valence; transition metals (Sc…Zn) use a documented flat default of 2. Used to cap bond formation in `Builder`. Also a **normalised single-bond energy table** `bondEnergy :: Int -> Int -> Number` (kJ/mol ÷ 100) for common pairs (O–H 4.63, H–H 4.36, C–H 4.13, C–C 3.46 … weak O–O 1.46, N–N 1.63, F–F 1.55); untabulated pairs use geometric-mean fallback, with documented teaching simplifications. Consumed by `Builder.pullBonds` to decide whether a stretched bond holds (energy ≥ drag strength) or breaks |
| `Builder` | `Builder.purs` | Pure **dynamic world model** facade (→ `Builder.Types`, `Builder.Geom`, `Builder.Spawn`, `Builder.Overlap`, `Builder.Bonds`, `Builder.Electrons`). Re-exports ~41 public names so all importers + `window.__builder` seam remain unchanged. Model types: `PlacedAtom {id,z,pos}` / `BBond {a,b}` / `BuilderState {atoms,bonds,nextId,picked}`; **spawn**: `spawnPos i` (golden-angle/Fibonacci, ~sqrt-radius growth); constraint ops: `addAtom`/`moveAtom`/`moveMolecule` + `clear` (all call `resolveOverlaps` BEFORE `recomputeBonds`, enforcing Pauli-exclusion; anchor-aware); `recomputeBonds` (proximity + valence-capped via `Chem` + hysteresis); `moveAtomWith strength` (strength-aware bond-breaking via `pullBonds` with 10 Gauss-Seidel passes); `minSeparation`/`resolveOverlaps` (per-element contact floor, deterministic); `molecules`/`componentOf`/`formulaOf` (connected-component ops + Hill-style formula); electron ops: `degreeOf`/`loneCountOf`/`bondElectronPositions`/`loneElectronPositions` (count-conserving split); valence/core split: `valenceShellOf`/`coreLoneElectronPositions`/`valenceLoneElectronPositions`; grouped helpers for LOD; `bondSegments`/`bondMidpoints`; pure `projectToScreen`/`unprojectAtDepth`/`unprojectAtDepthFull` pick/drag helpers. No WebGL/`Effect`. |
| `BuilderApi` | `BuilderApi.purs` + `BuilderApi.js` | The **single shared `Ref BuilderState` source of truth** the renderer, the in-app Add/Clear buttons, the mouse drag, and a `window.__builder` test API (`addAtom`/`moveAtom`/`moveAtomWith`/`moveMolecule`/`getAtoms`/`getBonds`/`getMolecules`/`clear`/`loadStructure`) all read/write. Each mutation fires an `onChange` callback for eager re-render. `loadStructure(i)` replaces the entire `BuilderState` with `Lattice.structureOf(i).build`, fires `onChange`, and calls the `onSelectStructure` callback (card highlight + `#materials-info` panel update) so the seam and the card-click path stay in sync. `installBuilderControls` wires `#add-btn`/`#clear-btn`. DOM-only FFI, never WebGL |
| `OrbitApi` | `OrbitApi.purs` + `OrbitApi.js` | The **Builder camera-orbit seam** extending `window.__builder` with `setOrbit(yaw, pitch)` / `getOrbit()` methods over a single shared orbit `Ref` (held by `Main`, kept outside the pure model). Pitch is clamped on write by `Camera.clampPitch`. The E2E seam reads/writes the same `Ref` the renderer and the orbit-drag use (single source of truth). DOM-only FFI, never WebGL |
| `Controls` | `Controls.purs` + `Controls.js` | DOM-only **anime.js** controls FFI (imports only animejs; never WebGL): `renderInfoPanel` (data-driven `#molecule-info` rows + anime.js stagger reveal), `installBondButton`, `runBondAnimation` (tweens a JS value → `State.bondProgress`), `animateControlBarIn` (glassy control-bar entrance animation), `installPanelToggle` (a `#panel-toggle` icon that slides the `#controls` panel in/out as a left drawer via anime.js; closed drawer is `pointer-events:none` + off-screen so it never blocks the canvas), `installButtonPulse` (click-pulse bounce on a button by id). Sibling to `Text` |
| `Palette` | `Palette.purs` | Shell/sub-shell colours: `shellColor n` (distinct per shell) + `subshellColor n l` (shell hue, lighter by ℓ). Pure |
| `Starfield` | `Starfield.purs` | Deterministic Fibonacci-sphere star positions for the atomos backdrop |
| `Text` | `Text.purs` + `Text.js` | anime.js **HTML overlay-text** FFI (`scrambleInto`/`setVisible`) — DOM only, never WebGL. Drives the atomos element label, scene-title banner, and orbital-info (electron-configuration) overlay. (`Controls` is the sibling control FFI for the molecule scene, `Labels` the sibling per-atom overlay FFI for the Builder scene.) |
| `Labels` | `Labels.purs` + `Labels.js` | DOM-only **per-atom overlay-label** FFI (`syncAtomLabels`/`clearAtomLabels`) — HTML overlay text, never WebGL. Reconciles a pool of `.atom-label` divs in the `#atom-labels` container (one per atom id) against projected positions/opacity for the Builder atomic-symbol labels; `clearAtomLabels` empties it when leaving Builder. Sibling to `Text`/`Controls` |
| `FRP.Loop` | `FRP/Loop.purs` + `FRP/Loop.js` | rAF loop + input plumbing (keyboard, mouse, shear button, scene toggle, element selector, `#view-2d` checkbox via `installView2DToggle`, the `#valence-only` checkbox via `installValenceOnlyToggle` → `Input.toggleValenceOnly`, the `#free-electrons-only` checkbox via `installFreeElectronsOnlyToggle` → `Input.toggleFreeElectronsOnly`, the `#subshell-view` checkbox via `installSubshellViewToggle` → `Input.toggleSubshellView`, the **`#drag-strength` slider** via `installDragStrengthSlider` → `Input.dragStrength` (a `Maybe Number` channel feeding `Main.applyDragStrength`), the **`#layer-space` slider** via `installLayerSpaceSlider` → `Input.layerSpace` (a `Maybe Number` channel feeding `Main.applyLayerSpace`), and a `bondProgress` channel via `installBondButton` → `Input`). Also a canvas-scoped **wheel FFI** (`installWheelListener`: `wheel` with `{passive:false}` + `preventDefault`, feeding `Input.zoomDelta`), the on-screen **zoom slider FFI** (`installZoomSlider`: `#zoom-slider` range input that pushes absolute zoom into `Input.zoomSet`; `setZoomSlider` syncs the live `State.zoom` back to the thumb each frame, guarded during active drag), the on-screen **orbit buttons FFI** (`installOrbitButtons`: `#orbit-up/#orbit-down/#orbit-left/#orbit-right` + `#orbit-reset` click listeners that fold a fixed `applyOrbit` delta into the shared orbit Ref, or reset to `{yaw:0, pitch:0}`; Builder and Materials), a canvas **pointer FFI** (`installCanvasPointer`: mousedown/move/up — the mousedown callback also passes the native click count `event.detail` so `Main.installBuilderPick` can tell single-click [whole molecule] from double-click [single atom]; dragging empty space in Builder and Materials feeds a `{ dx, dy }` delta into an `orbitDelta` Input channel, driving `Main.applyOrbit` yaw/pitch updates), `installAddButton`/`installClearButton` for the Builder `#add-btn`/`#clear-btn`, and `installNuclearControls` for the Nuclide transmutation/reaction UI buttons and Z/N inputs. Also `setBuilderDetail` (writes the per-frame eased `State.detail` to `window.__builderDetail` as a debug/E2E seam) |

State is a plain record (`transform`, `speed`, `mouseLast`, `frame`, `scene`,
`element`, `view2D`, `subshellView`, `valenceOnly`, `freeElectronsOnly`, `antibonding`, `bondProgress`, `dragStrength`, `layerSpace`, `zoom`, `detail`, `builderYaw`, `builderPitch`, `builder`, `nuclear`) advanced each
frame (the `Input` channel gained `toggle2D`, applied via `Main.applyToggle2D`,
`toggleSubshellView`, applied via the pure `Main.applySubshellView` (`subshellView`
initialises to `true`), `toggleValenceOnly`, applied via the pure `Main.applyValenceOnly` (`valenceOnly`
initialises to `false`), `toggleFreeElectronsOnly`, applied via the pure `Main.applyFreeElectronsOnly` (`freeElectronsOnly`
initialises to `false`), a `dragStrength` channel fed by the `#drag-strength` slider (applied via `Main.applyDragStrength`, initialises to `3.0`), a `layerSpace` channel fed by the `#layer-space` slider (applied via `Main.applyLayerSpace`, initialises to `1.6`), a `bondProgress`
channel fed by the anime.js bond animation, an `orbitDelta` channel fed by empty-space drag in the Builder (applied via `Main.applyOrbit`, builderYaw/builderPitch initialise to `0.0`), a `zoomDelta` channel fed by the wheel
FFI (applied via `Main.applyZoom` → `Camera.applyZoomStep`), and a `zoomSet` channel fed by the `#zoom-slider` (applied via `Main.applyZoomSet`, clamps via `Camera.clampZoom`)); updates return new records
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
  organized into per-domain `test/` spec modules (aggregated by `test/Main.purs`; `test/Util.purs` holds shared helpers: `1e-10` tolerance epsilon, `approxEq`, `approxEqMatrix`, `check`). Specs: `MatrixSpec`, `VectorSpec`, `MeshesWorldSpec`, `AtomSpec`, `MoleculeChemSpec`, `BuilderSpec`, `BuilderOverlapSpec`, `BuilderBondsSpec`, `CameraLayerSpec`, `SceneStarfieldSpec`, `MainStateSpec`, `LatticeSpec`, `NucleusPackingSpec`, `NuclearSpec` — total 526+ assertions covering `Math.Matrix`, `Vector`, `Meshes` (world geometry), `World`, `Builder` (constraint ops + drag-strength bond dynamics + 3D spawn + orbit/unproject), `Camera` (orbit, viewProjection, pitch clamp), `Lattice` (registry, Diamond/Graphene geometry invariants, minSeparation, no spurious bonds), `Atom` (nucleus packing density across H→Kr), and `Nuclear` (SEMF/Q/decay/conservation). Browser behavior is covered by
  Playwright canvas-verification specs in `e2e/` (run with `npm run e2e`;
  `playwright.config.js` sets `retries: 2` for SwiftShader render-timing
  robustness). The Builder and Materials scenes are driven in E2E via the `window.__builder` test
  seam (`addAtom`/`moveAtom`/`moveAtomWith`/`getBonds`/`getMolecules`/`clear`/`loadStructure`/`setOrbit`/`getOrbit`), which shares the
  same `BuilderApi` `Ref` (world model) and orbit `Ref` (camera state) as the renderer so reads reflect mutations immediately;
  the Pauli-exclusion no-overlap constraint is verified in
  `builder-no-overlap.spec.js` (anchor-aware separation, deterministic resolution), drag-strength bond-breaking dynamics in `builder-drag-strength.spec.js` (6 scenarios), 3D spawn positions in `builder-3d-spawn.spec.js` (spread, determinism, Pauli floor), 3D orbit interaction in `builder-3d-orbit.spec.js` (camera rotation, true-depth drag, bonds across depth), and orbit buttons in `builder-orbit-buttons.spec.js` (directional orbit steps, reset); `Camera.buttonOrbitDelta` is covered by unit tests reusing `applyOrbit`. Layer-space slider is covered by `builder-layer-space.spec.js` (world spread, render/pick parity) and `builder-scale-decouple.spec.js` (sub-atomic structure scale-invariance: nucleon nearest-neighbour separation + electron-ring clearance constant across layerSpace). Nucleus packing is covered by `NucleusPackingSpec` (per-element mean/max nearest-neighbour density guards across H→Kr) and `nucleus-packing.spec.js` e2e. Zoom slider is covered by `zoom-slider.spec.js` (absolute set, two-way sync, gesture composition). The Materials scene is additionally covered by `materials-scene.spec.js` (6-cycle navigation, default-load Diamond, loadStructure seam, orbit), `materials-cards.spec.js` (gallery visibility, card clicks, Diamond/Graphene distinct), and `materials-panel.spec.js` (panel visibility, data-driven content, seam sync, orbit + valence-only in Materials). The Nuclide scene is covered by `element-transmutation.spec.js` (isotope/element changes, Z/N inputs, reset) and `nuclear-reactions.spec.js` (α/β−/β+/fuse/fission Q-values, decay classification, conservation).
  Untested: `Graphics.GL` FFI internals, `FRP.Loop` orbit drag. PureScript is
  formatted with `purs-tidy`.
- Do NOT commit build output. `dist/`, `output/`, `.spago/`, `node_modules/`
  are gitignored.
- Keep each FFI `.js` file paired with its `.purs` module, and type all FFI
  imports as `Effect` when they perform side effects.
- Prefer many small focused modules and immutable state (return new records,
  never mutate in place).
- Hard 800-line/file cap enforced by `npm run validate` (runs after `npm run build` in babysitter processes); see [CODING_STANDARDS.md](./CODING_STANDARDS.md) for organization rules, the cap, and the validator.

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
