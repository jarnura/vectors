# vectors

A PureScript + WebGL2 3D graphics demo — a vertical-sliced
**"learn matter" learning platform** (atoms → chemistry → properties of matter) —
with five **scenes**, toggled by an on-screen switch (`nextScene` 5-cycles):
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
- **Builder** — **slice 2**: an interactive **molecule-builder sandbox**. From a
  modern, **glassy (backdrop-blur) anime.js-animated control bar** you **Add** atoms
  of any element (Z = 1..36) and **Clear** the world; you **drag atoms in 3D** with
  the mouse. A **single-click + drag moves the WHOLE connected molecule** (every
  bonded atom translates rigidly together — relative geometry + internal bonds
  preserved, via `Builder.moveMolecule` over the atom's connected component), while
  a **double-click on an atom then drag moves just that one atom** (`Builder.moveAtom`,
  detected by the native mouse click count, `event.detail ≥ 2`); a lone atom moves
  the same either way. When atoms come near each other they **auto-bond, valence-aware**
  (H=1, C=4, N=3, O=2, … via `Chem.valence`) with **break hysteresis** (bonds form
  under `bondThreshold`, break only past the wider `breakThreshold`). Connected atoms
  become a **molecule** with a derived Hill-style Unicode formula (e.g. `H₂O`). Each
  placed atom renders its **real per-element nucleus** (via `Atom.nucleons`, so e.g.
  Carbon/Oxygen show a denser/larger nucleus than Hydrogen); a covalent bond shows a
  **shared electron pair** in the bond, while each atom carries only its **lone
  electrons** (valence − bonded degree), so the total electron count is conserved
  (`Σ valence`). Electrons are **colour-coded by role**: an atom's **valence**
  electrons (its outermost shell plus the shared bonding pair) render in a distinct
  **amber**, vs its **core** (inner-shell) electrons which stay **blue** — the
  valence shell is taken from the element configuration (`Atom.electronShells`, last
  shell), so e.g. Carbon (shells `[2,4]`) shows 2 blue core electrons on the inner
  ring and 4 amber valence electrons on the outer ring. A **"Valence only"
  toggle** (`#valence-only` checkbox) hides the core (inner-shell, blue)
  electrons, leaving only the valence electrons (amber outer-shell lone + the
  amber shared bonding pair) — Builder-only; other scenes unaffected. The
  dynamic world lives in a pure model (`Builder`) behind a single shared `Ref`
  source of truth (`BuilderApi`), also exposed as a `window.__builder` test API.
  The controls live in a **left drawer**, opened by a `#panel-toggle` icon
  (top-left, below the scene title) that slides the glassy panel in from the
  left via anime.js on click and back out on a second click.
- **Scale** — a **zoom-driven, ordered, extensible ladder of scale LAYERS**
  ("powers of ten"), today two: a **Sub-atomic** layer (the default) rendering one
  **focus atom** atomos-style — nucleus (`Atom.nucleons`) + per-shell orbital
  rings + frame-animated electrons (focus element: **Hydrogen**) — and a higher
  **Atomic** layer rendering the **focus molecule** as whole **atom-balls + bonds**
  (one `Meshes.sphere` per atom, not nucleon clusters; molecule: **H₂**, the
  `Molecule` registry's current entry). **Crossing a zoom threshold swaps the layer
  AND resets the camera zoom to a mid value** (each layer with its own fresh 0.2–5.0
  range): in Sub-atomic, zooming **out** past ≈0.3 switches up to Atomic (zoom
  resets to 1.0); in Atomic, zooming **in** past ≈3.0 switches down to Sub-atomic;
  at the top/bottom layer a further zoom just clamps (no switch). It **reuses the
  existing `Input.zoomDelta` camera input** (mouse wheel + `#zoom-in`/`#zoom-out`
  buttons → `Main.applyZoom`) — no new input channel; the crossing is a pure
  function of accumulated zoom (`Layer.applyLayerZoom`) applied **scene-gated** in
  the step right after `applyZoom`. A `#scale-layer` HTML overlay label (anime.js
  **text only**, never WebGL) announces the active layer, shown only in this scene.
  The ladder is **extensible** — a future Molecular/Nuclear layer is one
  constructor + ladder entry. Other scenes are untouched.

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
| `Main` | `Main.purs` | Entry point; wires canvas, renderer, loop, input; builds per-scene `Entity` lists and selects on `State.scene`. `EntityMesh = Solid \| Wire` dispatch. Uses `Camera.projection` for the perspective projection; the draw loop re-projects when **zoom or size** changes (`applyZoom` folds wheel input via `Camera.applyZoomStep`). Molecule scene renders two nuclei + the shared electron pair model-driven; `#bond-btn` runs the anime.js bond animation (drawing atoms together via `State.bondProgress`); `updateOverlay` shows `#molecule-info` only in the molecule scene. Atomos render selects sub-shell vs shell-only ring geometry on `State.subshellView` × `State.view2D` (4 combos; both geometries built once at init); pure `applySubshellView` composed into step. Builder scene renders placed atoms + bonds (instanced/reused meshes built once at init, placed each frame from the shared `Ref`); `installBuilderPick` does a 3D pointer pick + drag (scene-gated to Builder so cube rotation is unaffected; builds `Camera.projection State.zoom` so pick/drag matches the zoomed view) → live valence-aware re-bonding; the pick latches a `whole` flag from the mouse click count (`event.detail < 2` ⇒ single-click ⇒ drag the **whole molecule** via `Builder.moveMolecule`; double-click ⇒ drag a **single atom** via `Builder.moveAtom`). When `State.valenceOnly`, the Builder render drops the core-electron group (Builder-only). Scale scene renders the active `State.layer` (Atomic ⇒ molecule atom-balls + bonds; Sub-atomic ⇒ focus-atom nucleus + rings + electrons; both meshes built once at init), with a pure scene-gated `applyLayer` (`Layer.applyLayerZoom`) composed right after `applyZoom`, and `updateOverlay` driving the `#scale-layer` overlay. Wires `Controls.installPanelToggle` for the left-drawer controls panel |
| `Camera` | `Camera.purs` | Pure **camera** module (imports `Math.Matrix` only; no `Effect`/WebGL): single source of truth for the camera constants (`fov`, `cameraDistance`, `clipNear`, `clipFar`) + `minZoom`/`maxZoom` (0.2–5.0). `projection zoom w h` builds a zoom-aware perspective projection (effective camera distance = `cameraDistance/zoom`; byte-identical to the old projection at zoom 1.0); `clampZoom` clamps to range; `applyZoomStep` applies a multiplicative wheel step; `buttonZoomDelta` is the fixed synthetic wheel-delta magnitude the on-screen `#zoom-in`/`#zoom-out` buttons push through `applyZoomStep`. Replaces the old `Main.perspectiveProjection` |
| `Graphics.GL` | `GL.purs` + `GL.js` | WebGL2 FFI: renderer, meshes, colors, clear color, draw calls |
| `Math.Matrix` | `Math/Matrix.purs` | Matrix linear algebra (multiply, projection, `translate`, `scale`, `shear`, etc.) |
| `Vector` | `Vector.purs` | Rotation matrices (`rotateX/Y/Z`) and vector ops |
| `Meshes` | `Meshes.purs` | Geometry specs: cubes, world (`groundPlane`, `gridFloor`), `sphere` (particles/stars), `orbitRing` (thin per-sub-shell orbital ring line), and `orbitRingFlat` (flat XY-plane ring for the 2D Bohr view) |
| `World` | `World.purs` | Static world-backdrop constants/transforms (`groundTransform`, `gridTransform`, `skyColor`) |
| `Scene` | `Scene.purs` | `Scene = CubePoc \| Atomos \| Molecule \| Builder \| Scale` (5 scenes), `nextScene` (5-cycles), `sceneTitle` (`Scale = "scale"`), atomos `spaceColor` |
| `Layer` | `Layer.purs` | Pure **scale-ladder** model for the Scale scene (no `Effect`/WebGL): `data ScaleLayer = SubAtomic \| Atomic` (ordered micro→macro), `allLayers`, `layerUp`/`layerDown :: ScaleLayer -> Maybe ScaleLayer`, `layerName`, threshold/reset constants (`zoomOutThreshold`/`zoomInThreshold`/`layerResetZoom`), and `applyLayerZoom :: ScaleLayer -> Number -> { layer, zoom }` (the crossing/reset rule: cross a threshold ⇒ step layer + reset zoom to mid; clamp at the ends). Extensible — a future layer is one constructor + ladder entry |
| `Atom` | `Atom.purs` | Element table (Z=1..36, H…Kr) + Madelung sub-shell filling (`fillSubshells`/`subshellCap`/`configString`, per-shell totals via `electronShells`) + nucleon cluster + `electronPositions` (discrete electrons on per-sub-shell orbital rings, `subshellRadius`/`subshellInclination`; `electronPositionsBySubshell2D` gives flat XY-plane positions for the 2D Bohr view); `electronPositionsByShell` / `electronPositionsByShell2D` give shell-collapsed positions (all electrons on their principal shell ring, 3D + flat); `shellRings` returns occupied principal shell rings with radii; `clampElectron` exported for electron count clamping |
| `Molecule` | `Molecule.purs` | Pure, open-ended molecule model: records `Bond {a,b,order,shared}` / `MolAtom {element,center}` / `Property {label,value}` / `Molecule {name,formula,atoms,bonds,properties}`; a `molecules` registry + total/clamp-safe `moleculeOf`; `bondLength`; `sharedElectronPositions` (the shared covalent pair in the internuclear overlap, frame-animated); `moleculeNucleons` (reuses `Atom.nucleons` per atom, translated). Imports `Atom` only |
| `Chem` | `Chem.purs` | Pure, clamp-safe **valence table** `valence :: Int -> Int` for Z = 1..36 (H…Kr). Main-group elements use their common covalent valence; transition metals (Sc…Zn) use a documented flat default of 2. Used to cap bond formation in `Builder` |
| `Builder` | `Builder.purs` | Pure **dynamic world model** for the builder sandbox: `PlacedAtom {id,z,pos}` / `BBond {a,b}` / `BuilderState {atoms,bonds,nextId,picked}`; `addAtom`/`moveAtom`/`clear`; `recomputeBonds` (proximity + valence-capped via `Chem` + break hysteresis, `bondThreshold`/`breakThreshold`); `molecules` (connected components) + `componentOf` (the connected component containing an atom id, reusing the `molecules` flood) + `moveMolecule` (rigidly translate the whole component of an anchor atom so the anchor lands at a target — same delta for every component atom, internal bonds preserved — then `recomputeBonds`; a lone atom reduces to `moveAtom`) + `formulaOf` (Hill-style Unicode formula); `bondMidpoints`; electron helpers `degreeOf`/`loneCountOf` (per-atom bonded degree and lone count = valence − degree) + `bondElectronPositions`/`loneElectronPositions` (shared pair in each bond and the remaining lone electrons on each atom, electron count conserved); valence/core split helpers `valenceShellOf` (outermost shell from `Atom.electronShells`) + `coreLoneElectronPositions`/`valenceLoneElectronPositions` (inner-shell vs outermost-shell lone electrons for the amber/blue colour split, with `loneElectronPositions = core <> valence`); pure `projectToScreen`/`unprojectAtDepth` pick/unproject helpers. No WebGL/`Effect` |
| `BuilderApi` | `BuilderApi.purs` + `BuilderApi.js` | The **single shared `Ref BuilderState` source of truth** the renderer, the in-app Add/Clear buttons, the mouse drag, and a `window.__builder` test API (`addAtom`/`moveAtom`/`moveMolecule`/`getAtoms`/`getBonds`/`getMolecules`/`clear`) all read/write. Each mutation fires an `onChange` callback for eager re-render. `installBuilderControls` wires `#add-btn`/`#clear-btn`. DOM-only FFI, never WebGL |
| `Controls` | `Controls.purs` + `Controls.js` | DOM-only **anime.js** controls FFI (imports only animejs; never WebGL): `renderInfoPanel` (data-driven `#molecule-info` rows + anime.js stagger reveal), `installBondButton`, `runBondAnimation` (tweens a JS value → `State.bondProgress`), `animateControlBarIn` (glassy control-bar entrance animation), `installPanelToggle` (a `#panel-toggle` icon that slides the `#controls` panel in/out as a left drawer via anime.js; closed drawer is `pointer-events:none` + off-screen so it never blocks the canvas), `installButtonPulse` (click-pulse bounce on a button by id). Sibling to `Text` |
| `Palette` | `Palette.purs` | Shell/sub-shell colours: `shellColor n` (distinct per shell) + `subshellColor n l` (shell hue, lighter by ℓ). Pure |
| `Starfield` | `Starfield.purs` | Deterministic Fibonacci-sphere star positions for the atomos backdrop |
| `Text` | `Text.purs` + `Text.js` | anime.js **HTML overlay-text** FFI (`scrambleInto`/`setVisible`) — DOM only, never WebGL. Drives the atomos element label, scene-title banner, orbital-info (electron-configuration) overlay, and the Scale-scene `#scale-layer` active-layer label. (`Controls` is the sibling control FFI for the molecule scene.) |
| `FRP.Loop` | `FRP/Loop.purs` + `FRP/Loop.js` | rAF loop + input plumbing (keyboard, mouse, shear button, scene toggle, element selector, `#view-2d` checkbox via `installView2DToggle`, the `#valence-only` checkbox via `installValenceOnlyToggle` → `Input.toggleValenceOnly`, the `#subshell-view` checkbox via `installSubshellViewToggle` → `Input.toggleSubshellView`, and a `bondProgress` channel via `installBondButton` → `Input`). Also a canvas-scoped **wheel FFI** (`installWheelListener`: `wheel` with `{passive:false}` + `preventDefault`, feeding `Input.zoomDelta`), the on-screen **zoom buttons FFI** (`installZoomButtons`: `#zoom-in`/`#zoom-out` click listeners that push ∓`Camera.buttonZoomDelta` into the same `Input.zoomDelta` channel as the wheel), a canvas **pointer FFI** (`installCanvasPointer`: mousedown/move/up — the mousedown callback also passes the native click count `event.detail` so `Main.installBuilderPick` can tell single-click [whole molecule] from double-click [single atom]) and `installAddButton`/`installClearButton` for the Builder `#add-btn`/`#clear-btn` |

State is a plain record (`transform`, `speed`, `mouseLast`, `frame`, `scene`,
`element`, `view2D`, `subshellView`, `valenceOnly`, `bondProgress`, `zoom`, `layer`, `builder`) advanced each
frame (the `Input` channel gained `toggle2D`, applied via `Main.applyToggle2D`,
`toggleSubshellView`, applied via the pure `Main.applySubshellView` (`subshellView`
initialises to `true`), `toggleValenceOnly`, applied via the pure `Main.applyValenceOnly` (`valenceOnly`
initialises to `false`), a `bondProgress`
channel fed by the anime.js bond animation, and a `zoomDelta` channel fed by both the wheel
FFI and the on-screen `#zoom-in`/`#zoom-out` buttons (`installZoomButtons`), applied via
`Main.applyZoom` → `Camera.applyZoomStep`); updates return new records
rather than mutating. `zoom` initialises to `1.0`. `layer` (the Scale-scene active
`ScaleLayer`) initialises to `SubAtomic`, advanced by the pure scene-gated
`Main.applyLayer` (`Layer.applyLayerZoom`) composed right after `applyZoom`. The `builder` field mirrors the shared `BuilderApi` `Ref` (the Builder
world model), refreshed into the rendered state each frame and on eager
re-render. The world meshes, nucleus, and starfield use scene-/element-derived
transforms; electrons and the shared molecule pair advance with `frame`. The
on-screen `#controls` bar is a **glassy (backdrop-blur)** panel that now lives in
a **left drawer** toggled by a `#panel-toggle` icon (`Controls.installPanelToggle`,
anime.js slide-in/out); the old auto-show entrance (`animateControlBarIn`) is no
longer wired. Buttons keep their click pulses.

## Conventions

- Conventional commits: `feat:` / `refactor:` / `fix:` (also `test:`, `chore:`,
  `docs:`, `perf:`, `ci:`).
- Ship tests alongside behavior changes. Unit tests are hand-rolled assertions
  in `test/Main.purs` (`1e-10` tolerance) covering `Math.Matrix`, `Vector`,
  `Meshes` (world geometry), and `World`. Browser behavior is covered by
  Playwright canvas-verification specs in `e2e/` (run with `npm run e2e`;
  `playwright.config.js` sets `retries: 2` for SwiftShader render-timing
  robustness). The Builder scene is driven in E2E via the `window.__builder` test
  seam (`addAtom`/`moveAtom`/`getBonds`/`getMolecules`/`clear`), which shares the
  same `BuilderApi` `Ref` as the renderer so reads reflect mutations immediately.
  Untested: `Graphics.GL` FFI internals, `FRP.Loop`. PureScript is
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
