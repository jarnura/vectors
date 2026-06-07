# vectors

A PureScript + WebGL2 3D graphics demo — **slice 1 of a vertical-sliced
"learn matter" learning platform** (atoms → chemistry → properties of matter) —
with three **scenes**, toggled by an on-screen switch (`nextScene` 3-cycles):
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
  both views). (Idealized Madelung; Cr/Cu anomalies not modeled.)
- **Molecule** — a 3D **H₂ molecule** in the same near-black space: two hydrogen
  nuclei whose covalent bond is rendered as a **shared electron pair** sitting in
  the internuclear overlap (no stick), frame-animated. All controls are **anime.js**
  driven: a **#bond-btn** ("Form bond") runs an anime.js bond-formation animation
  that draws the two atoms together (tweening `State.bondProgress`), and an animated
  **#molecule-info details panel** is populated **data-driven** from the molecule's
  properties. The `Molecule` model is **open-ended** — a molecule registry of
  records (atoms/bonds/properties) — so H₂ is just the first entry of a growing set.

Each fundamental particle is a sphere. Perspective projection + canvas-resize
throughout.

## Commands

| Command | What it does |
|---------|--------------|
| `npm run build` | `spago bundle` → browser bundle at `dist/index.js` |
| `npm test` | `spago test` → runs `Test.Main` |
| `npm run dev` | bundle, then `node dev-server.js` (static server on `0.0.0.0:47474`) |
| `npm run e2e` | Playwright canvas-verification E2E suite (boots the dev server) |

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
| `Main` | `Main.purs` | Entry point; wires canvas, renderer, loop, input; builds per-scene `Entity` lists and selects on `State.scene`. `EntityMesh = Solid \| Wire` dispatch. Molecule scene renders two nuclei + the shared electron pair model-driven; `#bond-btn` runs the anime.js bond animation (drawing atoms together via `State.bondProgress`); `updateOverlay` shows `#molecule-info` only in the molecule scene |
| `Graphics.GL` | `GL.purs` + `GL.js` | WebGL2 FFI: renderer, meshes, colors, clear color, draw calls |
| `Math.Matrix` | `Math/Matrix.purs` | Matrix linear algebra (multiply, projection, `translate`, `scale`, `shear`, etc.) |
| `Vector` | `Vector.purs` | Rotation matrices (`rotateX/Y/Z`) and vector ops |
| `Meshes` | `Meshes.purs` | Geometry specs: cubes, world (`groundPlane`, `gridFloor`), `sphere` (particles/stars), `orbitRing` (thin per-sub-shell orbital ring line), and `orbitRingFlat` (flat XY-plane ring for the 2D Bohr view) |
| `World` | `World.purs` | Static world-backdrop constants/transforms (`groundTransform`, `gridTransform`, `skyColor`) |
| `Scene` | `Scene.purs` | `Scene = CubePoc \| Atomos \| Molecule` (3 scenes), `nextScene` (3-cycles), atomos `spaceColor` |
| `Atom` | `Atom.purs` | Element table (Z=1..36, H…Kr) + Madelung sub-shell filling (`fillSubshells`/`subshellCap`/`configString`, per-shell totals via `electronShells`) + nucleon cluster + `electronPositions` (discrete electrons on per-sub-shell orbital rings, `subshellRadius`/`subshellInclination`; `electronPositionsBySubshell2D` gives flat XY-plane positions for the 2D Bohr view) |
| `Molecule` | `Molecule.purs` | Pure, open-ended molecule model: records `Bond {a,b,order,shared}` / `MolAtom {element,center}` / `Property {label,value}` / `Molecule {name,formula,atoms,bonds,properties}`; a `molecules` registry + total/clamp-safe `moleculeOf`; `bondLength`; `sharedElectronPositions` (the shared covalent pair in the internuclear overlap, frame-animated); `moleculeNucleons` (reuses `Atom.nucleons` per atom, translated). Imports `Atom` only |
| `Controls` | `Controls.purs` + `Controls.js` | DOM-only **anime.js** controls FFI (imports only animejs; never WebGL): `renderInfoPanel` (data-driven `#molecule-info` rows + anime.js stagger reveal), `installBondButton`, `runBondAnimation` (tweens a JS value → `State.bondProgress`). Sibling to `Text` |
| `Palette` | `Palette.purs` | Shell/sub-shell colours: `shellColor n` (distinct per shell) + `subshellColor n l` (shell hue, lighter by ℓ). Pure |
| `Starfield` | `Starfield.purs` | Deterministic Fibonacci-sphere star positions for the atomos backdrop |
| `Text` | `Text.purs` + `Text.js` | anime.js **HTML overlay-text** FFI (`scrambleInto`/`setVisible`) — DOM only, never WebGL. Drives the atomos element label, scene-title banner, and orbital-info (electron-configuration) overlay. (`Controls` is the sibling control FFI for the molecule scene.) |
| `FRP.Loop` | `FRP/Loop.purs` + `FRP/Loop.js` | rAF loop + input plumbing (keyboard, mouse, shear button, scene toggle, element selector, `#view-2d` checkbox via `installView2DToggle`, and a `bondProgress` channel via `installBondButton` → `Input`) |

State is a plain record (`transform`, `speed`, `mouseLast`, `frame`, `scene`,
`element`, `view2D`, `bondProgress`) advanced each frame (the `Input` channel
gained `toggle2D`, applied via `Main.applyToggle2D`, and a `bondProgress` channel
fed by the anime.js bond animation); updates return new records rather than
mutating. The world meshes, nucleus, and starfield use scene-/element-derived
transforms; electrons and the shared molecule pair advance with `frame`.

## Conventions

- Conventional commits: `feat:` / `refactor:` / `fix:` (also `test:`, `chore:`,
  `docs:`, `perf:`, `ci:`).
- Ship tests alongside behavior changes. Unit tests are hand-rolled assertions
  in `test/Main.purs` (`1e-10` tolerance) covering `Math.Matrix`, `Vector`,
  `Meshes` (world geometry), and `World`. Browser behavior is covered by
  Playwright canvas-verification specs in `e2e/` (run with `npm run e2e`;
  `playwright.config.js` sets `retries: 2` for SwiftShader render-timing
  robustness). Untested: `Graphics.GL` FFI internals, `FRP.Loop`. PureScript is
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
