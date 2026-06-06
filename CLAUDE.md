# vectors

A PureScript + WebGL2 3D graphics demo with two **scenes**, toggled by an
on-screen switch:
- **Cube POC** — a solid-lit main cube + orbiting satellite inside a world
  backdrop (green ground, wireframe grid, sky-blue horizon), with
  mouse/keyboard rotation and a **Shear** control.
- **atomos** — a 3D atom visualizer in near-black space with a starfield: a
  nucleus of proton (red) + neutron (gray) spheres and electrons on animated
  Bohr orbits. The atom is **configurable by element** (Z = 1..8) via a selector.

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
**animejs** (used via the `Text` FFI for HTML overlay text only — never WebGL).

## Architecture

Entry: `index.html` loads `dist/index.js` (the esbuild bundle, gitignored),
which boots the `Main` module.

Module map (under `src/`):

| Module | Files | Role |
|--------|-------|------|
| `Main` | `Main.purs` | Entry point; wires canvas, renderer, loop, input; builds per-scene `Entity` lists and selects on `State.scene`. `EntityMesh = Solid \| Wire` dispatch |
| `Graphics.GL` | `GL.purs` + `GL.js` | WebGL2 FFI: renderer, meshes, colors, clear color, draw calls |
| `Math.Matrix` | `Math/Matrix.purs` | Matrix linear algebra (multiply, projection, `translate`, `scale`, `shear`, etc.) |
| `Vector` | `Vector.purs` | Rotation matrices (`rotateX/Y/Z`) and vector ops |
| `Meshes` | `Meshes.purs` | Geometry specs: cubes, world (`groundPlane`, `gridFloor`), and `sphere` (particles/stars) |
| `World` | `World.purs` | Static world-backdrop constants/transforms (`groundTransform`, `gridTransform`, `skyColor`) |
| `Scene` | `Scene.purs` | `Scene = CubePoc \| Atomos`, `nextScene`, atomos `spaceColor` |
| `Atom` | `Atom.purs` | Element table (Z=1..8) + electron-shell filling + nucleon cluster + `electronPositions` (Bohr orbits) |
| `Starfield` | `Starfield.purs` | Deterministic Fibonacci-sphere star positions for the atomos backdrop |
| `Text` | `Text.purs` + `Text.js` | anime.js **HTML overlay-text** FFI (`scrambleInto`/`setVisible`) — DOM only, never WebGL. Drives the atomos element label + scene-title banner |
| `FRP.Loop` | `FRP/Loop.purs` + `FRP/Loop.js` | rAF loop + input plumbing (keyboard, mouse, shear button, scene toggle, element selector → `Input`) |

State is a plain record (`transform`, `speed`, `mouseLast`, `frame`, `scene`,
`element`) advanced each frame; updates return new records rather than mutating.
The world meshes, nucleus, and starfield use scene-/element-derived transforms;
electrons advance with `frame`.

## Conventions

- Conventional commits: `feat:` / `refactor:` / `fix:` (also `test:`, `chore:`,
  `docs:`, `perf:`, `ci:`).
- Ship tests alongside behavior changes. Unit tests are hand-rolled assertions
  in `test/Main.purs` (`1e-10` tolerance) covering `Math.Matrix`, `Vector`,
  `Meshes` (world geometry), and `World`. Browser behavior is covered by
  Playwright canvas-verification specs in `e2e/` (run with `npm run e2e`).
  Untested: `Graphics.GL` FFI internals, `FRP.Loop`. PureScript is formatted
  with `purs-tidy`.
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
