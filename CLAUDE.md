# vectors

A PureScript + WebGL2 3D graphics demo. Renders meshes (a main cube plus an
orbiting satellite cube) with perspective projection, mouse/keyboard-driven
rotation, and canvas-resize handling.

## Commands

| Command | What it does |
|---------|--------------|
| `npm run build` | `spago bundle` → browser bundle at `dist/index.js` |
| `npm test` | `spago test` → runs `Test.Main` |
| `npm run dev` | bundle, then `node dev-server.js` (static server on `0.0.0.0:47474`) |

Toolchain: spago + purs 0.15.16 + esbuild, driven via npm. PureScript deps are
declared in `spago.yaml`; npm holds only the dev toolchain and Playwright.

## Architecture

Entry: `index.html` loads `dist/index.js` (the esbuild bundle, gitignored),
which boots the `Main` module.

Module map (under `src/`):

| Module | Files | Role |
|--------|-------|------|
| `Main` | `Main.purs` | Entry point; wires canvas, renderer, render loop, input, and the `Entity` list (cube + satellite) |
| `Graphics.GL` | `GL.purs` + `GL.js` | WebGL2 FFI: renderer, meshes, colors, draw calls |
| `Math.Matrix` | `Math/Matrix.purs` | Matrix linear algebra (multiply, projection, etc.) |
| `Vector` | `Vector.purs` | Rotation matrices (`rotateX/Y/Z`) and vector ops |
| `Meshes` | `Meshes.purs` | Geometry specs for wireframe and solid cubes |
| `FRP.Loop` | `FRP/Loop.purs` + `FRP/Loop.js` | requestAnimationFrame render loop + input plumbing |

State is a plain record (`transform`, `speed`, `mouseLast`, `frame`) advanced
each frame; updates return new records rather than mutating.

## Conventions

- Conventional commits: `feat:` / `refactor:` / `fix:` (also `test:`, `chore:`,
  `docs:`, `perf:`, `ci:`).
- Ship tests alongside behavior changes. Tests are hand-rolled assertions in
  `test/Main.purs` (rotation-matrix properties, `1e-10` tolerance). Covered:
  `Math.Matrix`, `Vector`. Untested: `Meshes`, `Graphics.GL`, `FRP.Loop`.
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
