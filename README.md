# vectors

A small **PureScript + WebGL2** 3D graphics demo. A solid-lit cube and an
orbiting satellite cube sit inside a simple **world** — a green ground plane, a
wireframe grid floor, and a sky-blue horizon — with perspective projection and
mouse/keyboard-driven rotation.

## Controls

- **Arrow keys** — rotate the main cube (X/Y axes).
- **Mouse drag** — rotate the cube freely.

## Develop

```bash
npm install          # dev toolchain + Playwright
npm run dev          # bundle + serve on http://localhost:47474
npm test             # unit tests (Math.Matrix, Vector, Meshes, World)
npm run e2e          # Playwright canvas-verification E2E suite
npm run build        # production bundle → dist/index.js
```

Toolchain: spago + purs 0.15.16 + esbuild. PureScript is formatted with
`purs-tidy`.

## Architecture

A pure functional core (`Math.Matrix`, `Vector`, `Meshes`, `World`) feeds a thin
WebGL2 FFI layer (`Graphics.GL`) driven by an FRP render loop (`FRP.Loop`). The
scene is an `Entity` list; each entity carries either a `Solid` or `Wire` mesh
and a `State -> Matrix` model transform (the world meshes use constant
transforms). See [`CLAUDE.md`](./CLAUDE.md) for the full module map.

## Deploy

Pushing to `master` builds and deploys to GitHub Pages
(`.github/workflows/jekyll-gh-pages.yml`).
