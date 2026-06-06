# vectors

A small **PureScript + WebGL2** 3D graphics demo with two scenes, toggled by an
on-screen switch:

- **Cube POC** — a solid-lit cube + orbiting satellite inside a simple world
  (green ground, wireframe grid, sky-blue horizon).
- **atomos** — a 3D atom visualizer in deep space: a nucleus of proton (red) and
  neutron (gray) spheres with electrons on animated orbital rings, configurable
  by element (Z = 1..36, H…Kr). Electrons fill **sub-shells** in Madelung/Aufbau
  order (each subshell capped at 4ℓ+2); each sub-shell draws a **thin orbital ring
  line** tracing its orbit. An **element-name label**, a **scene-title banner**,
  and an **orbital-info overlay** (live electron configuration) — all anime.js
  text scramble, HTML overlay — react to the controls.

## Controls

- **Switch scene** (top-left) — toggle Cube POC ↔ atomos.
- **Element Z** (atomos) — choose the atom (Z = 1..36: H … Kr); nucleus and
  sub-shell electron rings update live.
- **Arrow keys / mouse drag** — rotate the cube (Cube POC).
- **Shear input + Apply** — shear the main cube by the entered value
  (`x' = x + k·y`); repeated clicks compound.

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
