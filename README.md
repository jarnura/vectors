# vectors

A small **PureScript + WebGL2** 3D graphics demo — a vertical-sliced
**"learn matter"** platform (atoms → chemistry → properties of matter → element transformation) — with
six scenes, toggled by an on-screen switch (6-cycles: Cube POC → atomos →
Molecule → Builder → Materials → Nuclide → Cube POC):

- **Cube POC** — a solid-lit cube + orbiting satellite inside a simple world
  (green ground, wireframe grid, sky-blue horizon).
- **atomos** — a 3D atom visualizer in deep space: a nucleus of proton (red) and
  neutron (gray) spheres with electrons on animated orbital rings, configurable
  by element (Z = 1..36, H…Kr). Electrons fill **sub-shells** in Madelung/Aufbau
  order; each sub-shell draws a **thin orbital ring line**, and **discrete
  electrons** (bright spheres) orbit on those rings — **colour-coded by shell**
  (each shell a distinct colour, sub-shells lighter). A **2D toggle** flattens
  the atom into a Bohr diagram (concentric circles facing the camera) versus the
  tilted 3D orbital system. An **element-name label**, a
  **scene-title banner**, and an **orbital-info overlay** (live electron
  configuration) — anime.js text scramble, HTML overlay — react to the controls.
- **Molecule** — an **H₂ molecule**: two hydrogen nuclei whose covalent bond is
  drawn as a **shared electron pair** in the internuclear overlap (no stick). A
  **Form-bond button** runs an anime.js animation drawing the atoms together, and
  an animated **properties panel** (`#molecule-info`) lists the molecule's
  properties, populated data-driven from an open-ended molecule model. All
  molecule controls are anime.js driven. (Slice 1 of the chemistry layer.)
- **Builder** — **slice 2**, an interactive **molecule-builder sandbox**. From a
  modern **glassy (backdrop-blur) anime.js control bar** you **Add** atoms of any
  element (Z = 1..36) and **Clear** the world, then **drag atoms in 3D** with the
  mouse. Atoms that come near each other **auto-bond, valence-aware** (H=1, C=4,
  N=3, O=2, … with break hysteresis); connected atoms form a **molecule** with a
  derived Unicode formula (e.g. `H₂O`). Each atom renders its **real per-element
  nucleus** (Carbon/Oxygen denser/larger than Hydrogen), sits at a **per-element
  size** (normalised covalent radius — Hydrogen smallest), and shows its **atomic
  symbol** (H, C, O, …) as an HTML overlay label; each bond shows a **shared
  electron pair**, and atoms carry only their **lone electrons** (valence-conserved).
  The Builder background is **plain space — no starfield** (atomos and Molecule
  keep theirs).
  An atom's **valence** electrons (outermost shell + the bonding pair) render in
  **amber**, vs its **core** inner-shell electrons in **blue**; a **"Valence only"
  toggle** hides the blue core electrons, leaving just the amber valence + bonds.
  The controls live in a **left drawer** opened by a **panel-toggle icon** (below
  the scene title) that slides the glassy panel in/out via anime.js. **Zooming
  drives a smooth level-of-detail** on each atom: zoom **out** and each atom
  collapses to a single element-coloured ball; zoom **in** and it blooms into its
  real nucleus + electrons. The in-between is a continuous, frame-eased cross-fade
  (the **zoom slider** moves smoothly — no abrupt swap, no zoom reset),
  reusing the existing camera zoom; bonds stay visible in both layers — drawn as
  **connecting lines** between atom-balls when zoomed out and as the shared
  electron pair when zoomed in — and "Valence only" still applies.
- **Materials** — **slice 3**: a curated **crystal-structure gallery**. Reuses
  the entire Builder render path (same atom/bond/electron entities, same orbit
  Ref, same zoom/LOD, same layer-space, same `#valence-only` toggle). A **glassy cards gallery**
  (`#materials-cards`) shows one card per curated structure; clicking a card
  loads it via the shared `BuilderState` Ref and fires an eager re-render. A
  **data-driven info panel** (`#materials-info`) shows the selected structure's
  Name, Formula, Hybridization, Coordination, Bond length, and a real-world note
  — updated on card click, on default-load (Diamond on first entry), and when
  `window.__builder.loadStructure(i)` is called through the automation seam.
  Current structures: **Diamond** (sp³, tetrahedral, coord 4) and **Graphene**
  (sp², honeycomb, coord 3). A 3rd material is data-only: add one record to the
  `Lattice.structures` registry and it appears in the gallery automatically.
- **Nuclide** — **element layer**: a nuclear-physics sandbox. A nucleus (Z protons + N neutrons, rendered as coloured spheres) is displayed alongside a **data-driven `#nuclide-info` panel** showing Symbol, Name, Z, A (mass number), **binding energy + binding-per-nucleon in MeV**, **stability/decay mode** (Stable, BetaMinus, BetaPlusEC, Alpha, or Unbound), and a "Last reaction" row (products, Q-value in MeV, RELEASED/ABSORBED). **Transmutation controls** allow manual isotope exploration (`#nuc-add-proton`/`#nuc-remove-proton`/`#nuc-add-neutron`/`#nuc-remove-neutron` + manual Z/N inputs). **Named reactions** (`#react-alpha`/`#react-beta-minus`/`#react-beta-plus`/`#react-fuse`/`#react-fission`) conserve Z & A and compute **Q-values** from binding energies (positive = exothermic RELEASED; negative = endothermic ABSORBED). The **decay classifier** uses N/Z stability band heuristics to flag Unbound nucleon-unbound states (He-5, Li-5, Be-8, etc.). Teaching simplifications: SEMF is poor for light nuclei — fixed measured binding energies anchor canonical reactions (α-decay, D-T fusion). Default: Carbon-12 (Z=6, N=6, stable).

## Controls

- **Switch scene** (top-left) — cycle Cube POC → atomos → Molecule → Builder → Materials → Nuclide → Cube POC.
- **Element Z** (atomos) — choose the atom (Z = 1..36: H … Kr); nucleus and
  sub-shell electron rings update live.
- **2D checkbox** (atomos) — flatten the atom into a 2D Bohr diagram (concentric
  circles facing the camera); uncheck to restore the tilted 3D orbital view.
- **Sub-shells checkbox** (atomos, checked by default) — show the sub-shell ring
  view (one thin ring per filled sub-shell, sub-shell colours); uncheck for a
  shell-only view (one ring per principal shell, collapsed from all sub-shells,
  shell colours).
- **Form bond** (Molecule) — run the anime.js bond-formation animation that draws
  the two hydrogen atoms together into the H₂ covalent bond.
- **Add / Clear** (Builder) — Add an atom of the selected element (Z = 1..36) to
  the sandbox, or Clear all atoms.
- **Drag atoms** (Builder) — drag atoms in 3D with the mouse; atoms that come
  close auto-bond (valence-aware, with break hysteresis) into molecules.
- **Move a molecule** (Builder) — **single-click + drag** moves the whole
  connected molecule (all bonded atoms together); **double-click an atom then
  drag** moves just that one atom.
- **Valence only checkbox** (Builder + Materials) — hide the blue core (inner-shell)
  electrons, leaving only an atom's amber valence electrons and the bonding pairs.
- **Panel toggle** (icon below the scene title) — slide the controls drawer in
  from the left (and back out) via anime.js.
- **Mouse wheel** (all scenes) — zoom the camera: scroll out to pull back (e.g. to
  see many molecules at once), scroll in for detail.
- **Zoom slider** (all scenes, in the controls panel) — drag to set the camera zoom
  directly (0.05–5.0); updates two-way as wheel zoom and reframes change it.
- **Orbit D-pad buttons** (Builder + Materials, in the controls panel) — **↑↓←→ arrows** orbit
  the camera by a fixed step per click; **⊙ reset** returns to the default orbit
  angle. Orbit has yaw+pitch DOF only (no roll).
- **Layer-space slider** (Builder + Materials, in the controls panel) — spread the atoms
  further apart (1.0–4.0, default 1.6); larger values increase the effective world
  scale and give the LOD bloom more room.
- **Zoom level-of-detail** (Builder + Materials) — the same camera zoom smoothly fades each
  atom between a single element-coloured ball (zoomed out) and its full nucleus +
  electrons (zoomed in), eased per frame.
- **Arrow keys / mouse drag** — rotate the cube (Cube POC).
- **Shear input + Apply** — shear the main cube by the entered value
  (`x' = x + k·y`); repeated clicks compound.
- **Transmutation buttons** (Nuclide) — `+p`, `−p`, `+n`, `−n` change proton/neutron count; **Reset** returns to Carbon-12.
- **Z / N inputs** (Nuclide) — manually set proton/neutron number (clamped 1..118 for Z; N ≥ 0).
- **Reaction buttons** (Nuclide) — **α decay**, **β− decay**, **β+ decay**, **Fuse**, **Fission** trigger named nuclear reactions; Fuse/Fission show Z/N input fields for target/fragment nuclides.

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

## Run with Docker (local)

Build and run the static site as a local container (multi-stage build: a glibc
Node builder runs `spago bundle`, then a tiny nginx image serves `index.html` +
`dist/`):

```bash
npm run docker:build   # docker build -t vectors:local .
npm run docker:run     # docker run --rm -p 8080:80 vectors:local
```

Then open http://localhost:8080. This is a local container only — distinct from
the GitHub Pages deploy below.

## Deploy

Pushing to `master` builds and deploys to GitHub Pages
(`.github/workflows/jekyll-gh-pages.yml`).
