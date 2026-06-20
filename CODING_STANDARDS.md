# Coding Standards

These standards apply to the `vectors` PureScript + WebGL2 codebase. They are
intentionally lightweight and describe how this repo is *already* organised, plus
the one hard, machine-enforced rule: the per-file line-count cap.

## Project organisation

- **Many small, focused modules.** Each module does one thing. Prefer extracting
  a new sub-module over growing an existing one.
- **Group by domain / namespace, not by file type.** Geometry lives in `Meshes`,
  camera math in `Camera`, the dynamic builder model in `Builder`, etc. — not in
  generic `utils`/`helpers` buckets.
- **New sub-modules live UNDER the parent namespace.** When a module grows too
  large, split it into children of its own namespace — e.g. `Builder.Bonds`,
  `Builder.Overlap` — rather than into a sibling or a generic module. This keeps
  module names and import paths stable for everything that already depends on the
  parent, and makes the split a mechanical move rather than a churn of imports.
- **Keep each FFI `.js` file paired with its `.purs` module** (e.g.
  `BuilderApi.purs` ↔ `BuilderApi.js`). Never split an FFI implementation away
  from its PureScript foreign-import declarations.

The authoritative description of every module and its responsibilities is the
**module map in [CLAUDE.md](./CLAUDE.md)** — treat that table as the source of
truth for where code belongs.

## Hard file-size cap

- **800 lines per file, hard maximum.** No tracked source file may exceed it.
- **Target ≤400 lines** for a typical file; treat anything approaching the cap as
  a signal to split along the namespace rules above.
- Enforced mechanically by **`npm run validate`** (see the gate below). The cap is
  overridable for local experiments via the `MAX_FILE_LINES` env var, but the
  committed default is 800 and CI/babysitter use the default.

The validator (`scripts/validate-standards.mjs`) scans tracked
`src/**/*.purs`, `test/**/*.purs`, `src/**/*.js`, and `e2e/**/*.js` files,
skipping `dist/`, `output/`, `.spago/`, `node_modules/`, and `.a5c/`. It prints a
single JSON object listing any offenders (sorted by descending line count) and
exits non-zero if any file exceeds the cap.

## PureScript conventions

These are already followed throughout the repo:

- **Pure and total where possible.** Keep geometry, model, and math modules free
  of `Effect`/WebGL; isolate side effects at the edges (`Main`, FFI modules).
- **Effect-typed FFI imports.** Any foreign import that performs a side effect is
  typed as `Effect` (or an `Effect`-returning function).
- **Immutable State.** State updates return new records — never mutate in place.
  The per-frame `State` record is advanced functionally each frame.
- **`purs-tidy` formatting.** PureScript is formatted with `purs-tidy`; keep it
  clean before committing.

## The validate-after-build gate

`npm run validate` runs `scripts/validate-standards.mjs` and **fails the gate if
any file exceeds the line cap**.

Ordering in babysitter processes:

1. It runs **sequentially AFTER `npm run build`** (`spago build`) — the build is
   the type-check gate, and validate runs once the build succeeds.
2. It runs **before the parallel format / unit / review gates**, so a structural
   violation is caught early.
3. A **hard 0-offender `validate`** is part of the **final / deploy gate** — a
   release must have zero files over the cap.

When adding a new babysitter process, add a `validate` shell task that runs
`npm run validate` immediately after that process's build task.

## Module-organisation note

For *where* a given piece of code belongs, defer to the **module map in
[CLAUDE.md](./CLAUDE.md)**. This document defines the rules; CLAUDE.md is the
living catalogue of modules those rules produced.
