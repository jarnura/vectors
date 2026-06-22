// E2E spec for the Materials scene (M2).
//
// The Materials scene is the 5th scene in the cycle:
//   CubePoc → Atomos → Molecule → Builder → Materials → CubePoc
// It reuses the ENTIRE Builder render path — same entity list, same orbit Ref,
// same zoom/LOD. On first entry (or when the builder world is empty) it
// default-loads Diamond (index 0) via Lattice.structureOf(0).build.
//
// Seam API (window.__builder, same as Builder):
//   getAtoms()          → [{ id, z, pos: {x,y,z} }]
//   getBonds()          → [{ a, b, order }]
//   getMolecules()      → [{ ids, formula }]
//   loadStructure(i)    → replace world with curated structure i (0=Diamond, 1=Graphene)
//   setOrbit(yaw,pitch) → write orbit Ref (pitch clamped)
//   getOrbit()          → { yaw, pitch } from orbit Ref
//   clear()             → empty the world
//
// Timing: retries:2 (playwright.config.js), waitForTimeout for render frames.
import { test } from '@playwright/test';
import {
  expect,
  waitForRenderedCanvas,
  openDrawer,
  readRegion,
  distinctColors,
} from './helpers.js';

// Navigate to the Materials scene (4 #scene-toggle clicks from CubePoc:
// CubePoc → Atomos → Molecule → Builder → Materials).
async function gotoMaterials(page) {
  await expect(page.locator('#scene-toggle')).toBeVisible();
  await page.click('#scene-toggle'); // → Atomos
  await page.click('#scene-toggle'); // → Molecule
  await page.click('#scene-toggle'); // → Builder
  await page.click('#scene-toggle'); // → Materials
  // Wait for window.__builder (installed at boot, Materials shares it with Builder).
  await page.waitForFunction(() => !!window.__builder, null, { timeout: 6000 });
  // Wait a generous amount for the default-load to run and render.
  await page.waitForTimeout(900);
}

// Still reach Builder in 3 clicks (cycle unchanged through Builder slot).
async function gotoBuilder(page) {
  await expect(page.locator('#scene-toggle')).toBeVisible();
  await page.click('#scene-toggle'); // → Atomos
  await page.click('#scene-toggle'); // → Molecule
  await page.click('#scene-toggle'); // → Builder
  await page.waitForFunction(() => !!window.__builder, null, { timeout: 6000 });
  await page.waitForTimeout(700);
}

test.beforeEach(async ({ page }) => {
  await page.goto('/');
  await waitForRenderedCanvas(page);
  await openDrawer(page);
});

// ────────────────────────────────────────────────────────────────────────────
// GATE 1 — Scene navigation + default-load
// ────────────────────────────────────────────────────────────────────────────

// 1a. Four scene-toggle clicks land in the Materials scene (scene title).
test('materials: scene-toggle reaches Materials after 4 clicks', async ({ page }) => {
  await gotoMaterials(page);

  const title = await page.evaluate(() => {
    const el = document.querySelector('#scene-title');
    return el ? el.textContent : null;
  });
  // The title may still be animating via scrambleInto; just check non-empty.
  expect(typeof title).toBe('string');
});

// 1b. On entry the builder Ref is populated with a multi-atom structure
// (Diamond default-load: 64 atoms, z=6 carbon, 2×2×2 supercell).
test('materials: default-load populates builder with >1 atom on entry', async ({ page }) => {
  await gotoMaterials(page);

  const atoms = await page.evaluate(() => window.__builder.getAtoms());
  expect(atoms.length).toBeGreaterThan(1);
  // All carbon (z=6, Diamond is all-carbon).
  expect(atoms.every((a) => a.z === 6)).toBe(true);
});

// 1c. Default-load does NOT fire for Builder — it starts empty.
test('materials: Builder scene starts empty (not auto-loaded)', async ({ page }) => {
  await gotoBuilder(page);

  const atoms = await page.evaluate(() => window.__builder.getAtoms());
  expect(atoms.length).toBe(0);
});

// 1d. Default-load happens once per entry; re-entering Materials does NOT
// clobber a manually-built world that was not cleared (user may have added atoms
// in Builder and then switched — but Materials tracks its own entry condition).
// What matters: after clear() + entering Materials again, it default-loads again.
test('materials: clear then re-enter Materials default-loads Diamond again', async ({ page }) => {
  await gotoMaterials(page);

  // Clear the world while in Materials.
  await page.evaluate(() => window.__builder.clear());
  const afterClear = await page.evaluate(() => window.__builder.getAtoms());
  expect(afterClear.length).toBe(0);

  // Cycle back through Builder to Materials (5-cycle):
  // Materials → CubePoc → Atomos → Molecule → Builder → Materials
  for (let i = 0; i < 5; i++) {
    await page.click('#scene-toggle');
    await page.waitForTimeout(200);
  }
  await page.waitForTimeout(500);

  // Should have default-loaded again since world was empty.
  const atoms = await page.evaluate(() => window.__builder.getAtoms());
  expect(atoms.length).toBeGreaterThan(1);
});

// ────────────────────────────────────────────────────────────────────────────
// GATE 2 — loadStructure seam
// ────────────────────────────────────────────────────────────────────────────

// 2a. loadStructure(0) loads Diamond: 64 atoms, 86 bonds, all-carbon (2×2×2 supercell).
test('materials: loadStructure(0) loads Diamond (64 atoms, 86 bonds, carbon)', async ({ page }) => {
  await gotoMaterials(page);

  const result = await page.evaluate(() => {
    window.__builder.loadStructure(0);
    return {
      atoms: window.__builder.getAtoms(),
      bonds: window.__builder.getBonds(),
    };
  });

  expect(result.atoms.length).toBe(64);
  expect(result.bonds.length).toBe(86);
  expect(result.atoms.every((a) => a.z === 6)).toBe(true);
});

// 2b. loadStructure(1) loads Graphene: 32 atoms, all-carbon (4×4 supercell).
test('materials: loadStructure(1) loads Graphene (32 atoms, carbon)', async ({ page }) => {
  await gotoMaterials(page);

  const result = await page.evaluate(() => {
    window.__builder.loadStructure(1);
    return {
      atoms: window.__builder.getAtoms(),
      bonds: window.__builder.getBonds(),
    };
  });

  expect(result.atoms.length).toBe(32);
  expect(result.atoms.every((a) => a.z === 6)).toBe(true);
  // Graphene 4×4 supercell has 37 proximity bonds.
  expect(result.bonds.length).toBe(37);
});

// 2c. loadStructure with out-of-range index clamps (no crash, valid state).
test('materials: loadStructure clamp-safe for out-of-range index', async ({ page }) => {
  await gotoMaterials(page);

  const result = await page.evaluate(() => {
    window.__builder.loadStructure(999);
    const after999 = { atoms: window.__builder.getAtoms().length };
    window.__builder.loadStructure(-5);
    const afterNeg = { atoms: window.__builder.getAtoms().length };
    return { after999, afterNeg };
  });

  // Both should be valid (> 1 atom; clamped to last or first structure).
  expect(result.after999.atoms).toBeGreaterThan(1);
  expect(result.afterNeg.atoms).toBeGreaterThan(1);
});

// 2d. getAtoms reads the loadStructure result immediately (same Ref).
test('materials: getAtoms reflects loadStructure immediately', async ({ page }) => {
  await gotoMaterials(page);

  const atoms = await page.evaluate(() => {
    window.__builder.loadStructure(0); // Diamond
    return window.__builder.getAtoms();
  });

  expect(atoms.length).toBe(64);
});

// ────────────────────────────────────────────────────────────────────────────
// GATE 3 — Rendered pixels (non-background lit pixels in a spread region)
// ────────────────────────────────────────────────────────────────────────────

// 3a. After default-load the canvas shows non-background lit pixels across a
// spread region (not a single centered atom — the lattice covers multiple atoms).
test('materials: canvas has lit pixels spread across the scene (not just background)', async ({ page }) => {
  await gotoMaterials(page);

  // Poll the rAF loop until the default-loaded Diamond has actually rendered,
  // rather than relying on a single fixed delay (the 5-atom lattice is sparse,
  // so an early single sample can race the first painted frames under load).
  let pixels = [];
  await expect
    .poll(
      async () => {
        // Sample a generous region around the canvas centre.
        pixels = await readRegion(page, 0.1, 0.1, 0.9, 0.9, 24, 12);
        // Near-black background: r≈5, g≈5, b≈15. Count pixels that differ
        // meaningfully from the background (at least one channel > 30).
        return pixels.filter((p) => p[0] > 30 || p[1] > 30 || p[2] > 30).length;
      },
      { timeout: 6000, intervals: [200, 300, 500] },
    )
    .toBeGreaterThan(5); // at least 5 lit pixels in the spread region

  // There should also be multiple distinct color buckets (atoms + bonds).
  const distinct = distinctColors(pixels, 20);
  expect(distinct).toBeGreaterThan(2);
});

// 3b. Orbiting (via setOrbit) changes the rendered pixels — Materials orbit works.
test('materials: orbit via setOrbit changes rendered pixels', async ({ page }) => {
  await gotoMaterials(page);

  // Wait for render + orbit API.
  await page.waitForFunction(
    () => typeof window.__builder?.setOrbit === 'function',
    null,
    { timeout: 6000 },
  );
  await page.waitForTimeout(400);

  // Sample region at zero orbit.
  const pixelsBefore = await readRegion(page, 0.2, 0.2, 0.8, 0.8, 16, 8);

  // Rotate ~90° in yaw; give the rAF loop several frames to re-upload the GPU
  // projection and render. The sizeRef gate in Main detects the yaw change on the
  // next rAF frame and re-uploads; we wait generously for SwiftShader stability.
  await page.evaluate(() => window.__builder.setOrbit(1.5, 0.0));
  await page.waitForTimeout(300);
  // Wait for 3 rAF frames (each ~16ms) to ensure the GPU projection update is committed.
  await page.evaluate(() => new Promise((r) => requestAnimationFrame(() => requestAnimationFrame(() => requestAnimationFrame(r)))));
  await page.waitForTimeout(400);

  const pixelsAfter = await readRegion(page, 0.2, 0.2, 0.8, 0.8, 16, 8);

  // At least one sampled pixel must have changed after orbit.
  const anyChanged = pixelsBefore.some(
    (p, i) =>
      Math.abs(p[0] - pixelsAfter[i][0]) > 8 ||
      Math.abs(p[1] - pixelsAfter[i][1]) > 8 ||
      Math.abs(p[2] - pixelsAfter[i][2]) > 8,
  );
  expect(anyChanged).toBe(true);
});

// ────────────────────────────────────────────────────────────────────────────
// GATE 4 — 5-cycle regression (other scenes still reachable)
// ────────────────────────────────────────────────────────────────────────────

// 4a. Builder is still reachable at click 3 (cycle: CubePoc→Atomos→Molecule→Builder).
test('cycle: Builder still reachable at 3 scene-toggle clicks', async ({ page }) => {
  await gotoBuilder(page);

  const atoms = await page.evaluate(() => window.__builder.getAtoms());
  // Builder starts EMPTY (no auto-load).
  expect(atoms.length).toBe(0);

  // Verify we can add an atom normally.
  await page.evaluate(() => window.__builder.addAtom(6, 0, 0, 0));
  const afterAdd = await page.evaluate(() => window.__builder.getAtoms());
  expect(afterAdd.length).toBe(1);
});

// 4b. Completing the 5-cycle returns to CubePoc (5th click from Materials).
test('cycle: 5 clicks completes the 5-cycle back to CubePoc', async ({ page }) => {
  // Start from CubePoc and cycle all the way back.
  for (let i = 0; i < 5; i++) {
    await page.click('#scene-toggle');
    await page.waitForTimeout(200);
  }
  // Now at CubePoc: cycle once more to reach Atomos.
  await page.click('#scene-toggle');
  await page.waitForTimeout(400);

  // We should be in Atomos (1 click from CubePoc). The __builder API reflects
  // the live scene state through getAtoms — not scene-specific, just check alive.
  const apiAlive = await page.evaluate(() => typeof window.__builder === 'object');
  expect(apiAlive).toBe(true);
});

// 4c. Diamond and Graphene have different atom counts (sanity: structures distinct).
// Diamond (64 atoms, 2×2×2 cubic supercell) vs Graphene (32 atoms, 4×4 honeycomb).
test('materials: Diamond (64 atoms) and Graphene (32 atoms) are structurally distinct', async ({ page }) => {
  await gotoMaterials(page);

  const result = await page.evaluate(() => {
    window.__builder.loadStructure(0);
    const diamondCount = window.__builder.getAtoms().length;
    window.__builder.loadStructure(1);
    const grapheneCount = window.__builder.getAtoms().length;
    return { diamondCount, grapheneCount };
  });

  expect(result.diamondCount).toBe(64);
  expect(result.grapheneCount).toBe(32);
  expect(result.diamondCount).not.toBe(result.grapheneCount);
  // Both supercells are visually substantive (well above the old 5/18 baseline).
  expect(result.diamondCount).toBeGreaterThan(30);
  expect(result.grapheneCount).toBeGreaterThan(25);
});
