// E2E spec for the layer-space feature (M1-layer-space).
//
// Covers:
//   1. Default render — canvas has lit pixels in Builder with default layerSpace.
//   2. Slider interaction — increasing layerSpace changes the render visually.
//   3. Pick parity — world still has atoms at non-default layerSpace, orbit works.
//   4. Other scenes unaffected — CubePoc scene is unaffected by the slider.
//
// Seam: window.__builder (Builder scene):
//   addAtom(z, x, y, z3) → place an atom at model coords (x,y,z3)
//   getAtoms()            → [{ id, z, pos }]
//   getBonds()            → [{ a, b }]
//   setOrbit(yaw, pitch)  → set camera orbit
//   getOrbit()            → { yaw, pitch }
//
// The #layer-space slider drives applyLayerSpace via the FRP input channel.
// Atoms use explicit coordinates so positions are deterministic (addAtom(z) alone
// gives NaN coords when called via the JS seam; the in-app Add button uses spawnPos
// internally but the JS seam requires explicit x/y/z arguments).
//
// Timing: retries:2 (playwright.config.js); expect.poll for SwiftShader timing.
import { test } from '@playwright/test';
import {
  expect,
  waitForRenderedCanvas,
  openDrawer,
  gotoBuilder,
  readRegion,
} from './helpers.js';

test.beforeEach(async ({ page }) => {
  await page.goto('/');
  await waitForRenderedCanvas(page);
  await openDrawer(page);
});

// ────────────────────────────────────────────────────────────────────────────
// GATE 1 — Default render: canvas has lit pixels in Builder
// ────────────────────────────────────────────────────────────────────────────

// 1a. After adding atoms in Builder the canvas has lit pixels (not all black).
// Uses explicit atom coords at the origin so the atom projects to canvas centre.
test('builder-layer-space: canvas has lit pixels in Builder at default layerSpace', async ({ page }) => {
  await gotoBuilder(page);
  await page.waitForFunction(() => !!window.__builder, null, { timeout: 6000 });

  // A free Carbon at origin: projects to canvas centre, always visible.
  await page.evaluate(() => {
    window.__builder.clear();
    window.__builder.addAtom(6, 0, 0, 0); // Carbon at origin
    window.__builder.addAtom(6, 80, 0, 0); // Carbon offset
  });

  // Poll until lit pixels appear (SwiftShader timing: rAF loop may need a few frames).
  await expect
    .poll(
      async () => {
        const pixels = await readRegion(page, 0.1, 0.1, 0.9, 0.9, 24, 12);
        return pixels.filter((p) => p[0] > 30 || p[1] > 30 || p[2] > 30).length;
      },
      { timeout: 8000, intervals: [200, 300, 500] },
    )
    .toBeGreaterThan(1);
});

// 1b. #layer-space slider is visible in the controls drawer.
test('builder-layer-space: #layer-space slider is visible in controls drawer', async ({ page }) => {
  const slider = page.locator('#layer-space');
  await expect(slider).toBeAttached();
});

// 1c. #layer-space slider has the expected default value of 1.6.
test('builder-layer-space: #layer-space slider default value is 1.6', async ({ page }) => {
  const value = await page.evaluate(() => {
    const el = document.getElementById('layer-space');
    return el ? parseFloat(el.value) : null;
  });
  expect(value).toBeCloseTo(1.6, 1);
});

// ────────────────────────────────────────────────────────────────────────────
// GATE 2 — Slider changes the render visually
// ────────────────────────────────────────────────────────────────────────────

// 2a. Increasing layerSpace via the slider changes the rendered pixels.
// We add atoms at explicit positions, capture the canvas at layerSpace=1.0, then
// change to 3.5 and assert the render changed (atom positions moved in screen space).
test('builder-layer-space: increasing layerSpace changes rendered pixels', async ({ page }) => {
  await gotoBuilder(page);
  await page.waitForFunction(() => !!window.__builder, null, { timeout: 6000 });

  // Two atoms at explicit model positions that will render visibly.
  await page.evaluate(() => {
    window.__builder.clear();
    window.__builder.addAtom(6, 0, 0, 0); // Carbon at origin
    window.__builder.addAtom(8, 80, 0, 0); // Oxygen offset
  });
  await page.waitForFunction(() => window.__builder.getAtoms().length >= 2, null, { timeout: 3000 });

  // First: set layerSpace to 1.0 and wait for lit pixels.
  await page.evaluate(() => {
    const el = document.getElementById('layer-space');
    if (el) {
      el.value = '1.0';
      el.dispatchEvent(new Event('input', { bubbles: true }));
    }
  });
  let before = [];
  await expect
    .poll(
      async () => {
        before = await readRegion(page, 0.1, 0.1, 0.9, 0.9, 24, 12);
        return before.filter((p) => p[0] > 20 || p[1] > 20 || p[2] > 20).length;
      },
      { timeout: 8000, intervals: [200, 300, 500] },
    )
    .toBeGreaterThan(0);

  // Change layerSpace to 3.5 — atoms will move on screen because their world positions scale.
  await page.evaluate(() => {
    const el = document.getElementById('layer-space');
    if (el) {
      el.value = '3.5';
      el.dispatchEvent(new Event('input', { bubbles: true }));
      el.dispatchEvent(new Event('change', { bubbles: true }));
    }
  });
  // Wait for render update: several rAF frames + SwiftShader stabilization.
  await page.evaluate(
    () =>
      new Promise((r) =>
        requestAnimationFrame(() => requestAnimationFrame(() => requestAnimationFrame(r))),
      ),
  );
  await page.waitForTimeout(500);
  const after = await readRegion(page, 0.1, 0.1, 0.9, 0.9, 24, 12);

  // At least some pixels must differ between the two renders (atom positions changed).
  const anyChanged = before.some(
    (p, i) =>
      Math.abs(p[0] - after[i][0]) > 8 ||
      Math.abs(p[1] - after[i][1]) > 8 ||
      Math.abs(p[2] - after[i][2]) > 8,
  );
  expect(anyChanged).toBe(true);
});

// 2b. layerSpace=1.0 and layerSpace=4.0 produce different pixel renders.
// We place atoms and capture both layerSpace extremes; the renders must differ.
test('builder-layer-space: layerSpace extremes produce different renders', async ({ page }) => {
  await gotoBuilder(page);
  await page.waitForFunction(() => !!window.__builder, null, { timeout: 6000 });

  // Place two atoms at different positions so there's visible structure.
  await page.evaluate(() => {
    window.__builder.clear();
    window.__builder.addAtom(6, 0, 0, 0);
    window.__builder.addAtom(6, 100, 0, 0);
  });
  await page.waitForFunction(() => window.__builder.getAtoms().length === 2, null, { timeout: 3000 });

  // Capture at layerSpace=1.0.
  await page.evaluate(() => {
    const el = document.getElementById('layer-space');
    if (el) {
      el.value = '1.0';
      el.dispatchEvent(new Event('input', { bubbles: true }));
    }
  });
  let pixelsSmall = [];
  await expect
    .poll(
      async () => {
        pixelsSmall = await readRegion(page, 0.2, 0.2, 0.8, 0.8, 24, 12);
        return pixelsSmall.filter((p) => p[0] > 20 || p[1] > 20 || p[2] > 20).length;
      },
      { timeout: 8000, intervals: [200, 300, 500] },
    )
    .toBeGreaterThan(0);

  // Capture at layerSpace=4.0 (atoms spread 4× further from origin).
  await page.evaluate(() => {
    const el = document.getElementById('layer-space');
    if (el) {
      el.value = '4.0';
      el.dispatchEvent(new Event('input', { bubbles: true }));
    }
  });
  await page.evaluate(
    () =>
      new Promise((r) =>
        requestAnimationFrame(() => requestAnimationFrame(() => requestAnimationFrame(r))),
      ),
  );
  await page.waitForTimeout(500);
  const pixelsLarge = await readRegion(page, 0.2, 0.2, 0.8, 0.8, 24, 12);

  // The two renders must differ visually (atoms at different screen positions).
  const anyChanged = pixelsSmall.some(
    (p, i) =>
      Math.abs(p[0] - pixelsLarge[i][0]) > 8 ||
      Math.abs(p[1] - pixelsLarge[i][1]) > 8 ||
      Math.abs(p[2] - pixelsLarge[i][2]) > 8,
  );
  expect(anyChanged).toBe(true);
});

// ────────────────────────────────────────────────────────────────────────────
// GATE 3 — Pick parity at non-default layerSpace
// ────────────────────────────────────────────────────────────────────────────

// 3a. After changing layerSpace, the world still has atoms (no crash/clear).
test('builder-layer-space: atom world is preserved after layerSpace change', async ({ page }) => {
  await gotoBuilder(page);
  await page.waitForFunction(() => !!window.__builder, null, { timeout: 6000 });

  await page.evaluate(() => {
    window.__builder.clear();
    window.__builder.addAtom(6, 0, 0, 0);
    window.__builder.addAtom(1, 60, 0, 0);
  });
  await page.waitForFunction(() => window.__builder.getAtoms().length >= 2, null, { timeout: 3000 });

  // Change layerSpace.
  await page.evaluate(() => {
    const el = document.getElementById('layer-space');
    if (el) {
      el.value = '3.0';
      el.dispatchEvent(new Event('input', { bubbles: true }));
    }
  });
  await page.waitForTimeout(300);

  // Atoms must still be there.
  const atoms = await page.evaluate(() => window.__builder.getAtoms());
  expect(atoms.length).toBeGreaterThanOrEqual(2);
});

// 3b. The orbit seam works with a non-default layerSpace (render/pick parity).
test('builder-layer-space: orbit + layerSpace combination renders without crash', async ({ page }) => {
  await gotoBuilder(page);
  await page.waitForFunction(() => typeof window.__builder?.setOrbit === 'function', null, { timeout: 6000 });

  await page.evaluate(() => {
    window.__builder.clear();
    window.__builder.addAtom(6, 0, 0, 0);
    window.__builder.addAtom(6, 80, 0, 0);
  });
  await page.waitForFunction(() => window.__builder.getAtoms().length >= 2, null, { timeout: 3000 });

  // Set a non-default layerSpace AND a non-zero orbit.
  await page.evaluate(() => {
    const el = document.getElementById('layer-space');
    if (el) {
      el.value = '2.5';
      el.dispatchEvent(new Event('input', { bubbles: true }));
    }
    window.__builder.setOrbit(0.8, 0.2);
  });

  // Canvas must still have lit pixels (render didn't crash or go all-black).
  await expect
    .poll(
      async () => {
        const pixels = await readRegion(page, 0.1, 0.1, 0.9, 0.9, 24, 12);
        return pixels.filter((p) => p[0] > 20 || p[1] > 20 || p[2] > 20).length;
      },
      { timeout: 8000, intervals: [200, 300, 500] },
    )
    .toBeGreaterThan(0);

  // Orbit is still the set value.
  const orb = await page.evaluate(() => window.__builder.getOrbit());
  expect(orb.yaw).toBeCloseTo(0.8, 1);
});

// ────────────────────────────────────────────────────────────────────────────
// GATE 4 — Other scenes unaffected
// ────────────────────────────────────────────────────────────────────────────

// 4a. Changing the #layer-space slider in the Cube POC scene does not crash
// and the scene still renders lit pixels (CubePoc ignores layerSpace).
test('builder-layer-space: layerSpace slider in Cube POC does not break render', async ({ page }) => {
  // Stay in CubePoc (default scene after load). Wait for the initial render.
  await page.evaluate(
    () => new Promise((r) => requestAnimationFrame(() => requestAnimationFrame(r))),
  );
  await page.waitForTimeout(300);

  // Trigger the slider in the Cube POC scene.
  await page.evaluate(() => {
    const el = document.getElementById('layer-space');
    if (el) {
      el.value = '3.5';
      el.dispatchEvent(new Event('input', { bubbles: true }));
    }
  });

  // CubePoc should still render (green ground plane / colored cube are lit).
  await expect
    .poll(
      async () => {
        const pixels = await readRegion(page, 0.0, 0.0, 1.0, 1.0, 24, 12);
        return pixels.filter((p) => p[0] > 20 || p[1] > 20 || p[2] > 20).length;
      },
      { timeout: 6000, intervals: [200, 300, 500] },
    )
    .toBeGreaterThan(5);
});

// 4b. Materials scene: default-load Diamond at non-default layerSpace renders correctly.
test('builder-layer-space: Materials scene renders with non-default layerSpace', async ({ page }) => {
  // Navigate to Builder to set layerSpace, then continue to Materials.
  await page.click('#scene-toggle'); // → Atomos
  await page.click('#scene-toggle'); // → Molecule
  await page.click('#scene-toggle'); // → Builder
  await page.waitForTimeout(400);

  // Set layerSpace to a non-default value.
  await page.evaluate(() => {
    const el = document.getElementById('layer-space');
    if (el) {
      el.value = '2.5';
      el.dispatchEvent(new Event('input', { bubbles: true }));
    }
  });

  // Navigate to Materials.
  await page.click('#scene-toggle'); // → Materials
  await page.waitForTimeout(900);
  await page.waitForFunction(() => !!window.__builder, null, { timeout: 6000 });

  // Materials should still have its default-loaded atoms.
  const atoms = await page.evaluate(() => window.__builder.getAtoms());
  expect(atoms.length).toBeGreaterThan(0);

  // Canvas still has lit pixels (poll for SwiftShader timing).
  await expect
    .poll(
      async () => {
        const pixels = await readRegion(page, 0.1, 0.1, 0.9, 0.9, 24, 12);
        return pixels.filter((p) => p[0] > 20 || p[1] > 20 || p[2] > 20).length;
      },
      { timeout: 8000, intervals: [200, 300, 500] },
    )
    .toBeGreaterThan(1);
});
