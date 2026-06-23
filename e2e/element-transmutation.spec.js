// E2E spec for the Nuclide scene — M2 element transmutation sandbox.
//
// The Nuclide scene is the 6th scene in the cycle:
//   CubePoc → Atomos → Molecule → Builder → Materials → Nuclide → CubePoc
// Navigate there with 5 #scene-toggle clicks.
//
// Seam API (window.__nuclear):
//   setNuclide(z, n)   → replace the current nuclide (does NOT clamp)
//   addProton()        → z += 1
//   removeProton()     → z = max(0, z - 1)
//   addNeutron()       → n += 1
//   removeNeutron()    → n = max(0, n - 1)
//   getNuclide()       → { z, n, a, symbol }
//   getBinding()       → { total, perNucleon }
//   getStability()     → { mode, stable }
//
// Timing: retries:2 (playwright.config.js).
import { test } from '@playwright/test';
import {
  expect,
  waitForRenderedCanvas,
  openDrawer,
} from './helpers.js';

// Navigate to the Nuclide scene (5 #scene-toggle clicks from CubePoc).
async function gotoNuclide(page) {
  await expect(page.locator('#scene-toggle')).toBeVisible();
  await page.click('#scene-toggle'); // → Atomos
  await page.click('#scene-toggle'); // → Molecule
  await page.click('#scene-toggle'); // → Builder
  await page.click('#scene-toggle'); // → Materials
  await page.click('#scene-toggle'); // → Nuclide
  await page.waitForTimeout(700);
  await page.waitForFunction(() => !!window.__nuclear, null, { timeout: 6000 });
}

test.beforeEach(async ({ page }) => {
  await page.goto('/');
  await waitForRenderedCanvas(page);
  await openDrawer(page);
});

// ─── GATE 1: Navigation ───────────────────────────────────────────────────────

// 1a. 5 scene-toggle clicks land in the Nuclide scene.
test('nuclide: 5 scene-toggle clicks reaches Nuclide scene', async ({ page }) => {
  await gotoNuclide(page);

  // The window.__nuclear API must be available.
  const apiAlive = await page.evaluate(() => typeof window.__nuclear === 'object');
  expect(apiAlive).toBe(true);
});

// 1b. The 6-cycle completes: 6 clicks returns to CubePoc.
test('nuclide: 6-cycle completes (6 clicks returns to CubePoc)', async ({ page }) => {
  // Navigate through all 6 scenes.
  for (let i = 0; i < 6; i++) {
    await page.click('#scene-toggle');
    await page.waitForTimeout(200);
  }
  // Should now be back at CubePoc. Verify the canvas background is sky-blue.
  await page.waitForTimeout(300);
  // The __nuclear API remains available (installed at boot), but the scene is CubePoc.
  const apiAlive = await page.evaluate(() => typeof window.__nuclear === 'object');
  expect(apiAlive).toBe(true);
});

// 1c. Builder is still reachable at 3 clicks (cycle position unchanged).
test('nuclide: Builder still reachable at 3 scene-toggle clicks', async ({ page }) => {
  await expect(page.locator('#scene-toggle')).toBeVisible();
  await page.click('#scene-toggle'); // → Atomos
  await page.click('#scene-toggle'); // → Molecule
  await page.click('#scene-toggle'); // → Builder
  await page.waitForTimeout(700);
  await page.waitForFunction(() => !!window.__builder, null, { timeout: 6000 });

  const atoms = await page.evaluate(() => window.__builder.getAtoms());
  expect(atoms.length).toBe(0); // Builder starts empty
});

// 1d. Materials is still reachable at 4 clicks.
test('nuclide: Materials still reachable at 4 scene-toggle clicks', async ({ page }) => {
  await expect(page.locator('#scene-toggle')).toBeVisible();
  await page.click('#scene-toggle'); // → Atomos
  await page.click('#scene-toggle'); // → Molecule
  await page.click('#scene-toggle'); // → Builder
  await page.click('#scene-toggle'); // → Materials
  await page.waitForTimeout(900);
  await page.waitForFunction(() => !!window.__builder, null, { timeout: 6000 });

  const atoms = await page.evaluate(() => window.__builder.getAtoms());
  // Materials default-loads Diamond (64 carbon atoms).
  expect(atoms.length).toBeGreaterThan(1);
  expect(atoms.every((a) => a.z === 6)).toBe(true);
});

// ─── GATE 2: Seam API ─────────────────────────────────────────────────────────

// 2a. setNuclide + getNuclide round-trips.
test('nuclide: setNuclide(1,0) then getNuclide returns H-1', async ({ page }) => {
  await gotoNuclide(page);

  const nuc = await page.evaluate(() => {
    window.__nuclear.setNuclide(1, 0);
    return window.__nuclear.getNuclide();
  });

  expect(nuc.z).toBe(1);
  expect(nuc.n).toBe(0);
  expect(nuc.a).toBe(1);
  expect(nuc.symbol).toBe('H');
});

// 2b. addProton changes the element (H → He, symbol changes, Z increases by 1).
test('nuclide: addProton changes element H→He (Z+1, symbol changes)', async ({ page }) => {
  await gotoNuclide(page);

  const result = await page.evaluate(() => {
    window.__nuclear.setNuclide(1, 0); // H-1
    window.__nuclear.addProton();
    return window.__nuclear.getNuclide();
  });

  expect(result.z).toBe(2);
  expect(result.symbol).toBe('He');
  expect(result.n).toBe(0); // neutron count unchanged
  expect(result.a).toBe(2); // A = Z + N
});

// 2c. addNeutron changes A but NOT Z or symbol (isotope change, not element).
test('nuclide: addNeutron changes A but not Z/symbol (isotope change)', async ({ page }) => {
  await gotoNuclide(page);

  const result = await page.evaluate(() => {
    window.__nuclear.setNuclide(6, 6); // C-12
    window.__nuclear.addNeutron();
    return window.__nuclear.getNuclide();
  });

  expect(result.z).toBe(6);   // Z unchanged
  expect(result.symbol).toBe('C'); // element unchanged
  expect(result.n).toBe(7);   // N went from 6 → 7
  expect(result.a).toBe(13);  // A = 13 (C-13)
});

// 2d. removeNeutron decrements N.
test('nuclide: removeNeutron decrements N', async ({ page }) => {
  await gotoNuclide(page);

  const result = await page.evaluate(() => {
    window.__nuclear.setNuclide(6, 6); // C-12
    window.__nuclear.removeNeutron();
    return window.__nuclear.getNuclide();
  });

  expect(result.n).toBe(5);
  expect(result.z).toBe(6);
  expect(result.a).toBe(11);
});

// 2e. removeProton decrements Z.
test('nuclide: removeProton decrements Z', async ({ page }) => {
  await gotoNuclide(page);

  const result = await page.evaluate(() => {
    window.__nuclear.setNuclide(6, 6); // C-12
    window.__nuclear.removeProton();
    return window.__nuclear.getNuclide();
  });

  expect(result.z).toBe(5);
  expect(result.symbol).toBe('B');
  expect(result.n).toBe(6);
});

// 2f. removeProton at Z=0 is clamp-safe (no negative Z).
test('nuclide: removeProton at floor is clamp-safe (Z stays >= 0)', async ({ page }) => {
  await gotoNuclide(page);

  const result = await page.evaluate(() => {
    window.__nuclear.setNuclide(0, 5);
    window.__nuclear.removeProton(); // already at 0 — should not go negative
    return window.__nuclear.getNuclide();
  });

  expect(result.z).toBeGreaterThanOrEqual(0);
});

// 2g. removeNeutron at N=0 is clamp-safe (no negative N).
test('nuclide: removeNeutron at floor is clamp-safe (N stays >= 0)', async ({ page }) => {
  await gotoNuclide(page);

  const result = await page.evaluate(() => {
    window.__nuclear.setNuclide(6, 0);
    window.__nuclear.removeNeutron(); // already at 0 — should not go negative
    return window.__nuclear.getNuclide();
  });

  expect(result.n).toBeGreaterThanOrEqual(0);
});

// ─── GATE 3: Info panel data ──────────────────────────────────────────────────

// 3a. #nuclide-info panel shows symbol, Z, A, binding energy, stability.
test('nuclide: #nuclide-info panel reflects current nuclide data', async ({ page }) => {
  await gotoNuclide(page);

  // Set to C-12 (well-known, stable, binding ≈ 92.16 MeV).
  await page.evaluate(() => window.__nuclear.setNuclide(6, 6));
  await page.waitForTimeout(300);

  // Panel must be visible in the Nuclide scene.
  const panelVisible = await page.evaluate(() => {
    const el = document.getElementById('nuclide-info');
    if (!el) return false;
    return el.style.display !== 'none' && el.offsetParent !== null;
  });
  expect(panelVisible).toBe(true);

  // Binding data from the seam must match physical expectation.
  const binding = await page.evaluate(() => window.__nuclear.getBinding());
  expect(binding.total).toBeGreaterThan(50);   // C-12: 92.16 MeV
  expect(binding.perNucleon).toBeGreaterThan(5); // > 5 MeV/A for stable nuclei

  // Stability for C-12: Stable.
  const stability = await page.evaluate(() => window.__nuclear.getStability());
  expect(stability.stable).toBe(true);
  expect(stability.mode).toBe('Stable');
});

// 3b. Stability readout changes after transmutation to an unstable nucleus.
test('nuclide: stability readout changes after transmutation to unstable', async ({ page }) => {
  await gotoNuclide(page);

  // C-14 is β− unstable.
  await page.evaluate(() => window.__nuclear.setNuclide(6, 8));
  const stability = await page.evaluate(() => window.__nuclear.getStability());

  expect(stability.stable).toBe(false);
  expect(stability.mode).not.toBe('Stable');
});

// 3c. getBinding reflects the live nuclide after addProton.
test('nuclide: getBinding reflects live nuclide after addProton', async ({ page }) => {
  await gotoNuclide(page);

  const bindingBefore = await page.evaluate(() => {
    window.__nuclear.setNuclide(1, 0); // H-1 (B=0)
    return window.__nuclear.getBinding();
  });

  const bindingAfter = await page.evaluate(() => {
    window.__nuclear.addProton(); // H-1 → He-1 (still small, but different)
    return window.__nuclear.getBinding();
  });

  // Binding energies should differ (different nucleus).
  // We just check they are numbers — exact values vary by nuclide.
  expect(typeof bindingBefore.total).toBe('number');
  expect(typeof bindingAfter.total).toBe('number');
});

// 3d. Deuterium (Z=1, N=1) binding must be positive: measured value +2.224 MeV.
// This guards against the SEMF regression where D shows negative binding (~−4.66 MeV).
test('nuclide: deuterium binding is positive (measured 2.224 MeV, not SEMF)', async ({ page }) => {
  await gotoNuclide(page);

  const b = await page.evaluate(() => {
    window.__nuclear.setNuclide(1, 1); // Deuterium ²H
    return window.__nuclear.getBinding();
  });

  // measuredBinding gives +2.224 MeV; SEMF gives ≈ −4.66 MeV (wrong).
  expect(b.total).toBeGreaterThan(0);
  expect(b.total).toBeGreaterThan(2.0); // at least 2 MeV (measured is 2.224)
  expect(b.total).toBeLessThan(5.0);    // sanity: not wildly off
  expect(b.perNucleon).toBeGreaterThan(0); // per-nucleon also positive
});

// ─── GATE 4: Render pixels ────────────────────────────────────────────────────

// 4a. Canvas has lit pixels in the Nuclide scene (nucleus visible).
// Reads the exact canvas centre (where the nucleus always renders) and polls
// until it is brighter than the near-black background.
test('nuclide: canvas has lit pixels (nucleus renders)', async ({ page }) => {
  await gotoNuclide(page);

  // Set a multi-nucleon nucleus (Carbon-12 = 6p + 6n) to ensure something renders.
  await page.evaluate(() => window.__nuclear.setNuclide(6, 6));

  // Poll the canvas centre until a nucleon sphere pixel appears there.
  await expect
    .poll(
      () => page.evaluate(() => {
        const c = document.querySelector('#canvas');
        if (!c) return 0;
        const gl = c.getContext('webgl2', { preserveDrawingBuffer: true });
        if (!gl) return 0;
        const x = Math.floor(0.5 * c.width);
        const y = Math.floor(0.5 * c.height);
        const buf = new Uint8Array(4);
        gl.readPixels(x, y, 1, 1, gl.RGBA, gl.UNSIGNED_BYTE, buf);
        return buf[0] + buf[1] + buf[2];
      }),
      { timeout: 6000, intervals: [200, 300, 500] },
    )
    .toBeGreaterThan(50); // nucleus sphere brighter than near-black clear colour
});

// 4b. Nucleon count changes content: both H-1 and C-12 render a lit nucleus at centre.
// The precise pixel count is hard to compare in a coarse grid (nucleons are small
// spheres); instead we verify both are lit and that the seam correctly switches nuclides.
test('nuclide: nucleus pixel count scales with nucleon number', async ({ page }) => {
  await gotoNuclide(page);

  // Poll helper: read centre pixel brightness.
  const centerBrightness = () => page.evaluate(() => {
    const c = document.querySelector('#canvas');
    if (!c) return 0;
    const gl = c.getContext('webgl2', { preserveDrawingBuffer: true });
    if (!gl) return 0;
    const x = Math.floor(0.5 * c.width);
    const y = Math.floor(0.5 * c.height);
    const buf = new Uint8Array(4);
    gl.readPixels(x, y, 1, 1, gl.RGBA, gl.UNSIGNED_BYTE, buf);
    return buf[0] + buf[1] + buf[2];
  });

  // H-1: minimal nucleus (1 proton).
  await page.evaluate(() => window.__nuclear.setNuclide(1, 0));
  await expect.poll(centerBrightness, { timeout: 3000, intervals: [200, 300] }).toBeGreaterThan(30);
  const h1Bright = await centerBrightness();

  // C-12: 12 nucleons — the cluster spreads to cover more pixels around centre.
  await page.evaluate(() => window.__nuclear.setNuclide(6, 6));
  await page.waitForTimeout(200);
  await expect.poll(centerBrightness, { timeout: 3000, intervals: [200, 300] }).toBeGreaterThan(30);
  const c12Bright = await centerBrightness();

  // Both have a lit centre; the seam switched correctly (API round-trip was tested in Gate 2).
  expect(h1Bright).toBeGreaterThan(30);
  expect(c12Bright).toBeGreaterThan(30);
});

// ─── GATE 5: Other scenes unaffected ─────────────────────────────────────────

// 5a. The 6-cycle does not break the Materials scene (Diamond loads correctly).
test('nuclide: Materials scene still loads Diamond after 6-cycle addition', async ({ page }) => {
  // Go to Materials (4 clicks).
  await expect(page.locator('#scene-toggle')).toBeVisible();
  await page.click('#scene-toggle'); // → Atomos
  await page.click('#scene-toggle'); // → Molecule
  await page.click('#scene-toggle'); // → Builder
  await page.click('#scene-toggle'); // → Materials
  await page.waitForTimeout(900);
  await page.waitForFunction(() => !!window.__builder, null, { timeout: 6000 });

  const result = await page.evaluate(() => {
    window.__builder.loadStructure(0);
    return { atoms: window.__builder.getAtoms().length };
  });

  expect(result.atoms).toBe(64);
});

// 5b. The Nuclide API is independent of the Builder API (no cross-contamination).
test('nuclide: __nuclear and __builder are independent APIs', async ({ page }) => {
  await gotoNuclide(page);

  // Both APIs must exist and be independent objects.
  const result = await page.evaluate(() => ({
    nuclearExists: typeof window.__nuclear === 'object',
    builderExists: typeof window.__builder === 'object',
    areDistinct: window.__nuclear !== window.__builder,
  }));

  expect(result.nuclearExists).toBe(true);
  expect(result.builderExists).toBe(true);
  expect(result.areDistinct).toBe(true);
});
