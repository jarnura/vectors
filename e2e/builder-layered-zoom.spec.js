// builder-layered-zoom M2: in the Builder scene the placed atoms smoothly
// CROSS-FADE with the camera zoom. Zoomed IN (high zoom; default 1.0) the atoms
// render at full sub-atomic detail (nucleus + electrons); zoomed OUT (low zoom)
// each atom collapses to a single ball. The blend is driven by a State field
// `detail` (0 = balls … 1 = full detail) that is EASED each frame toward a
// smoothstep of the zoom, so even discrete slider changes animate smoothly.
//
// The deterministic E2E hook is the debug global `window.__builderDetail`: a
// number in [0,1] updated EVERY frame with the live eased detail. We assert on it
// (reliable under SwiftShader) and back it up with a single coarse region
// pixel-delta to confirm the render actually responds.
//
// Zoom is driven by the #zoom-slider range input (replacing the old #zoom-in /
// #zoom-out buttons). Setting the slider to a LOW value zooms OUT → detail eases
// DOWN toward 0; a HIGH value zooms IN → detail eases UP toward 1.
import { test } from '@playwright/test';
import {
  expect, waitForRenderedCanvas, openDrawer, readRegion,
} from './helpers.js';

// Reach the Builder scene (CubePoc → Atomos → Molecule → Builder = three
// #scene-toggle clicks) and wait for window.__builder.
async function gotoBuilder(page) {
  await expect(page.locator('#scene-toggle')).toBeVisible();
  await page.click('#scene-toggle'); // → atomos
  await page.click('#scene-toggle'); // → molecule
  await page.click('#scene-toggle'); // → builder
  await page.waitForTimeout(700); // generous: let the builder scene boot + render
  await page.waitForFunction(() => !!window.__builder, null, { timeout: 6000 });
}

// Add two atoms within bonding range (mirrors the world.spec.js bond coords:
// two H 60 model units apart → within bondThreshold) so there is real
// sub-atomic structure (nucleus + electrons) to cross-fade.
async function addBondedPair(page) {
  await page.evaluate(() => {
    const b = window.__builder;
    b.clear();
    b.addAtom(1, -30, 0, 0); // H (id 0)
    b.addAtom(1, 30, 0, 0); // H (id 1) — 60 apart → within bondThreshold
  });
}

// Read the live eased detail value off the debug global (undefined until wired).
async function readDetail(page) {
  return page.evaluate(() => window.__builderDetail);
}

// Set the #zoom-slider to a given value, dispatch an 'input' event so the
// listener fires, and settle a few frames so easing can advance.
// Uses page.evaluate to bypass step-boundary restrictions (page.fill rejects
// values not on the step).
async function setZoom(page, value) {
  await page.evaluate((v) => {
    const el = document.getElementById('zoom-slider');
    if (!el) return;
    el.value = String(v);
    el.dispatchEvent(new Event('input', { bubbles: true }));
  }, value);
  await page.waitForTimeout(200);
}

// Coarse region around the atoms (centre of canvas) for a pixel-delta check.
const COLS = 36;
const ROWS = 24;
const region = (page) => readRegion(page, 0.18, 0.18, 0.82, 0.82, COLS, ROWS);
// Sum of absolute per-channel differences across a region grid (render changed?).
function regionDelta(a, b) {
  let d = 0;
  for (let i = 0; i < a.length; i++) {
    d += Math.abs(a[i][0] - b[i][0]);
    d += Math.abs(a[i][1] - b[i][1]);
    d += Math.abs(a[i][2] - b[i][2]);
  }
  return d;
}

test.beforeEach(async ({ page }) => {
  await page.goto('/');
  await waitForRenderedCanvas(page);
  // Controls (scene-toggle + zoom slider) live in a left drawer that no longer
  // auto-opens on boot — open it up front so those controls are reachable.
  await openDrawer(page);
});

// 1) At the default zoom (1.0) the eased detail settles at (near) full detail.
test('builder LOD: starts at full detail (~1) at default zoom', async ({ page }) => {
  await gotoBuilder(page);
  await addBondedPair(page);

  // Let the per-frame easing settle toward the smoothstep of zoom=1.0.
  await page.waitForTimeout(600);

  // Full sub-atomic detail at default zoom → detail eases to ~1.
  await expect
    .poll(async () => readDetail(page), { timeout: 4000 })
    .toBeGreaterThanOrEqual(0.9);
});

// 2) Zoom-out eases detail DOWN smoothly toward balls (0), passing through
// fractional values monotonically, and the render visibly responds.
test('builder LOD: zoom-out eases detail down smoothly toward balls', async ({ page }) => {
  await gotoBuilder(page);
  await addBondedPair(page);
  await page.waitForTimeout(600);

  // Start near full detail (~1).
  const start = await readDetail(page);
  expect(start).toBeGreaterThanOrEqual(0.9);

  const before = await region(page);

  // Zoom OUT in 5 steps straddling the new band [0.10, 0.20], sampling detail
  // each time to prove smooth easing rather than a 1→0 jump.
  // Steps descend through the band: 0.18 (inside), 0.15 (mid), 0.12, 0.10 (lo), 0.07 (below).
  const zoomSteps = [0.18, 0.15, 0.12, 0.10, 0.07];
  const captures = [];
  for (const z of zoomSteps) {
    await setZoom(page, z);
    captures.push(await readDetail(page));
  }

  // Eventually collapses toward balls (detail ≤ 0.2) once we drop below detailLo=0.10.
  await expect
    .poll(async () => readDetail(page), { timeout: 4000 })
    .toBeLessThanOrEqual(0.2);

  // SMOOTHNESS: at least one captured midpoint is strictly fractional in (0,1) —
  // detail passed THROUGH intermediate values, it did not jump 1→0.
  const fractional = captures.filter((v) => v > 0 && v < 1);
  expect(fractional.length).toBeGreaterThan(0);

  // MONOTONIC-ish DOWN: each capture is non-increasing within a small tolerance
  // (easing toward a falling target never climbs back up between samples).
  const tol = 0.05;
  for (let i = 1; i < captures.length; i++) {
    expect(captures[i]).toBeLessThanOrEqual(captures[i - 1] + tol);
  }

  // The render actually responded: the centre region changed measurably.
  const after = await region(page);
  expect(regionDelta(before, after)).toBeGreaterThan(50);
});

// 3) From a zoomed-out (balls) state, zoom-in eases detail back UP toward full
// sub-atomic detail.
test('builder LOD: zoom-in eases detail back up toward full detail', async ({ page }) => {
  await gotoBuilder(page);
  await addBondedPair(page);
  await page.waitForTimeout(600);

  // Drive it OUT first → collapsed toward balls (0.07 is below detailLo=0.10).
  await setZoom(page, 0.07);
  await expect
    .poll(async () => readDetail(page), { timeout: 4000 })
    .toBeLessThanOrEqual(0.2);

  // Now set slider to a high zoom → detail eases back toward 1.
  await setZoom(page, 3.0);
  await expect
    .poll(async () => readDetail(page), { timeout: 4000 })
    .toBeGreaterThanOrEqual(0.9);
});

// 4) Multi-nucleon persistence: the sub-particle layer PERSISTS at wide zoom-out
// (zoom ~0.20–0.25, above the new detailHi=0.20 floor) so the user can survey many
// nucleons densely together without flipping to the atom-ball view.
// Only dropping below detailLo=0.10 (e.g. zoom~0.07) collapses to balls.
test('builder LOD: sub-particle layer persists at wide zoom-out (Krypton, zoom 0.22)', async ({ page }) => {
  await gotoBuilder(page);

  // Krypton (Z=36) has 36 nucleons — a rich multi-nucleon nucleus to survey.
  await page.evaluate(() => {
    const b = window.__builder;
    b.clear();
    b.addAtom(36, 0, 0, 0);
  });
  await page.waitForTimeout(600);

  // Confirm full detail at default zoom (1.0) before driving the camera back.
  await expect
    .poll(async () => readDetail(page), { timeout: 4000 })
    .toBeGreaterThanOrEqual(0.9);

  // Drive to zoom 0.22 — above new detailHi (0.20), so sub-particle layer stays
  // fully ON. The eased detail must remain HIGH (≥ 0.9).
  await setZoom(page, 0.22);
  await expect
    .poll(async () => readDetail(page), { timeout: 4000 })
    .toBeGreaterThanOrEqual(0.9);

  // Confirm the render still shows lit pixels (nucleon spheres are visible,
  // not just the background black) at the wide framing.
  const pixels = await region(page);
  const litPixels = pixels.filter((p) => p[0] + p[1] + p[2] > 30);
  expect(litPixels.length).toBeGreaterThan(0);

  // Now drop to zoom 0.07 (below detailLo=0.10) — collapses to balls.
  await setZoom(page, 0.07);
  await expect
    .poll(async () => readDetail(page), { timeout: 4000 })
    .toBeLessThanOrEqual(0.2);
});
