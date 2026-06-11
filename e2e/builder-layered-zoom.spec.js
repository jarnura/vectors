// builder-layered-zoom M2: in the Builder scene the placed atoms smoothly
// CROSS-FADE with the camera zoom. Zoomed IN (high zoom; default 1.0) the atoms
// render at full sub-atomic detail (nucleus + electrons); zoomed OUT (low zoom)
// each atom collapses to a single ball. The blend is driven by a State field
// `detail` (0 = balls … 1 = full detail) that is EASED each frame toward a
// smoothstep of the zoom, so even discrete button-zoom steps animate smoothly.
//
// The deterministic E2E hook is the debug global `window.__builderDetail`: a
// number in [0,1] updated EVERY frame with the live eased detail. We assert on it
// (reliable under SwiftShader) and back it up with a single coarse region
// pixel-delta to confirm the render actually responds.
//
// Zoom is driven by the existing on-screen #zoom-out / #zoom-in buttons, which
// live inside the #controls drawer (opened with openDrawer like the other builder
// specs). #zoom-out pulls the camera back → detail eases DOWN toward 0; #zoom-in
// pushes in → detail eases UP toward 1.
//
// RED until M2 wires State.detail, the per-frame easeDetail, the Builder render
// LOD cross-fade, and the window.__builderDetail debug global. Today
// window.__builderDetail is undefined, so every poll below times out / the
// thresholds are never reached.
import { test } from '@playwright/test';
import {
  expect, waitForRenderedCanvas, openDrawer, readRegion,
} from './helpers.js';

// Reach the Builder scene (CubePoc → Atomos → Molecule → Builder = three
// #scene-toggle clicks) and wait for window.__builder. The drawer is opened up
// front in beforeEach (like the other builder/zoom specs) so #scene-toggle and the
// #zoom-in / #zoom-out buttons are reachable. Mirrors world.spec.js gotoBuilder.
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

// Click a control N times, settling a frame or two between clicks so the per-frame
// easing can advance. Mirrors zoom-buttons.spec.js clickN.
async function clickN(page, selector, n) {
  for (let i = 0; i < n; i++) {
    await page.click(selector);
    await page.waitForTimeout(100);
  }
  await page.waitForTimeout(300);
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
  // Controls (scene-toggle + zoom buttons) live in a left drawer that no longer
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

  // Zoom OUT, sampling detail at intermediate points to prove smooth easing
  // (fractional values, non-increasing) rather than a 1→0 jump.
  const captures = [];
  for (let step = 0; step < 5; step++) {
    await clickN(page, '#zoom-out', 2); // 10 clicks total across the loop
    captures.push(await readDetail(page));
  }

  // Eventually collapses toward balls (detail ≤ 0.2).
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

  // Drive it OUT first → collapsed toward balls.
  await clickN(page, '#zoom-out', 10);
  await expect
    .poll(async () => readDetail(page), { timeout: 4000 })
    .toBeLessThanOrEqual(0.2);

  // Now zoom IN more times than the zoom-outs → detail eases back toward 1.
  await clickN(page, '#zoom-in', 12);
  await expect
    .poll(async () => readDetail(page), { timeout: 4000 })
    .toBeGreaterThanOrEqual(0.9);
});
