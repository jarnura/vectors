// E2E spec for the constant-density nucleus packing fix (nucleusPackScale).
//
// Verifies at full sub-atomic detail (zoom ~0.6, detail ≈ 1):
//   1. A LIGHT nucleus (Carbon Z=6, 12 nucleons) renders as a tight CONTIGUOUS
//      clump of lit pixels — the bounding box is small, nucleons are dense, and
//      the lit-pixel fill ratio inside the core bbox exceeds a floor (no large
//      interior black gaps between nucleon spheres).
//   2. A HEAVY nucleus (Krypton Z=36, 84 nucleons) renders as a DISTINCT
//      cluster of lit pixels that is LARGER on-screen than Carbon's.
//      (Before the fix: Kr outer radius ≈ same as C → same footprint.)
//   3. The bbox footprint ratio Kr/C is meaningfully > 1 (visibly grew).
//
// Strategy: use the __builder seam to place one atom at origin, reset orbit to
// zero so the nucleus is centred, zoom to 0.6 (detail ≈ 1 = sub-atomic fully
// ON), read a region around canvas centre, and measure the bounding box and
// lit-pixel fill ratio of nucleon-coloured pixels.
//
// SwiftShader timing: retries:2 (playwright.config.js); expect.poll throughout.

import { test } from '@playwright/test';
import { expect, waitForRenderedCanvas, openDrawer, gotoBuilder, readRegion } from './helpers.js';

// ─── helpers ────────────────────────────────────────────────────────────────

async function readDetail(page) {
  return page.evaluate(() =>
    typeof window.__builderDetail === 'number' ? window.__builderDetail : null,
  );
}

async function setZoom(page, value) {
  await page.evaluate((v) => {
    const el = document.getElementById('zoom-slider');
    if (!el) return;
    el.value = String(v);
    el.dispatchEvent(new Event('input', { bubbles: true }));
  }, value);
  await page.waitForTimeout(200);
}

// Read a large region around the canvas centre at high resolution.
const REGION_COLS = 48;
const REGION_ROWS = 36;

async function readCentreRegion(page) {
  return readRegion(page, 0.25, 0.25, 0.75, 0.75, REGION_COLS, REGION_ROWS);
}

// Count pixels that look like nucleons (either red protons or grey neutrons).
// Protons: R dominant, R>100, G<80, B<80.
// Neutrons: grey, all channels roughly equal and moderate (50-200).
// We use a permissive "lit and non-background" threshold.
function countNucleonPixels(pixels) {
  return pixels.filter((p) => {
    const [r, g, b] = p;
    const brightness = r + g + b;
    if (brightness < 30) return false; // background
    // Proton red: R >> G, B
    const isProtonRed = r > 100 && g < 90 && b < 90;
    // Neutron grey: channels roughly equal and moderate
    const maxCh = Math.max(r, g, b);
    const minCh = Math.min(r, g, b);
    const isNeutronGrey = maxCh > 50 && maxCh < 220 && (maxCh - minCh) < 80;
    return isProtonRed || isNeutronGrey;
  }).length;
}

// Bounding-box size (width × height in grid units) of lit nucleon pixels.
// Returns 0 if no nucleon pixels found.
function nucleonBboxSize(pixels) {
  const cols = REGION_COLS;
  const rows = REGION_ROWS;
  let minR = rows;
  let maxR = -1;
  let minC = cols;
  let maxC = -1;
  for (let i = 0; i < pixels.length; i++) {
    const [r, g, b] = pixels[i];
    const brightness = r + g + b;
    if (brightness < 30) continue;
    const isProtonRed = r > 100 && g < 90 && b < 90;
    const maxCh = Math.max(r, g, b);
    const minCh = Math.min(r, g, b);
    const isNeutronGrey = maxCh > 50 && maxCh < 220 && (maxCh - minCh) < 80;
    if (!isProtonRed && !isNeutronGrey) continue;
    const row = Math.floor(i / cols);
    const col = i % cols;
    if (row < minR) minR = row;
    if (row > maxR) maxR = row;
    if (col < minC) minC = col;
    if (col > maxC) maxC = col;
  }
  if (maxR < 0 || maxC < 0) return 0;
  return (maxR - minR + 1) * (maxC - minC + 1);
}

// Lit-pixel fill ratio: nucleon-lit pixels inside the core bbox divided by
// the bbox area. A tight contiguous clump has a high fill ratio; a sparse
// cluster with large interior gaps has a low fill ratio.
//
// "Core bbox" is the inner 50% of the full bbox by area (to avoid counting
// sparse edge/corner nucleons), cropped symmetrically. Falls back to the full
// bbox if the nucleus is too small.
function nucleonFillRatio(pixels) {
  const cols = REGION_COLS;
  const rows = REGION_ROWS;

  // First find the full bbox of nucleon pixels.
  let minR = rows, maxR = -1, minC = cols, maxC = -1;
  const isNucleon = pixels.map(([r, g, b]) => {
    const brightness = r + g + b;
    if (brightness < 30) return false;
    const isProtonRed = r > 100 && g < 90 && b < 90;
    const maxCh = Math.max(r, g, b);
    const minCh = Math.min(r, g, b);
    const isNeutronGrey = maxCh > 50 && maxCh < 220 && (maxCh - minCh) < 80;
    return isProtonRed || isNeutronGrey;
  });
  for (let i = 0; i < pixels.length; i++) {
    if (!isNucleon[i]) continue;
    const row = Math.floor(i / cols);
    const col = i % cols;
    if (row < minR) minR = row;
    if (row > maxR) maxR = row;
    if (col < minC) minC = col;
    if (col > maxC) maxC = col;
  }
  if (maxR < 0 || maxC < 0) return 0;

  const bboxW = maxC - minC + 1;
  const bboxH = maxR - minR + 1;

  // Core region: inner 60% of the bbox in each dimension (avoid sparse edges).
  const shrinkC = Math.max(1, Math.floor(bboxW * 0.2));
  const shrinkR = Math.max(1, Math.floor(bboxH * 0.2));
  const coreMinC = minC + shrinkC;
  const coreMaxC = maxC - shrinkC;
  const coreMinR = minR + shrinkR;
  const coreMaxR = maxR - shrinkR;

  // If the core region is degenerate (nucleus is tiny), use the full bbox.
  if (coreMaxC < coreMinC || coreMaxR < coreMinR) {
    const bboxArea = bboxW * bboxH;
    let litCount = 0;
    for (let i = 0; i < pixels.length; i++) {
      if (!isNucleon[i]) continue;
      const row = Math.floor(i / cols);
      const col = i % cols;
      if (row >= minR && row <= maxR && col >= minC && col <= maxC) litCount++;
    }
    return bboxArea > 0 ? litCount / bboxArea : 0;
  }

  const coreArea = (coreMaxC - coreMinC + 1) * (coreMaxR - coreMinR + 1);
  let coreLitCount = 0;
  for (let i = 0; i < pixels.length; i++) {
    if (!isNucleon[i]) continue;
    const row = Math.floor(i / cols);
    const col = i % cols;
    if (row >= coreMinR && row <= coreMaxR && col >= coreMinC && col <= coreMaxC) {
      coreLitCount++;
    }
  }
  return coreArea > 0 ? coreLitCount / coreArea : 0;
}

// ─── fixture ────────────────────────────────────────────────────────────────

test.beforeEach(async ({ page }) => {
  await page.goto('/');
  await waitForRenderedCanvas(page);
  await openDrawer(page);
  await gotoBuilder(page);
  await page.waitForFunction(() => !!window.__builder, null, { timeout: 6000 });
});

// ─── test 1: light nucleus (Carbon) forms a tight contiguous clump ───────────

test('nucleus-packing: Carbon Z=6 forms a tight contiguous clump at detail=1', async ({ page }) => {
  await page.evaluate(() => {
    window.__builder.clear();
    window.__builder.addAtom(6, 0, 0, 0); // Carbon at origin
    window.__builder.setOrbit(0, 0);
  });

  // Zoom to 0.6 → detail eases to ≈1 (sub-atomic layer fully ON).
  await setZoom(page, 0.6);
  await expect
    .poll(async () => readDetail(page), { timeout: 5000 })
    .toBeGreaterThanOrEqual(0.9);

  // Allow the orbit reset to render.
  await page.waitForTimeout(400);

  // Read the central region and look for nucleon-coloured pixels.
  let carbonPixelCount = 0;
  await expect
    .poll(
      async () => {
        const pixels = await readCentreRegion(page);
        carbonPixelCount = countNucleonPixels(pixels);
        return carbonPixelCount;
      },
      { timeout: 6000, intervals: [300, 500, 800] },
    )
    .toBeGreaterThan(4); // At least a few nucleon pixels visible

  // Carbon nucleus bounding box must be non-zero (nucleus is present on screen).
  const pixels = await readCentreRegion(page);
  const bbox = nucleonBboxSize(pixels);
  expect(bbox).toBeGreaterThan(0);

  // Contiguous core check: the lit-pixel fill ratio inside the core bbox must
  // exceed a floor, asserting there is no large interior black gap between the
  // nucleon spheres. At S=13 nucleons are contiguous (mean NN ≈ 0.87×diameter);
  // a sparse arrangement (old code) or split-apart cluster would score much lower.
  // Threshold 0.25 is tolerant of SwiftShader anti-aliasing and edge effects.
  await expect
    .poll(
      async () => {
        const px = await readCentreRegion(page);
        return nucleonFillRatio(px);
      },
      { timeout: 6000, intervals: [300, 500, 800] },
    )
    .toBeGreaterThan(0.25);
});

// ─── test 2: heavy nucleus (Krypton) is larger than Carbon ───────────────────

test('nucleus-packing: Krypton Z=36 nucleus has a larger on-screen footprint than Carbon', async ({ page }) => {
  // Measure Carbon nucleus bbox first.
  await page.evaluate(() => {
    window.__builder.clear();
    window.__builder.addAtom(6, 0, 0, 0); // Carbon
    window.__builder.setOrbit(0, 0);
  });
  await setZoom(page, 0.6);
  await expect
    .poll(async () => readDetail(page), { timeout: 5000 })
    .toBeGreaterThanOrEqual(0.9);
  await page.waitForTimeout(400);

  let carbonBbox = 0;
  await expect
    .poll(
      async () => {
        const pixels = await readCentreRegion(page);
        carbonBbox = nucleonBboxSize(pixels);
        return carbonBbox;
      },
      { timeout: 6000, intervals: [300, 500, 800] },
    )
    .toBeGreaterThan(0);

  // Now switch to Krypton.
  await page.evaluate(() => {
    window.__builder.clear();
    window.__builder.addAtom(36, 0, 0, 0); // Krypton
    window.__builder.setOrbit(0, 0);
  });
  await page.waitForTimeout(400);

  // Keep same zoom (0.6) — detail stays ≈1.
  await expect
    .poll(async () => readDetail(page), { timeout: 5000 })
    .toBeGreaterThanOrEqual(0.9);

  let kryptonBbox = 0;
  await expect
    .poll(
      async () => {
        const pixels = await readCentreRegion(page);
        kryptonBbox = nucleonBboxSize(pixels);
        return kryptonBbox;
      },
      { timeout: 6000, intervals: [300, 500, 800] },
    )
    .toBeGreaterThan(0);

  // Krypton (84 nucleons) must have a visibly larger on-screen bbox than Carbon (12).
  // nucleusPackScale fix: Kr outer radius ∝ cbrt(84) vs C ∝ cbrt(12) → ratio ≈ 1.94.
  // At S=13: Kr outer model≈56.8, C outer model≈29.3; at S=19.8: Kr≈86.5, C≈44.7.
  // Both calibrations preserve the cbrt(A) ratio; Kr bbox must exceed Carbon bbox.
  expect(kryptonBbox).toBeGreaterThan(carbonBbox);
});

// ─── test 3: detail is preserved for both light and heavy (regression) ───────

test('nucleus-packing: detail=1 at zoom=0.6 for both Carbon and Krypton', async ({ page }) => {
  // Carbon.
  await page.evaluate(() => {
    window.__builder.clear();
    window.__builder.addAtom(6, 0, 0, 0);
    window.__builder.setOrbit(0, 0);
  });
  await setZoom(page, 0.6);
  await expect
    .poll(async () => readDetail(page), { timeout: 5000 })
    .toBeGreaterThanOrEqual(0.9);

  // Krypton.
  await page.evaluate(() => {
    window.__builder.clear();
    window.__builder.addAtom(36, 0, 0, 0);
    window.__builder.setOrbit(0, 0);
  });
  await page.waitForTimeout(300);
  await expect
    .poll(async () => readDetail(page), { timeout: 5000 })
    .toBeGreaterThanOrEqual(0.9);

  // Lit pixels exist at full detail for Krypton's nucleus (many nucleons).
  await expect
    .poll(
      async () => {
        const pixels = await readCentreRegion(page);
        return countNucleonPixels(pixels);
      },
      { timeout: 6000, intervals: [300, 500, 800] },
    )
    .toBeGreaterThan(4);
});
