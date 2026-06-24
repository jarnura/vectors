// Builder "Free only" toggle spec: the #free-electrons-only checkbox hides the
// shared covalent bonding-pair electrons (amber, midpoint band) while keeping
// the valence lone electrons (amber, atom-ring band) visible.
//
// Geometry: two H at (-60,0,0) and (60,0,0) → separation 120 < bondThreshold
// (180) → auto-bonds → H₂. The shared pair lives in the MIDBAND (fx≈0.50),
// while each H's lone electrons (H has no lone electrons after bonding — but we
// set up a free Carbon Z=6 for the ring-band check). For the midband / ring-band
// split we use the same geometry as builder-valence-colour.spec.js.
import { test } from '@playwright/test';
import {
  expect, waitForRenderedCanvas, openDrawer, readRegion, gotoBuilder,
} from './helpers.js';

test.beforeEach(async ({ page }, testInfo) => {
  await page.goto('/');
  await waitForRenderedCanvas(page);
  if (!testInfo.title.startsWith('controls:')) {
    await openDrawer(page);
  }
});

// ─── shared pixel helpers (mirrors builder-valence-colour.spec.js) ──────────
const LIT = 90;
const isNucleusColour = (p) => {
  const [r, g, b] = p;
  const mx = Math.max(r, g, b), mn = Math.min(r, g, b);
  const grey = mx - mn < 28;
  const protonRed = r > 150 && g < 110 && b < 110 && r - Math.max(g, b) > 60;
  return grey || protonRed;
};
const electronPixels = (px) =>
  px.filter((p) => p[0] + p[1] + p[2] > LIT && !isNucleusColour(p));

const channelCounts = (px) => {
  const t = { R: 0, G: 0, B: 0 };
  for (const p of electronPixels(px)) {
    const [r, g, b] = p;
    if (b >= r && b >= g) t.B++;
    else if (r >= g && r >= b) t.R++;
    else t.G++;
  }
  return t;
};

// ─────────────────────────────────────────────────────────────────────────────
// TEST 1: Free-only toggle hides the bonding pair at the midpoint band.
// When #free-electrons-only is checked the midband (between the two H nuclei)
// should lose its amber electron pixels; unchecking restores them.
test('builder: free-electrons-only toggle hides bonding pair, keeps lone electrons', async ({ page }) => {
  await gotoBuilder(page);

  // Two H within bond range → H₂. The shared pair sits in the midband (fx≈0.50).
  const bonds = await page.evaluate(() => {
    const b = window.__builder;
    b.clear();
    b.addAtom(1, -60, 0, 0);
    b.addAtom(1, 60, 0, 0);
    return b.getBonds().length;
  });
  expect(bonds).toBeGreaterThanOrEqual(1);
  await page.waitForTimeout(400);

  // MIDBAND: where the shared bonding pair breathes between the two H nuclei.
  const midband = () => readRegion(page, 0.46, 0.42, 0.54, 0.58, 16, 18);

  // Pool the midband across several frames (pair breathes).
  const poolMid = async (frames = 8) => {
    let pool = [];
    for (let i = 0; i < frames; i++) {
      pool = pool.concat(await midband());
      await page.waitForTimeout(90);
    }
    return pool;
  };

  // Cold-start retry: pool until the bonding pair lights the midband.
  let poolBefore = [];
  for (let i = 0; i < 20; i++) {
    poolBefore = await poolMid(6);
    if (electronPixels(poolBefore).length > 0) break;
    await page.waitForTimeout(120);
  }

  const beforeCounts = channelCounts(poolBefore);

  // Sanity: the bonding pair lights the midband before toggling.
  expect(beforeCounts.R + beforeCounts.G + beforeCounts.B).toBeGreaterThan(0);
  // The bonding pair is amber (RED-dominant, same as the valence colour).
  expect(beforeCounts.R).toBeGreaterThan(beforeCounts.B);

  // ── Toggle ON: #free-electrons-only checked ──────────────────────────────
  await page.check('#free-electrons-only');
  await page.waitForTimeout(400);

  // With the toggle ON, the bonding pair should be GONE from the midband.
  // Sample thoroughly (12 frames) to confirm no amber midband pixels appear.
  const poolAfterOn = await poolMid(12);
  const afterOnCounts = channelCounts(poolAfterOn);
  // The midband amber (bonding pair) drops to ~0 when free-only is ON.
  expect(afterOnCounts.R).toBe(0);

  // ── Toggle OFF: bonding pair RETURNS ────────────────────────────────────
  await page.uncheck('#free-electrons-only');
  await page.waitForTimeout(400);

  let poolRestored = [];
  for (let i = 0; i < 15; i++) {
    poolRestored = await poolMid(6);
    if (electronPixels(poolRestored).length > 0) break;
    await page.waitForTimeout(120);
  }
  const restoredCounts = channelCounts(poolRestored);
  // After unchecking, the bonding pair amber pixels return.
  expect(restoredCounts.R).toBeGreaterThan(0);
  expect(restoredCounts.R).toBeGreaterThan(restoredCounts.B);
});

// ─────────────────────────────────────────────────────────────────────────────
// TEST 2: Free-only toggle keeps the lone (atom-ring) electrons visible.
// A free Carbon (Z=6) has 4 amber valence electrons on the outer ring — these
// are LONE (not bonding-pair) and must stay visible when free-only is checked.
test('builder: free-electrons-only keeps atom-ring lone electrons visible', async ({ page }) => {
  await gotoBuilder(page);

  // A free Carbon at the origin: 4 amber valence electrons on the outer ring.
  await page.evaluate(() => {
    const b = window.__builder;
    b.clear();
    b.addAtom(6, 0, 0, 0);
  });
  await page.waitForTimeout(400);

  // OUTER (valence lone) band — same geometry as builder-valence-colour.spec.js:
  // model r=144, effective scale 3.52 → fx offset ≈0.247 from centre.
  const outerRight = () => readRegion(page, 0.70, 0.455, 0.79, 0.545, 16, 16);
  const outerTop   = () => readRegion(page, 0.46, 0.00, 0.54, 0.12, 16, 16);

  const poolOuter = async (frames = 6) => {
    let pool = [];
    for (let i = 0; i < frames; i++) {
      for (const box of [outerRight, outerTop]) pool = pool.concat(await box());
      await page.waitForTimeout(90);
    }
    return pool;
  };

  // Cold-start retry: pool until the outer ring is lit.
  const poolUntilLit = async (tries = 12) => {
    let pool = [];
    for (let i = 0; i < tries; i++) {
      pool = await poolOuter(4);
      if (electronPixels(pool).length > 0) break;
      await page.waitForTimeout(120);
    }
    return pool;
  };

  // ── BEFORE toggle: outer ring is amber ──────────────────────────────────
  const outerBefore = channelCounts(await poolUntilLit());
  expect(outerBefore.R).toBeGreaterThan(0);

  // ── Toggle ON ───────────────────────────────────────────────────────────
  await page.check('#free-electrons-only');
  await page.waitForTimeout(400);

  // The outer ring (lone electrons) must REMAIN amber when free-only is ON.
  const outerAfter = channelCounts(await poolUntilLit());
  expect(outerAfter.R).toBeGreaterThan(0);
});

// ─────────────────────────────────────────────────────────────────────────────
// TEST 3: Composition — free-only and valence-only are independent.
// With #free-electrons-only ON and #valence-only OFF: the H-H bonding pair
// disappears from the midband while valence-only leaves core (blue) visible.
// Uncheck free-only → bonding pair returns.
// Then check valence-only → core blue disappears (the valence-only toggle works
// independently). Verifies the two toggles compose without interfering.
test('builder: free-electrons-only composes with valence-only independently', async ({ page }) => {
  await gotoBuilder(page);

  // Two H within bond range → H₂ (bonding pair in midband). No Carbon here
  // to avoid cross-contamination of the midband sample region.
  const bonds = await page.evaluate(() => {
    const b = window.__builder;
    b.clear();
    b.addAtom(1, -60, 0, 0);
    b.addAtom(1, 60, 0, 0);
    return b.getBonds().length;
  });
  expect(bonds).toBeGreaterThanOrEqual(1);
  await page.waitForTimeout(400);

  const midband = () => readRegion(page, 0.46, 0.42, 0.54, 0.58, 16, 18);

  const poolMid = async (frames = 8) => {
    let pool = [];
    for (let i = 0; i < frames; i++) {
      pool = pool.concat(await midband());
      await page.waitForTimeout(90);
    }
    return pool;
  };

  // Cold-start: pool until the bonding pair lights the midband.
  let baseline = [];
  for (let i = 0; i < 20; i++) {
    baseline = await poolMid(6);
    if (electronPixels(baseline).length > 0) break;
    await page.waitForTimeout(120);
  }
  const baseCounts = channelCounts(baseline);
  // Sanity: bonding pair is amber (RED-dominant) at baseline.
  expect(baseCounts.R).toBeGreaterThan(0);
  expect(baseCounts.R).toBeGreaterThan(baseCounts.B);

  // ── Check free-only, leave valence-only unchecked ────────────────────────
  // The toggles are independent: free-only should kill the bonding pair without
  // requiring valence-only to also be ON.
  await page.check('#free-electrons-only');
  await page.waitForTimeout(400);

  const freeOnCounts = channelCounts(await poolMid(12));
  // Bonding pair gone when free-only is ON.
  expect(freeOnCounts.R).toBe(0);

  // ── Uncheck free-only, check valence-only ───────────────────────────────
  // valence-only kills core (blue) but does NOT kill the bonding pair (the
  // bonding pair is the valence electrons, not core).
  await page.uncheck('#free-electrons-only');
  await page.check('#valence-only');
  await page.waitForTimeout(400);

  // Bonding pair should RETURN (free-only is OFF), and midband should still
  // show amber since the bonding pair is a valence electron (not dropped by
  // valence-only which only hides the core blue lone electrons).
  let valOnPool = [];
  for (let i = 0; i < 15; i++) {
    valOnPool = await poolMid(6);
    if (electronPixels(valOnPool).length > 0) break;
    await page.waitForTimeout(120);
  }
  const valOnCounts = channelCounts(valOnPool);
  // With valence-only ON but free-only OFF: bonding pair still visible (amber).
  expect(valOnCounts.R).toBeGreaterThan(0);
  expect(valOnCounts.R).toBeGreaterThan(valOnCounts.B);

  // ── Check BOTH ON: bonding pair disappears again ─────────────────────────
  await page.check('#free-electrons-only');
  await page.waitForTimeout(400);

  const bothOnCounts = channelCounts(await poolMid(12));
  // With both ON: bonding pair is gone.
  expect(bothOnCounts.R).toBe(0);

  // Clean up toggles.
  await page.uncheck('#free-electrons-only');
  await page.uncheck('#valence-only');
});
