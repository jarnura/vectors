// builder drag-strength M2: dragging a SINGLE atom now pulls against its bonds
// with a tunable drag strength (the #drag-strength slider, seam-exposed as
// window.__builder.moveAtomWith(strength, id, x, y, z)). A bond whose tabulated
// energy (Chem.bondEnergy) is >= the drag strength HOLDS — the partner atom is
// tugged along the bond axis so the pair stays within breakThreshold (230) —
// while a weaker bond SNAPS. So at strength 0 EVERY bond holds (nothing can be
// ripped apart), at the slider default 3 weak bonds (O-O, 1.46) snap but strong
// ones (O-H, 4.63) hold, and at strength 10 every tabulated bond snaps. The
// legacy moveAtom stays moveAtomWith(1e18) — always snaps an overstretched bond.
//
// All scenarios drive the deterministic window.__builder test seam
// (addAtom/moveAtom/moveAtomWith/getAtoms/getBonds/clear) — the seam shares the
// renderer's Ref, so reads reflect mutations immediately, and no canvas pixels
// are needed (SwiftShader-safe).
import { test } from '@playwright/test';
import { expect, waitForRenderedCanvas, openDrawer } from './helpers.js';

// breakThreshold (230) plus a small tolerance, so a partner tugged exactly to
// the hold distance passes the "stayed within range" assertion.
const HOLD_RANGE = 230.1;

// Euclidean distance between two {x,y,z} positions from getAtoms().
function dist(p, q) {
  const dx = p.x - q.x;
  const dy = p.y - q.y;
  const dz = p.z - q.z;
  return Math.sqrt(dx * dx + dy * dy + dz * dz);
}

// Reach the Builder scene (CubePoc → Atomos → Molecule → Builder = three
// #scene-toggle clicks) and wait for window.__builder. Mirrors
// builder-no-overlap.spec.js / world.spec.js gotoBuilder.
async function gotoBuilder(page) {
  await expect(page.locator('#scene-toggle')).toBeVisible();
  await page.click('#scene-toggle'); // → atomos
  await page.click('#scene-toggle'); // → molecule
  await page.click('#scene-toggle'); // → builder
  await page.waitForTimeout(700); // generous: let the builder scene boot + render
  await page.waitForFunction(() => !!window.__builder, null, { timeout: 6000 });
}

test.beforeEach(async ({ page }) => {
  await page.goto('/');
  await waitForRenderedCanvas(page);
  // Controls (scene-toggle) live in a left drawer that no longer auto-opens on
  // boot — open it up front so #scene-toggle is reachable.
  await openDrawer(page);
});

// (a) WEAK SNAPS: an O-O bond (energy 1.46) dragged far apart at the default
// strength 3 snaps — 1.46 < 3, so pullBonds does NOT hold it and the far drag
// overstretches it past breakThreshold.
test('builder: drag strength 3 snaps the weak O-O bond on a far drag', async ({ page }) => {
  await gotoBuilder(page);

  const result = await page.evaluate(() => {
    const b = window.__builder;
    b.clear();
    b.addAtom(8, 0, 0, 0); // O (first)
    b.addAtom(8, 150, 0, 0); // O — 150 apart: under bondThreshold → bonds
    const bondsBefore = b.getBonds().length;
    const firstOId = b.getAtoms()[0].id;
    b.moveAtomWith(3, firstOId, 600, 0, 0); // far drag at default strength
    return { bondsBefore, bondsAfter: b.getBonds().length };
  });

  expect(result.bondsBefore).toBe(1);
  expect(result.bondsAfter).toBe(0);
});

// (b) STRONG HOLDS: an O-H bond (energy 4.63 >= 3) dragged the same far
// distance at strength 3 HOLDS — the H partner is tugged along the bond axis
// (its position changes) and the pair stays within breakThreshold.
test('builder: drag strength 3 holds the strong O-H bond and tugs the H along', async ({ page }) => {
  await gotoBuilder(page);

  const result = await page.evaluate(() => {
    const b = window.__builder;
    b.clear();
    b.addAtom(8, 0, 0, 0); // O (first)
    b.addAtom(1, 150, 0, 0); // H — 150 apart: bonds
    const bondsBefore = b.getBonds().length;
    const atoms = b.getAtoms();
    const oId = atoms[0].id;
    const hId = atoms[1].id;
    const hBefore = atoms[1].pos;
    b.moveAtomWith(3, oId, 600, 0, 0); // far drag at default strength
    const after = b.getAtoms();
    const oAfter = after.find((a) => a.id === oId).pos;
    const hAfter = after.find((a) => a.id === hId).pos;
    return { bondsBefore, bondsAfter: b.getBonds().length, hBefore, hAfter, oAfter };
  });

  expect(result.bondsBefore).toBe(1);
  // The strong bond SURVIVES the far drag.
  expect(result.bondsAfter).toBe(1);
  // The H partner was tugged along (its position changed).
  expect(dist(result.hBefore, result.hAfter)).toBeGreaterThan(1);
  // ... and the pair stayed within bond-holding range (breakThreshold).
  expect(dist(result.oAfter, result.hAfter)).toBeLessThanOrEqual(HOLD_RANGE);
});

// (c) OVERPOWERED: the SAME O-H build dragged at strength 10 snaps — no
// tabulated bond energy reaches 10, so a max-strength drag rips through even
// the strong O-H bond.
test('builder: drag strength 10 rips through the strong O-H bond', async ({ page }) => {
  await gotoBuilder(page);

  const result = await page.evaluate(() => {
    const b = window.__builder;
    b.clear();
    b.addAtom(8, 0, 0, 0); // O (first)
    b.addAtom(1, 150, 0, 0); // H — 150 apart: bonds
    const bondsBefore = b.getBonds().length;
    const oId = b.getAtoms()[0].id;
    b.moveAtomWith(10, oId, 600, 0, 0); // max-strength far drag
    return { bondsBefore, bondsAfter: b.getBonds().length };
  });

  expect(result.bondsBefore).toBe(1);
  expect(result.bondsAfter).toBe(0);
});

// (d) ZERO STRENGTH: even the WEAKEST bond (O-O, 1.46) holds a zero-strength
// drag — every bond energy is >= 0, so slider 0 means NOTHING ever breaks; the
// partner O follows the dragged one and the bond survives.
test('builder: drag strength 0 never breaks — the O-O partner follows the drag', async ({ page }) => {
  await gotoBuilder(page);

  const result = await page.evaluate(() => {
    const b = window.__builder;
    b.clear();
    b.addAtom(8, 0, 0, 0); // O (first)
    b.addAtom(8, 150, 0, 0); // O — 150 apart: bonds
    const bondsBefore = b.getBonds().length;
    const atoms = b.getAtoms();
    const draggedId = atoms[0].id;
    const partnerId = atoms[1].id;
    const partnerBefore = atoms[1].pos;
    b.moveAtomWith(0, draggedId, 600, 0, 0); // zero-strength far drag
    const after = b.getAtoms();
    const draggedAfter = after.find((a) => a.id === draggedId).pos;
    const partnerAfter = after.find((a) => a.id === partnerId).pos;
    return {
      bondsBefore,
      bondsAfter: b.getBonds().length,
      partnerBefore,
      partnerAfter,
      draggedAfter,
    };
  });

  expect(result.bondsBefore).toBe(1);
  // The weakest bond SURVIVES a zero-strength drag (slider 0 = nothing breaks).
  expect(result.bondsAfter).toBe(1);
  // The partner was pulled along...
  expect(dist(result.partnerBefore, result.partnerAfter)).toBeGreaterThan(1);
  // ... staying within bond-holding range of the dragged atom.
  expect(dist(result.draggedAfter, result.partnerAfter)).toBeLessThanOrEqual(HOLD_RANGE);
});

// (e) LEGACY: plain moveAtom is moveAtomWith(1e18) — stronger than ANY bond —
// so a far drag still snaps even the strong O-H bond (pre-slider behaviour is
// byte-compatible).
test('builder: legacy moveAtom still snaps the O-H bond on a far drag', async ({ page }) => {
  await gotoBuilder(page);

  const result = await page.evaluate(() => {
    const b = window.__builder;
    b.clear();
    b.addAtom(8, 0, 0, 0); // O (first)
    b.addAtom(1, 150, 0, 0); // H — 150 apart: bonds
    const bondsBefore = b.getBonds().length;
    const oId = b.getAtoms()[0].id;
    b.moveAtom(oId, 600, 0, 0); // legacy far drag (no strength arg)
    return { bondsBefore, bondsAfter: b.getBonds().length };
  });

  expect(result.bondsBefore).toBe(1);
  expect(result.bondsAfter).toBe(0);
});

// (f) SLIDER MARKUP: the #drag-strength range slider exists in the controls
// drawer with the pinned range (0..10, step 0.1) and the default value 3.
test('builder: the #drag-strength slider exists with range 0..10 step 0.1 default 3', async ({ page }) => {
  const slider = page.locator('#drag-strength');
  await expect(slider).toHaveCount(1);
  await expect(slider).toHaveAttribute('type', 'range');
  await expect(slider).toHaveAttribute('min', '0');
  await expect(slider).toHaveAttribute('max', '10');
  await expect(slider).toHaveAttribute('step', '0.1');
  await expect(slider).toHaveAttribute('value', '3');
});
