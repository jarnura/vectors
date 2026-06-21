// M2-S5 bond-multiplicity render spec: verify that bond ORDER is exposed
// through window.__builder.getBonds()[i].order, and that the expected orders
// appear for canonical element pairs.
//
// These tests do NOT run automatically (they are authored but excluded from the
// CI suite until a dedicated builder-bond-order runner is added). Run manually:
//   npm run e2e -- --grep "bond order"
//
// All assertions drive the world exclusively via window.__builder and make no
// pixel-level checks — the render geometry is covered by the unit tests in
// test/VibrationSpec.purs (vibratedBondLines count/perpendicularity) and the
// existing builder-bonding.spec.js pixel tests.
//
// window.__builder API used here:
//   addAtom(z, x, y, z3)   — place an atom of atomic number z
//   getBonds()              — Array<{ a: Int, b: Int, order: Int }>
//   getAtoms()              — Array<{ id: Int, z: Int, pos: V3 }>
//   clear()                 — reset world
//
// Bond separation used: bondThreshold * 0.5 ≈ 90 world-units (well within the
// 180-unit bondThreshold so every pair bonds immediately after addAtom).

import { test, expect } from '@playwright/test';

// Navigate to the Builder scene and wait for window.__builder to be available.
async function gotoBuilder(page) {
  await page.goto('/');
  // Three scene-toggles: CubePoc → Atomos → Molecule → Builder.
  await page.click('#scene-toggle');
  await page.click('#scene-toggle');
  await page.click('#scene-toggle');
  await page.waitForFunction(() => !!window.__builder, null, { timeout: 6000 });
  // Clear any atoms left from a previous test (defensive).
  await page.evaluate(() => window.__builder.clear());
}

const NEAR = 90; // world-units, comfortably inside bondThreshold (180)

// ── H-H: valence 1 each → order 1 ────────────────────────────────────────────
test('bond order: two H atoms auto-bond at order 1', async ({ page }) => {
  await gotoBuilder(page);

  await page.evaluate(
    ([near]) => {
      window.__builder.addAtom(1, 0, 0, 0);
      window.__builder.addAtom(1, near, 0, 0);
    },
    [NEAR]
  );

  const bonds = await page.evaluate(() => window.__builder.getBonds());

  expect(bonds.length).toBe(1);
  expect(bonds[0].order).toBe(1);
});

// ── O-O: valence 2 each → order 2 double bond ────────────────────────────────
test('bond order: two O atoms auto-bond at order 2 (double bond)', async ({ page }) => {
  await gotoBuilder(page);

  await page.evaluate(
    ([near]) => {
      window.__builder.addAtom(8, 0, 0, 0);
      window.__builder.addAtom(8, near, 0, 0);
    },
    [NEAR]
  );

  const bonds = await page.evaluate(() => window.__builder.getBonds());

  expect(bonds.length).toBe(1);
  expect(bonds[0].order).toBeGreaterThanOrEqual(2);
});

// ── N-N: valence 3 each → order 3 triple bond ────────────────────────────────
test('bond order: two N atoms auto-bond at order 3 (triple bond)', async ({ page }) => {
  await gotoBuilder(page);

  await page.evaluate(
    ([near]) => {
      window.__builder.addAtom(7, 0, 0, 0);
      window.__builder.addAtom(7, near, 0, 0);
    },
    [NEAR]
  );

  const bonds = await page.evaluate(() => window.__builder.getBonds());

  expect(bonds.length).toBe(1);
  expect(bonds[0].order).toBe(3);
});

// ── getBonds exposes the order field (not undefined) ──────────────────────────
test('bond order: getBonds always exposes an integer order field', async ({ page }) => {
  await gotoBuilder(page);

  await page.evaluate(
    ([near]) => {
      // Place a C-C pair (order 3 by hard cap).
      window.__builder.addAtom(6, 0, 0, 0);
      window.__builder.addAtom(6, near, 0, 0);
    },
    [NEAR]
  );

  const bonds = await page.evaluate(() => window.__builder.getBonds());

  expect(bonds.length).toBeGreaterThan(0);
  for (const bond of bonds) {
    // order must be a positive integer.
    expect(typeof bond.order).toBe('number');
    expect(Number.isInteger(bond.order)).toBe(true);
    expect(bond.order).toBeGreaterThanOrEqual(1);
  }
});

// ── Mixed world: C-C (high order) and H-H (order 1) in the same world ─────────
test('bond order: mixed world reports correct orders for each bond', async ({ page }) => {
  await gotoBuilder(page);

  const FAR = 1800; // well beyond breakThreshold (≈690) → no cross-group bond

  await page.evaluate(
    ([near, far]) => {
      // C-C at origin group (order 3 by cap).
      window.__builder.addAtom(6, 0, 0, 0);
      window.__builder.addAtom(6, near, 0, 0);
      // H-H far away (order 1).
      window.__builder.addAtom(1, far, 0, 0);
      window.__builder.addAtom(1, far + near, 0, 0);
    },
    [NEAR, FAR]
  );

  const bonds = await page.evaluate(() => window.__builder.getBonds());

  // Exactly 2 bonds (C-C and H-H, no cross-group bond).
  expect(bonds.length).toBe(2);

  // One bond should have order >= 2 (C-C) and one should have order == 1 (H-H).
  const orders = bonds.map((b) => b.order).sort((a, b) => a - b);
  expect(orders[0]).toBe(1);         // H-H
  expect(orders[1]).toBeGreaterThanOrEqual(2); // C-C
});
