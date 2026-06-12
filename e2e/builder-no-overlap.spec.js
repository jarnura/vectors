// builder no-overlap M2: the Pauli-exclusion minimum-separation constraint is
// wired into the Builder ops (addAtom / moveAtom / moveMolecule call
// Builder.resolveOverlaps before recomputeBonds), so no two atom centres ever
// sit closer than their contact floor (minSeparation, clamped to [130, 165] —
// always below the ~180 bondThreshold, so covalent bonding still works; the
// shared electron pair stays the one allowed overlap).
//
// All scenarios drive the deterministic window.__builder test seam
// (addAtom/moveAtom/moveMolecule/getAtoms/getBonds/getMolecules/clear) — the
// seam shares the renderer's Ref, so reads reflect mutations immediately, and
// no canvas pixels are needed (SwiftShader-safe). Anchor policy under test:
//   addAtom      → the NEW atom yields, existing atoms stay put;
//   moveAtom     → the dragged atom lands exactly at its target, others yield;
//   moveMolecule → the moved component is rigid (internal geometry preserved),
//                  external atoms yield.
//
// Floors in JS: floor(H,H) = 130 and floor(C,H) ≈ 130 (both at the clamp's
// lower bound), so we simply assert pair distances >= 129.9 (tolerance-safe)
// rather than recomputing per-pair values — EXCEPT the bonded-H₂ case, where
// we also assert < 180 to confirm the pair stays in bonding range.
import { test } from '@playwright/test';
import { expect, waitForRenderedCanvas, openDrawer } from './helpers.js';

// The contact-floor lower bound (Builder.absoluteMin = 130), minus a small
// tolerance so floating-point landings exactly ON the floor pass.
const FLOOR = 129.9;

// Euclidean distance between two {x,y,z} positions from getAtoms().
function dist(p, q) {
  const dx = p.x - q.x;
  const dy = p.y - q.y;
  const dz = p.z - q.z;
  return Math.sqrt(dx * dx + dy * dy + dz * dz);
}

// Reach the Builder scene (CubePoc → Atomos → Molecule → Builder = three
// #scene-toggle clicks) and wait for window.__builder. Mirrors
// builder-atom-symbols.spec.js / world.spec.js gotoBuilder.
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

// (a) Double-add at the SAME position: the second atom cannot stack on the
// first — the new atom yields and is pushed out to the contact floor, so the
// pair distance is >= 130 (assert >= 129.9, tolerance-safe).
test('builder: adding an atom on an occupied spot pushes the new atom to the floor', async ({ page }) => {
  await gotoBuilder(page);

  const atoms = await page.evaluate(() => {
    const b = window.__builder;
    b.clear();
    b.addAtom(8, 0, 0, 0); // O at the origin
    b.addAtom(8, 0, 0, 0); // O ON TOP of it — must be pushed off
    return b.getAtoms();
  });

  expect(atoms.length).toBe(2);
  expect(dist(atoms[0].pos, atoms[1].pos)).toBeGreaterThanOrEqual(FLOOR);
});

// (b) moveAtom ONTO an occupied position: the dragged atom is the anchor — it
// lands EXACTLY at the target — while the atom that was there is pushed out to
// the contact floor.
test('builder: moveAtom onto an occupied spot keeps the target, pushes the other atom', async ({ page }) => {
  await gotoBuilder(page);

  const result = await page.evaluate(() => {
    const b = window.__builder;
    b.clear();
    b.addAtom(6, 0, 0, 0); // C (id 0) at the origin
    b.addAtom(1, 600, 0, 0); // H (id 1) far away — no interaction
    const hId = b.getAtoms()[1].id;
    b.moveAtom(hId, 0, 0, 0); // drag the H exactly onto the C
    return { atoms: b.getAtoms(), hId };
  });

  expect(result.atoms.length).toBe(2);
  const moved = result.atoms.find((a) => a.id === result.hId);
  const pushed = result.atoms.find((a) => a.id !== result.hId);
  // The dragged H is AT the target (it is the anchor).
  expect(Math.abs(moved.pos.x)).toBeLessThan(0.1);
  expect(Math.abs(moved.pos.y)).toBeLessThan(0.1);
  expect(Math.abs(moved.pos.z)).toBeLessThan(0.1);
  // The C that occupied the spot was pushed out to the contact floor.
  expect(dist(moved.pos, pushed.pos)).toBeGreaterThanOrEqual(FLOOR);
});

// (c) moveMolecule collision: a bonded H–H pair (rigid component) dragged onto
// a lone O — every cross pair (each H vs the O) ends >= its floor, while the
// internal H–H distance is preserved exactly (rigid translation; intra-
// component pairs are both-anchor and skipped by the solver).
test('builder: moveMolecule keeps the molecule rigid and pushes external atoms', async ({ page }) => {
  await gotoBuilder(page);

  const result = await page.evaluate(() => {
    const b = window.__builder;
    b.clear();
    b.addAtom(1, 0, 0, 0); // H (id 0)
    b.addAtom(1, 150, 0, 0); // H (id 1) — 150 apart: above floor, under bondThreshold → bonds
    b.addAtom(8, 2000, 0, 0); // lone O (id 2), far away
    const before = b.getAtoms();
    const anchorId = before[0].id;
    const oId = before[2].id;
    const oPos = before[2].pos;
    const internalBefore = ((p, q) => Math.hypot(p.x - q.x, p.y - q.y, p.z - q.z))(
      before[0].pos, before[1].pos,
    );
    const bondsBefore = b.getBonds().length;
    b.moveMolecule(anchorId, oPos.x, oPos.y, oPos.z); // drag H₂ onto the O
    return { atoms: b.getAtoms(), anchorId, oId, internalBefore, bondsBefore };
  });

  // The fixture really built one H–H bond before the move.
  expect(result.bondsBefore).toBe(1);

  const hAtoms = result.atoms.filter((a) => a.id !== result.oId);
  const oAtom = result.atoms.find((a) => a.id === result.oId);
  expect(hAtoms.length).toBe(2);

  // Every cross pair (each H vs the O) respects the contact floor.
  for (const h of hAtoms) {
    expect(dist(h.pos, oAtom.pos)).toBeGreaterThanOrEqual(FLOOR);
  }

  // The molecule moved RIGIDLY: internal H–H distance preserved within 0.1.
  const internalAfter = dist(hAtoms[0].pos, hAtoms[1].pos);
  expect(Math.abs(internalAfter - result.internalBefore)).toBeLessThan(0.1);
});

// (d) Bonding THROUGH the floor: two H added only 50 apart (below the 130
// floor) are separated out to the floor — but the floor sits below the ~180
// bondThreshold, so they still auto-bond into a single H₂ component. Pauli
// exclusion never blocks covalent bonding.
test('builder: sub-floor add separates to the floor but still bonds (H₂)', async ({ page }) => {
  await gotoBuilder(page);

  const result = await page.evaluate(() => {
    const b = window.__builder;
    b.clear();
    b.addAtom(1, 0, 0, 0); // H at the origin
    b.addAtom(1, 50, 0, 0); // H only 50 away — below the floor
    return { atoms: b.getAtoms(), bonds: b.getBonds(), molecules: b.getMolecules() };
  });

  expect(result.atoms.length).toBe(2);
  const d = dist(result.atoms[0].pos, result.atoms[1].pos);
  // Pushed out to the floor, but kept inside bonding range.
  expect(d).toBeGreaterThanOrEqual(FLOOR);
  expect(d).toBeLessThan(180);
  // Exactly one bond → ONE 2-atom H₂ component.
  expect(result.bonds.length).toBe(1);
  expect(result.molecules.length).toBe(1);
  const mol = result.molecules[0];
  expect(mol.ids.length).toBe(2);
  expect(mol.formula === 'H₂' || mol.formula === 'H2').toBe(true);
});
