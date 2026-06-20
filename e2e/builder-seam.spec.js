// Builder window.__builder seam specs (moveMolecule / moveAtom).
// Split out of the original e2e/world.spec.js (behaviour-frozen reorganisation).
import { test } from '@playwright/test';
import {
  expect, waitForRenderedCanvas, openDrawer, gotoBuilder,
} from './helpers.js';

test.beforeEach(async ({ page }, testInfo) => {
  await page.goto('/');
  await waitForRenderedCanvas(page);
  // The controls now live in a left DRAWER that no longer auto-opens on boot.
  // For every spec EXCEPT the two drawer specs (titled "controls: …"), open the
  // drawer up front so the existing control interactions (scene-toggle, shear,
  // element selector, bond/add/clear, 2D/valence checkboxes) remain reachable.
  // The drawer specs need the initial CLOSED state, so we skip the auto-open for
  // them. The open drawer sits top-left and never overlaps the centre-right canvas
  // region the Builder drag/zoom specs operate on.
  if (!testInfo.title.startsWith('controls:')) {
    await openDrawer(page);
  }
});

// builder molecule-move (M2): single-click+drag moves the WHOLE connected
// molecule, double-click+drag moves just one atom. Verified deterministically
// through the window.__builder seam (reliable under SwiftShader): moveMolecule
// rigidly translates the whole component (every atom shifts by the SAME delta,
// internal bonds intact), while moveAtom still moves only one atom. The new
// seam entries moveMolecule(id,x,y,z) and getAtoms() arrive in M2 — RED before.
// Distances on raw world coords: bondThreshold ≈ 180. Build an H₂O chain:
// O (id 0) at origin with two H (ids 1,2) within bonding range.
test('builder: moveMolecule rigidly translates the whole molecule (seam)', async ({ page }) => {
  await gotoBuilder(page);

  // Build one 3-atom molecule: O at origin, two H within bondThreshold (≈180).
  const built = await page.evaluate(() => {
    const b = window.__builder;
    b.clear();
    b.addAtom(8, 0, 0, 0);   // O  (id 0)
    b.addAtom(1, 60, 0, 0);  // H  (id 1) — 60 from O → bonds
    b.addAtom(1, 0, 60, 0);  // H  (id 2) — 60 from O → bonds
    return {
      bonds: b.getBonds().length,
      molecules: b.getMolecules().length,
      atoms: b.getAtoms(),
    };
  });
  // One molecule of three atoms, two O–H bonds.
  expect(built.molecules).toBe(1);
  expect(built.bonds).toBe(2);
  expect(built.atoms.length).toBe(3);

  // Move the WHOLE molecule by dragging the O anchor (id 0) by a known delta.
  const DX = 250, DY = -150, DZ = 90;
  const moved = await page.evaluate(({ dx, dy, dz }) => {
    const b = window.__builder;
    const before = b.getAtoms();
    const o = before.find((a) => a.id === 0);
    b.moveMolecule(0, o.pos.x + dx, o.pos.y + dy, o.pos.z + dz);
    return {
      before,
      after: b.getAtoms(),
      bonds: b.getBonds().length,
      molecules: b.getMolecules().length,
    };
  }, { dx: DX, dy: DY, dz: DZ });

  // EVERY atom shifted by the SAME delta (rigid translation).
  for (const a0 of moved.before) {
    const a1 = moved.after.find((a) => a.id === a0.id);
    expect(Math.abs((a1.pos.x - a0.pos.x) - DX)).toBeLessThan(1e-6);
    expect(Math.abs((a1.pos.y - a0.pos.y) - DY)).toBeLessThan(1e-6);
    expect(Math.abs((a1.pos.z - a0.pos.z) - DZ)).toBeLessThan(1e-6);
  }
  // Connectivity intact: still one molecule, still two bonds.
  expect(moved.molecules).toBe(1);
  expect(moved.bonds).toBe(2);
});

// Single-atom move still works: moveAtom shifts ONLY the named atom (the
// double-click+drag path). Move one H far away → it detaches; the other two
// atoms keep their positions.
test('builder: moveAtom moves only one atom (single-atom path)', async ({ page }) => {
  await gotoBuilder(page);

  const res = await page.evaluate(() => {
    const b = window.__builder;
    b.clear();
    b.addAtom(8, 0, 0, 0);   // O  (id 0)
    b.addAtom(1, 60, 0, 0);  // H  (id 1)
    b.addAtom(1, 0, 60, 0);  // H  (id 2)
    const before = b.getAtoms();
    b.moveAtom(2, 800, 0, 0); // drag H id 2 far away (beyond breakThreshold)
    const after = b.getAtoms();
    return { before, after, molecules: b.getMolecules().length };
  });

  // O (id 0) and H (id 1) are unchanged; only id 2 moved.
  for (const id of [0, 1]) {
    const a0 = res.before.find((a) => a.id === id);
    const a1 = res.after.find((a) => a.id === id);
    expect(a1.pos.x).toBeCloseTo(a0.pos.x, 6);
    expect(a1.pos.y).toBeCloseTo(a0.pos.y, 6);
  }
  const h2before = res.before.find((a) => a.id === 2);
  const h2after = res.after.find((a) => a.id === 2);
  expect(Math.abs(h2after.pos.x - h2before.pos.x)).toBeGreaterThan(100);
  // The dragged atom detached → no longer a single 3-atom molecule.
  expect(res.molecules).toBeGreaterThan(1);
});
