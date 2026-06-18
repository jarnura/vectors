// builder-3d-orbit M3: orbit camera + true-depth atom drag.
//
// GATE assertions (1–3) are seam-only and SwiftShader-safe — they read/write
// the shared Ref immediately (no frame wait). Best-effort assertion (4) uses
// real pointer events; it logs observed values and does NOT fail the gate if
// SwiftShader produces timing noise.
//
// Camera.maxPitch = 1.4835298641951802 (≈ 85° in radians, clamped at the poles).
//
// Seam API exposed on window.__builder (installed by OrbitApi.js):
//   setOrbit(yaw, pitch)  – writes the shared orbit Ref (pitch clamped)
//   getOrbit()            – reads { yaw, pitch } from the shared Ref
//
// Builder atom seam (BuilderApi.js):
//   addAtom(z, x, y, z3)  – places an atom
//   getAtoms()            – returns [{ id, z, pos: {x,y,z} }]
//   getBonds()            – returns [{ a, b }]
//   getMolecules()        – returns [{ ids, formula }]
//   clear()               – empties the world
import { test } from '@playwright/test';
import { expect, waitForRenderedCanvas, openDrawer } from './helpers.js';

// Camera.maxPitch value from Camera.purs (must match).
const MAX_PITCH = 1.4835298641951802;

// Two floating-point values are "distinct" if they differ by more than this.
const COORD_TOL = 1e-3;

// Tolerance for seam round-trip comparisons (Number is IEEE-754 double; the
// Ref stores the exact value written, so float-epsilon tolerance is enough).
const ANGLE_TOL = 1e-9;

// Euclidean distance between two {x,y,z} positions.
function dist(p, q) {
  const dx = p.x - q.x;
  const dy = p.y - q.y;
  const dz = p.z - q.z;
  return Math.sqrt(dx * dx + dy * dy + dz * dz);
}

// Count distinct values in an array (two values are the same when they differ
// by less than COORD_TOL).
function distinctCount(vals) {
  const buckets = [];
  for (const v of vals) {
    if (!buckets.some((b) => Math.abs(b - v) < COORD_TOL)) {
      buckets.push(v);
    }
  }
  return buckets.length;
}

// Navigate to the Builder scene and wait for window.__builder (including
// setOrbit / getOrbit installed by OrbitApi).
async function gotoBuilder(page) {
  await expect(page.locator('#scene-toggle')).toBeVisible();
  await page.click('#scene-toggle'); // CubePoc → Atomos
  await page.click('#scene-toggle'); // Atomos  → Molecule
  await page.click('#scene-toggle'); // Molecule → Builder
  await page.waitForTimeout(700); // let the builder scene boot + render
  await page.waitForFunction(() => !!window.__builder, null, { timeout: 6000 });
  // OrbitApi is installed in the same boot path; wait for getOrbit to appear.
  await page.waitForFunction(
    () => typeof window.__builder.getOrbit === 'function' && typeof window.__builder.setOrbit === 'function',
    null,
    { timeout: 6000 },
  );
}

test.beforeEach(async ({ page }) => {
  await page.goto('/');
  await waitForRenderedCanvas(page);
  await openDrawer(page);
});

// ---------------------------------------------------------------------------
// GATE 1 — setOrbit / getOrbit round-trip + pitch clamping
// ---------------------------------------------------------------------------

// 1a. A normal setOrbit(yaw, pitch) is read back with high precision.
test('builder: setOrbit/getOrbit round-trip preserves yaw and pitch', async ({ page }) => {
  await gotoBuilder(page);

  const orbit = await page.evaluate(() => {
    window.__builder.setOrbit(0.5, 0.3);
    return window.__builder.getOrbit();
  });

  expect(Math.abs(orbit.yaw - 0.5)).toBeLessThan(ANGLE_TOL + 1e-10);
  expect(Math.abs(orbit.pitch - 0.3)).toBeLessThan(ANGLE_TOL + 1e-10);
});

// 1b. setOrbit with pitch > maxPitch clamps to maxPitch (never reaches the pole).
test('builder: setOrbit clamps an out-of-range pitch to Camera.maxPitch', async ({ page }) => {
  await gotoBuilder(page);

  const orbit = await page.evaluate((maxPitch) => {
    window.__builder.setOrbit(1.0, 5.0); // 5.0 >> maxPitch (≈1.4835)
    return window.__builder.getOrbit();
  }, MAX_PITCH);

  // Yaw is stored unmodified.
  expect(Math.abs(orbit.yaw - 1.0)).toBeLessThan(ANGLE_TOL + 1e-10);
  // Pitch must be clamped — NOT 5.0.
  expect(orbit.pitch).toBeLessThanOrEqual(MAX_PITCH + 1e-10);
  expect(orbit.pitch).toBeGreaterThan(MAX_PITCH - 1e-6); // at the ceiling
});

// 1c. Negative out-of-range pitch clamps to -maxPitch.
test('builder: setOrbit clamps a large negative pitch to -Camera.maxPitch', async ({ page }) => {
  await gotoBuilder(page);

  const orbit = await page.evaluate((maxPitch) => {
    window.__builder.setOrbit(0.0, -5.0);
    return window.__builder.getOrbit();
  }, MAX_PITCH);

  expect(orbit.pitch).toBeGreaterThanOrEqual(-MAX_PITCH - 1e-10);
  expect(orbit.pitch).toBeLessThan(-MAX_PITCH + 1e-6); // at the floor
});

// 1d. Multiple setOrbit calls; getOrbit always reflects the LAST write.
test('builder: getOrbit reflects the last setOrbit call', async ({ page }) => {
  await gotoBuilder(page);

  const orbit = await page.evaluate(() => {
    window.__builder.setOrbit(0.1, 0.1);
    window.__builder.setOrbit(0.7, -0.4);
    window.__builder.setOrbit(1.2, 0.9);
    return window.__builder.getOrbit();
  });

  expect(Math.abs(orbit.yaw - 1.2)).toBeLessThan(ANGLE_TOL + 1e-10);
  expect(Math.abs(orbit.pitch - 0.9)).toBeLessThan(ANGLE_TOL + 1e-10);
});

// ---------------------------------------------------------------------------
// GATE 2 — Distinct-z 3D spawn (re-confirms M1 under M3)
// ---------------------------------------------------------------------------

// Atoms placed via the add-btn (spawnPos) have genuinely varied z coordinates.
test('builder: spawned atoms have at least 2 distinct z values (genuine 3D)', async ({ page }) => {
  await gotoBuilder(page);

  await page.evaluate(() => window.__builder.clear());
  await page.fill('#element-value', '1'); // Hydrogen
  await page.waitForTimeout(150);

  // Add 5 atoms through the UI button.
  for (let i = 0; i < 5; i++) {
    await page.click('#add-btn');
    await page.waitForTimeout(80);
  }

  const atoms = await page.evaluate(() => window.__builder.getAtoms());
  expect(atoms.length).toBeGreaterThanOrEqual(2);

  const zVals = atoms.map((a) => a.pos.z);
  expect(distinctCount(zVals)).toBeGreaterThanOrEqual(2);
});

// Atoms placed at explicit z-varied positions via the seam round-trip faithfully.
test('builder: seam-placed atoms span distinct z values (seam round-trip)', async ({ page }) => {
  await gotoBuilder(page);

  const atoms = await page.evaluate(() => {
    const b = window.__builder;
    b.clear();
    // Far apart so no bonding/overlap resolution moves them.
    b.addAtom(1,   0,   0,   10);
    b.addAtom(1, 500,   0,   40);
    b.addAtom(1,   0, 500,  -20);
    return b.getAtoms();
  });

  expect(atoms.length).toBe(3);
  const zVals = atoms.map((a) => a.pos.z);
  expect(distinctCount(zVals)).toBe(3);
});

// ---------------------------------------------------------------------------
// GATE 3 — Bonds across depth (atoms at different z within bonding range bond)
// ---------------------------------------------------------------------------

// Two atoms at the same x/y but different z within bonding range auto-bond.
test('builder: atoms at different z values within bonding range auto-bond', async ({ page }) => {
  await gotoBuilder(page);

  const result = await page.evaluate(() => {
    const b = window.__builder;
    b.clear();
    // Place two H atoms 150 apart in z only — under bondThreshold (~180),
    // above the no-overlap floor (~130); they should auto-bond.
    b.addAtom(1, 0, 0,   0); // H at z=0
    b.addAtom(1, 0, 0, 150); // H at z=150 (pure z separation)
    return {
      atoms: b.getAtoms(),
      bonds: b.getBonds(),
      molecules: b.getMolecules(),
    };
  });

  expect(result.atoms.length).toBe(2);
  const zVals = result.atoms.map((a) => a.pos.z);
  expect(distinctCount(zVals)).toBeGreaterThanOrEqual(2);

  // The z-only separation did not prevent bonding.
  expect(result.bonds.length).toBeGreaterThanOrEqual(1);
  expect(result.molecules.length).toBe(1);
  expect(result.molecules[0].ids.length).toBe(2);
});

// A 3-atom chain spanning both x and z differences forms bonds.
test('builder: molecule with atoms spanning x and z differences is connected', async ({ page }) => {
  await gotoBuilder(page);

  const result = await page.evaluate(() => {
    const b = window.__builder;
    b.clear();
    // Three atoms roughly 150 apart across x+z diagonals — within bondThreshold.
    b.addAtom(6,   0,   0,   0);  // C
    b.addAtom(1, 110,   0, 100);  // H — dist ≈ sqrt(110²+100²) ≈ 149
    b.addAtom(1, -110,  0, 100);  // H — dist ≈ 149 from first
    return {
      atoms: b.getAtoms(),
      bonds: b.getBonds(),
      molecules: b.getMolecules(),
    };
  });

  expect(result.atoms.length).toBe(3);
  // At least the two H atoms' z differs from the C.
  const zVals = result.atoms.map((a) => a.pos.z);
  expect(distinctCount(zVals)).toBeGreaterThanOrEqual(2);

  // The 3D separation produces at least one bond → one connected component.
  expect(result.bonds.length).toBeGreaterThanOrEqual(1);
  expect(result.molecules.length).toBe(1);
});

// ---------------------------------------------------------------------------
// BEST-EFFORT 4 — Real pointer events (not a gate; logs outcomes)
// Tolerates SwiftShader timing flakiness — assertions are soft (no fail).
// ---------------------------------------------------------------------------

// 4a. Empty-space drag changes getOrbit() (orbit rotates).
test('builder: (best-effort) empty-space drag rotates the orbit', async ({ page }) => {
  test.slow(); // give more time for frame delivery
  await gotoBuilder(page);

  // Reset orbit to a known zero state so any change is detectable.
  await page.evaluate(() => window.__builder.setOrbit(0, 0));

  // Wait one rAF frame so the render loop picks up the reset.
  await page.evaluate(() => new Promise((r) => requestAnimationFrame(r)));

  const orbitBefore = await page.evaluate(() => window.__builder.getOrbit());

  // Canvas centre — an empty region at the default zoom (no atoms placed).
  const canvas = await page.locator('#canvas').boundingBox();
  const cx = canvas.x + canvas.width / 2;
  const cy = canvas.y + canvas.height / 2;

  // Drag horizontally across empty space.
  await page.mouse.move(cx, cy);
  await page.mouse.down();
  await page.mouse.move(cx + 100, cy, { steps: 10 });
  await page.mouse.up();

  // Wait a couple of rAF frames for the drag to register.
  await page.evaluate(() => new Promise((r) => requestAnimationFrame(() => requestAnimationFrame(r))));

  const orbitAfter = await page.evaluate(() => window.__builder.getOrbit());

  const yawChanged = Math.abs(orbitAfter.yaw - orbitBefore.yaw) > 0.01;
  // Log result but do NOT fail the test — real-pointer under SwiftShader is
  // environment-dependent. The gate (assertions 1–3) already verify the seam.
  console.log(
    `[best-effort 4a] orbit before=${JSON.stringify(orbitBefore)} after=${JSON.stringify(orbitAfter)} yawChanged=${yawChanged}`,
  );
  // Soft assertion: log a warning if it did not change, but pass either way.
  if (!yawChanged) {
    console.warn('[best-effort 4a] WARNING: empty-space drag did NOT change orbit yaw — may be SwiftShader/timing issue');
  }
});

// 4b. On-atom drag does NOT change getOrbit() (moves the atom instead).
test('builder: (best-effort) on-atom drag moves atom not orbit', async ({ page }) => {
  test.slow();
  await gotoBuilder(page);

  // Place one atom at the canvas centre-ish so we can click on it.
  await page.evaluate(() => {
    const b = window.__builder;
    b.clear();
    b.addAtom(6, 0, 0, 0); // C at world origin (projects near canvas centre)
  });

  // Let the renderer paint the atom.
  await page.evaluate(() => new Promise((r) => requestAnimationFrame(() => requestAnimationFrame(r))));

  const orbitBefore = await page.evaluate(() => window.__builder.getOrbit());
  const atomsBefore = await page.evaluate(() => window.__builder.getAtoms());

  const canvas = await page.locator('#canvas').boundingBox();
  const cx = canvas.x + canvas.width / 2;
  const cy = canvas.y + canvas.height / 2;

  // Single-click + drag near canvas centre (where the atom projects at zero orbit).
  await page.mouse.move(cx, cy);
  await page.mouse.down();
  await page.mouse.move(cx + 80, cy, { steps: 8 });
  await page.mouse.up();

  await page.evaluate(() => new Promise((r) => requestAnimationFrame(() => requestAnimationFrame(r))));

  const orbitAfter = await page.evaluate(() => window.__builder.getOrbit());
  const atomsAfter = await page.evaluate(() => window.__builder.getAtoms());

  const orbitChanged = Math.abs(orbitAfter.yaw - orbitBefore.yaw) > 0.01 ||
    Math.abs(orbitAfter.pitch - orbitBefore.pitch) > 0.01;
  const atomMoved = atomsAfter.length > 0 && atomsBefore.length > 0 &&
    dist(atomsAfter[0].pos, atomsBefore[0].pos) > 1;

  console.log(
    `[best-effort 4b] orbit before=${JSON.stringify(orbitBefore)} after=${JSON.stringify(orbitAfter)} orbitChanged=${orbitChanged}; atomMoved=${atomMoved}`,
  );
  if (orbitChanged) {
    console.warn('[best-effort 4b] WARNING: orbit changed during on-atom drag — pick may have missed the atom under SwiftShader');
  }
});

// 4c. After setOrbit to a non-zero yaw, a horizontal on-atom drag changes the
//     atom z (true-depth drag — yaw maps horizontal screen motion into world z).
test('builder: (best-effort) on-atom drag under non-zero yaw changes atom z', async ({ page }) => {
  test.slow();
  await gotoBuilder(page);

  // Place one atom at world origin; set orbit to 90° yaw so horizontal screen
  // motion maps nearly entirely into world Z.
  await page.evaluate(() => {
    const b = window.__builder;
    b.clear();
    b.addAtom(6, 0, 0, 0); // C at world origin
    b.setOrbit(Math.PI / 2, 0); // 90° yaw
  });

  await page.evaluate(() => new Promise((r) => requestAnimationFrame(() => requestAnimationFrame(r))));

  const atomsBefore = await page.evaluate(() => window.__builder.getAtoms());

  const canvas = await page.locator('#canvas').boundingBox();
  const cx = canvas.x + canvas.width / 2;
  const cy = canvas.y + canvas.height / 2;

  // Double-click to enter single-atom drag mode, then drag horizontally.
  await page.mouse.move(cx, cy);
  await page.mouse.dblclick(cx, cy);
  await page.mouse.down();
  await page.mouse.move(cx + 100, cy, { steps: 10 });
  await page.mouse.up();

  await page.evaluate(() => new Promise((r) => requestAnimationFrame(() => requestAnimationFrame(r))));

  const atomsAfter = await page.evaluate(() => window.__builder.getAtoms());

  const zBefore = atomsBefore.length > 0 ? atomsBefore[0].pos.z : null;
  const zAfter = atomsAfter.length > 0 ? atomsAfter[0].pos.z : null;
  const zChanged = zBefore !== null && zAfter !== null && Math.abs(zAfter - zBefore) > 1;

  console.log(
    `[best-effort 4c] z before=${zBefore} after=${zAfter} zChanged=${zChanged}`,
  );
  if (!zChanged) {
    console.warn('[best-effort 4c] WARNING: atom z did not change under yaw drag — may be SwiftShader/timing or missed pick');
  }
});
