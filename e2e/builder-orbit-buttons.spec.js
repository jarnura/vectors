// builder-orbit-buttons M2: on-screen orbit control buttons in the Builder
// controls drawer (#orbit-up, #orbit-down, #orbit-left, #orbit-right,
// #orbit-reset) drive the shared orbit Ref via Main.applyOrbit.
//
// Each directional button steps the camera by Camera.buttonOrbitDelta
// (≈ 0.1745 rad ≈ 10°). Reset writes {yaw:0, pitch:0}. Pitch is clamped to
// Camera.maxPitch (≈ 1.4835298641951802). Buttons are Builder-only — they must
// have no effect in other scenes.
//
// Seam API exposed on window.__builder (installed by OrbitApi.js):
//   setOrbit(yaw, pitch)  – writes the shared orbit Ref (pitch clamped)
//   getOrbit()            – returns { yaw, pitch }
import { test } from '@playwright/test';
import { expect, waitForRenderedCanvas, openDrawer } from './helpers.js';

// Camera constants (must match Camera.purs).
const MAX_PITCH = 1.4835298641951802;

// Expected per-click orbit step (Camera.buttonOrbitDelta ≈ 10° in radians).
// Derived from orbitSens (0.01) × 17.45 = 0.1745.
const STEP = 0.1745;

// Tolerance for a single step assertion: allow ±0.02 rad for any rounding.
const STEP_TOL = 0.02;

// Tight tolerance for reset (the Ref stores exact zeroes).
const RESET_TOL = 1e-9;

// Navigate to the Builder scene and wait for window.__builder (including
// setOrbit / getOrbit installed by OrbitApi). Mirrors builder-3d-orbit.spec.js.
async function gotoBuilder(page) {
  await expect(page.locator('#scene-toggle')).toBeVisible();
  await page.click('#scene-toggle'); // CubePoc → Atomos
  await page.click('#scene-toggle'); // Atomos  → Molecule
  await page.click('#scene-toggle'); // Molecule → Builder
  await page.waitForTimeout(700);
  await page.waitForFunction(() => !!window.__builder, null, { timeout: 6000 });
  await page.waitForFunction(
    () =>
      typeof window.__builder.getOrbit === 'function' &&
      typeof window.__builder.setOrbit === 'function',
    null,
    { timeout: 6000 },
  );
}

// Click a button N times with a brief settle between each click so the orbit
// Ref can be updated. Mirrors the clickN helper in builder-layered-zoom.spec.js
// and zoom-buttons.spec.js.
async function clickN(page, selector, n) {
  for (let i = 0; i < n; i++) {
    await page.click(selector);
    await page.waitForTimeout(80);
  }
  await page.waitForTimeout(200);
}

test.beforeEach(async ({ page }) => {
  await page.goto('/');
  await waitForRenderedCanvas(page);
  // Controls (scene-toggle + orbit buttons) live in the left drawer that no
  // longer auto-opens on boot — open it up front. Mirrors all other builder
  // specs that interact with drawer controls.
  await openDrawer(page);
});

// ---------------------------------------------------------------------------
// 1 — Baseline: after setOrbit(0,0) getOrbit reads back zero.
// ---------------------------------------------------------------------------

test('builder orbit-buttons: baseline getOrbit returns {yaw:0,pitch:0} after reset via seam', async ({ page }) => {
  await gotoBuilder(page);

  const orbit = await page.evaluate(() => {
    window.__builder.setOrbit(0, 0);
    return window.__builder.getOrbit();
  });

  expect(Math.abs(orbit.yaw)).toBeLessThan(RESET_TOL + 1e-10);
  expect(Math.abs(orbit.pitch)).toBeLessThan(RESET_TOL + 1e-10);
});

// ---------------------------------------------------------------------------
// 2 — Right/left yaw step: each button increments / decrements yaw by ≈ STEP.
// ---------------------------------------------------------------------------

test('builder orbit-buttons: #orbit-right increases yaw by one step', async ({ page }) => {
  await gotoBuilder(page);

  // Start at known zero.
  await page.evaluate(() => window.__builder.setOrbit(0, 0));

  // Click #orbit-right once — this button does not yet exist (RED).
  await page.click('#orbit-right');
  await page.waitForTimeout(150);

  const orbit = await page.evaluate(() => window.__builder.getOrbit());

  // Yaw must have increased by ≈ STEP (positive yaw = rightward rotation).
  expect(Math.abs(Math.abs(orbit.yaw) - STEP)).toBeLessThan(STEP_TOL);
  // Pitch must remain at zero (right button only affects yaw).
  expect(Math.abs(orbit.pitch)).toBeLessThan(STEP_TOL);
});

test('builder orbit-buttons: #orbit-left decreases yaw by one step (symmetric)', async ({ page }) => {
  await gotoBuilder(page);

  // Start one step to the right so we can step back symmetrically.
  await page.evaluate(() => window.__builder.setOrbit(0, 0));
  await page.click('#orbit-right');
  await page.waitForTimeout(150);

  const yawAfterRight = (await page.evaluate(() => window.__builder.getOrbit())).yaw;

  // Now click #orbit-left once — must return to ≈ 0.
  await page.click('#orbit-left');
  await page.waitForTimeout(150);

  const orbit = await page.evaluate(() => window.__builder.getOrbit());

  // Yaw returns toward zero (symmetric step in the opposite direction).
  expect(Math.abs(orbit.yaw)).toBeLessThan(STEP_TOL);
  // The step taken by left must cancel the step taken by right.
  expect(Math.abs(yawAfterRight - orbit.yaw - STEP)).toBeLessThan(STEP_TOL);
});

// ---------------------------------------------------------------------------
// 3 — Up pitch step + pole clamp.
// ---------------------------------------------------------------------------

test('builder orbit-buttons: #orbit-up changes pitch by one step', async ({ page }) => {
  await gotoBuilder(page);

  await page.evaluate(() => window.__builder.setOrbit(0, 0));

  // One click of #orbit-up — button does not yet exist (RED).
  await page.click('#orbit-up');
  await page.waitForTimeout(150);

  const orbit = await page.evaluate(() => window.__builder.getOrbit());

  // Pitch must have changed by ≈ STEP in magnitude (sign depends on convention).
  expect(Math.abs(Math.abs(orbit.pitch) - STEP)).toBeLessThan(STEP_TOL);
  // Yaw must remain at zero (up button only affects pitch).
  expect(Math.abs(orbit.yaw)).toBeLessThan(STEP_TOL);
});

test('builder orbit-buttons: repeated #orbit-up clamps pitch at Camera.maxPitch (no pole flip)', async ({ page }) => {
  await gotoBuilder(page);

  await page.evaluate(() => window.__builder.setOrbit(0, 0));

  // 20 clicks is well past the clamp (20 × 0.1745 ≈ 3.49 >> maxPitch ≈ 1.4835).
  await clickN(page, '#orbit-up', 20);

  const orbit = await page.evaluate(() => window.__builder.getOrbit());

  // Pitch magnitude must saturate at ≤ maxPitch (camera never flips over pole).
  expect(Math.abs(orbit.pitch)).toBeLessThanOrEqual(MAX_PITCH + STEP_TOL);
  // Must be near the ceiling, not somewhere far below (clamp engaged).
  expect(Math.abs(orbit.pitch)).toBeGreaterThanOrEqual(MAX_PITCH - STEP_TOL);
});

test('builder orbit-buttons: #orbit-down moves pitch in the opposite direction to #orbit-up', async ({ page }) => {
  await gotoBuilder(page);

  await page.evaluate(() => window.__builder.setOrbit(0, 0));

  // One up, one down — net pitch should return to ≈ 0.
  await page.click('#orbit-up');
  await page.waitForTimeout(150);
  await page.click('#orbit-down');
  await page.waitForTimeout(150);

  const orbit = await page.evaluate(() => window.__builder.getOrbit());

  expect(Math.abs(orbit.pitch)).toBeLessThan(STEP_TOL);
});

// ---------------------------------------------------------------------------
// 4 — Reset: clicking #orbit-reset writes {yaw:0, pitch:0} exactly.
// ---------------------------------------------------------------------------

test('builder orbit-buttons: #orbit-reset returns orbit to {yaw:0,pitch:0}', async ({ page }) => {
  await gotoBuilder(page);

  // Drive to a clearly non-zero orbit first.
  await page.evaluate(() => window.__builder.setOrbit(0.8, 0.6));
  const before = await page.evaluate(() => window.__builder.getOrbit());
  expect(Math.abs(before.yaw)).toBeGreaterThan(0.5); // confirm we are off-zero

  // Click #orbit-reset — does not yet exist (RED).
  await page.click('#orbit-reset');
  await page.waitForTimeout(150);

  const orbit = await page.evaluate(() => window.__builder.getOrbit());

  // Both angles must be exactly 0 (tight tolerance — the Ref stores exact value).
  expect(Math.abs(orbit.yaw)).toBeLessThan(RESET_TOL + 1e-10);
  expect(Math.abs(orbit.pitch)).toBeLessThan(RESET_TOL + 1e-10);
});

test('builder orbit-buttons: #orbit-reset after button-driven orbit also clears', async ({ page }) => {
  await gotoBuilder(page);

  await page.evaluate(() => window.__builder.setOrbit(0, 0));

  // Get there via button clicks so reset is tested against button-driven state.
  await clickN(page, '#orbit-right', 3);
  await clickN(page, '#orbit-up', 2);

  const before = await page.evaluate(() => window.__builder.getOrbit());
  // Confirm we are off-zero.
  expect(
    Math.abs(before.yaw) + Math.abs(before.pitch),
  ).toBeGreaterThan(STEP_TOL);

  await page.click('#orbit-reset');
  await page.waitForTimeout(150);

  const orbit = await page.evaluate(() => window.__builder.getOrbit());
  expect(Math.abs(orbit.yaw)).toBeLessThan(RESET_TOL + 1e-10);
  expect(Math.abs(orbit.pitch)).toBeLessThan(RESET_TOL + 1e-10);
});

// ---------------------------------------------------------------------------
// 5 — Builder-only: orbit buttons have no effect in other scenes.
// ---------------------------------------------------------------------------

test('builder orbit-buttons: buttons are inert (or absent) outside Builder scene', async ({ page }) => {
  // Stay in the default CubePoc scene — do NOT navigate to Builder.
  // Attempt to read the orbit seam (may not exist outside Builder boot).
  const orbitBefore = await page.evaluate(() => {
    if (window.__builder && typeof window.__builder.getOrbit === 'function') {
      return window.__builder.getOrbit();
    }
    return null;
  });

  // Try to click each orbit button. In RED they don't exist so clicks throw /
  // time out; we catch and swallow those so the REAL assertion (orbit unchanged)
  // is what decides pass/fail.
  const tryClick = async (sel) => {
    try {
      await page.click(sel, { timeout: 2000 });
    } catch (_) {
      // Button absent or not visible — expected in RED and in non-Builder scenes.
    }
  };

  await tryClick('#orbit-right');
  await tryClick('#orbit-up');
  await tryClick('#orbit-left');
  await tryClick('#orbit-down');

  const orbitAfter = await page.evaluate(() => {
    if (window.__builder && typeof window.__builder.getOrbit === 'function') {
      return window.__builder.getOrbit();
    }
    return null;
  });

  // If the seam exists, orbit must not have changed (buttons inert outside Builder).
  // If the seam doesn't exist in this scene, both values are null — also pass.
  if (orbitBefore !== null && orbitAfter !== null) {
    expect(Math.abs(orbitAfter.yaw - orbitBefore.yaw)).toBeLessThan(STEP_TOL);
    expect(Math.abs(orbitAfter.pitch - orbitBefore.pitch)).toBeLessThan(STEP_TOL);
  } else {
    // Seam not available outside Builder — that is fine; buttons cannot orbit.
    expect(orbitBefore).toBeNull();
  }
});

test('builder orbit-buttons: buttons are inert in Atomos scene', async ({ page }) => {
  // Navigate only to Atomos (one #scene-toggle click).
  await page.click('#scene-toggle'); // CubePoc → Atomos
  await page.waitForTimeout(500);

  const orbitBefore = await page.evaluate(() => {
    if (window.__builder && typeof window.__builder.getOrbit === 'function') {
      return window.__builder.getOrbit();
    }
    return null;
  });

  const tryClick = async (sel) => {
    try {
      await page.click(sel, { timeout: 2000 });
    } catch (_) {}
  };

  await tryClick('#orbit-right');
  await tryClick('#orbit-up');

  await page.waitForTimeout(200);

  const orbitAfter = await page.evaluate(() => {
    if (window.__builder && typeof window.__builder.getOrbit === 'function') {
      return window.__builder.getOrbit();
    }
    return null;
  });

  if (orbitBefore !== null && orbitAfter !== null) {
    expect(Math.abs(orbitAfter.yaw - orbitBefore.yaw)).toBeLessThan(STEP_TOL);
    expect(Math.abs(orbitAfter.pitch - orbitBefore.pitch)).toBeLessThan(STEP_TOL);
  } else {
    expect(orbitBefore).toBeNull();
  }
});
