// Baseline canvas-verification spec for the vectors WebGL2 scene.
//
// This is the harness the world-backdrop milestones extend. At baseline (before
// the world meshes exist) it only asserts the canvas renders SOMETHING (the
// cubes). Milestone-specific assertions (ground band, grid lines, sky color,
// horizon transition) are added/enabled as M1–M4 land.
import { test } from '@playwright/test';
import {
  expect, waitForRenderedCanvas, readPixel, readRow, distinctColors,
} from './helpers.js';

test.beforeEach(async ({ page }) => {
  await page.goto('/');
  await waitForRenderedCanvas(page);
});

test('canvas renders a non-empty scene', async ({ page }) => {
  // The scene is not a single flat color: sampling a row across the middle
  // should surface more than one color bucket (cube vs background at minimum).
  const row = await readRow(page, 0.5, 24);
  expect(distinctColors(row)).toBeGreaterThan(1);
});

test('center shows the main cube (foreground geometry present)', async ({ page }) => {
  const bg = await readPixel(page, 0.02, 0.02); // top-left corner ≈ background
  const center = await readPixel(page, 0.5, 0.5); // cube sits at screen center
  expect(center).not.toEqual(bg);
});

// --- Milestone hooks (enabled as the world meshes land) ----------------------
// M2: ground plane visible below the cube.
test.skip('M2: ground band visible below cube', async ({ page }) => {
  const sky = await readPixel(page, 0.5, 0.05);
  const ground = await readPixel(page, 0.5, 0.9);
  expect(ground).not.toEqual(sky);
});

// M3: grid lines alternate against the ground.
test.skip('M3: grid lines visible on ground', async ({ page }) => {
  const row = await readRow(page, 0.85, 32);
  expect(distinctColors(row)).toBeGreaterThan(1);
});

// M4: sky backdrop (top corner not white) + horizon transition.
test.skip('M4: sky backdrop and horizon transition', async ({ page }) => {
  const top = await readPixel(page, 0.5, 0.03);
  const white = [255, 255, 255, 255];
  expect(top).not.toEqual(white);
});
