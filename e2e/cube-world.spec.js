// Cube POC + world-backdrop canvas-verification specs.
// Split out of the original e2e/world.spec.js (behaviour-frozen reorganisation).
import { test } from '@playwright/test';
import {
  expect, waitForRenderedCanvas, openDrawer, readPixel, readRow, readRegion, distinctColors,
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
// M2: ground plane visible below the cube (green ground vs white background).
test('M2: ground band visible below cube', async ({ page }) => {
  const sky = await readPixel(page, 0.5, 0.05);      // top: background
  const ground = await readPixel(page, 0.5, 0.92);   // bottom: ground plane
  // Ground is green (G dominant); background is white. They must differ.
  expect(ground).not.toEqual(sky);
  expect(ground[1]).toBeGreaterThan(ground[0]); // green channel dominates red
});

// M3: grid lines add color variation over the otherwise-flat ground.
test('M3: grid lines visible on ground', async ({ page }) => {
  // Sample a 2D region of the ground (lower-center). A flat solid ground would
  // be ~1 color bucket; the grid lines introduce additional buckets.
  const region = await readRegion(page, 0.25, 0.6, 0.75, 0.95, 30, 14);
  expect(distinctColors(region)).toBeGreaterThan(1);
});

// Shear button: clicking with a value shears the main cube (pixels change),
// while the static ground/grid stay put.
test('shear button: clicking shears the main cube', async ({ page }) => {
  await expect(page.locator('#shear-btn')).toBeVisible();
  await expect(page.locator('#shear-value')).toBeVisible();

  // Capture the cube region before the shear.
  const before = await readRegion(page, 0.35, 0.3, 0.65, 0.6, 16, 10);

  await page.fill('#shear-value', '1.5');
  await page.click('#shear-btn');
  await page.waitForTimeout(300); // let a few frames render the sheared transform

  const after = await readRegion(page, 0.35, 0.3, 0.65, 0.6, 16, 10);

  // The cube region must change after applying a non-zero shear.
  const changed = before.some((p, i) =>
    Math.abs(p[0] - after[i][0]) + Math.abs(p[1] - after[i][1]) + Math.abs(p[2] - after[i][2]) > 24
  );
  expect(changed).toBe(true);

  // The static ground far below stays the same color (world backdrop unaffected).
  const ground = await readPixel(page, 0.5, 0.95);
  expect(ground[1]).toBeGreaterThan(ground[0]); // still green-dominant
});

// molecule-builder M3 (TEST D, regression): in the DEFAULT Cube POC scene a mouse
// click-drag over the canvas still ROTATES the cube — proving the new Builder
// pick+drag is scene-gated and did NOT break cube-POC mouse rotation. This uses a
// genuine down→move→up drag (distinct from the existing keyboard/shear specs).
test('cube POC: mouse still rotates the cube (drag scene-gated)', async ({ page }) => {
  // Default scene is Cube POC — no scene toggle.
  const box = await page.locator('#canvas').boundingBox();
  const cx = box.x + box.width / 2;
  const cy = box.y + box.height / 2;

  // Capture the cube region (screen centre) before the drag.
  const before = await readRegion(page, 0.35, 0.3, 0.65, 0.6, 16, 10);

  // A genuine click-drag across the canvas: this is the mouse-rotation path.
  await page.mouse.move(cx, cy);
  await page.mouse.down();
  await page.mouse.move(cx + 120, cy + 40, { steps: 6 });
  await page.mouse.move(cx + 180, cy + 80, { steps: 6 });
  await page.mouse.up();
  await page.waitForTimeout(300); // let the rotated transform render

  const after = await readRegion(page, 0.35, 0.3, 0.65, 0.6, 16, 10);

  // The cube region changed: the drag rotated the cube (mouse rotation intact).
  const changed = before.filter((p, i) =>
    Math.abs(p[0] - after[i][0]) + Math.abs(p[1] - after[i][1]) + Math.abs(p[2] - after[i][2]) > 24
  ).length;
  expect(changed).toBeGreaterThan(2);
});

// M4: sky backdrop (top is sky-blue, not white) + ground/sky differ.
test('M4: sky backdrop and horizon transition', async ({ page }) => {
  const top = await readPixel(page, 0.5, 0.03);     // sky region
  const ground = await readPixel(page, 0.5, 0.92);  // ground region
  // No longer white.
  expect(top).not.toEqual([255, 255, 255, 255]);
  // Sky is blue-dominant.
  expect(top[2]).toBeGreaterThan(top[0]);
  // There is a horizon: sky and ground are different colors.
  expect(top).not.toEqual(ground);
});
