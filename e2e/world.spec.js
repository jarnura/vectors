// Baseline canvas-verification spec for the vectors WebGL2 scene.
//
// This is the harness the world-backdrop milestones extend. At baseline (before
// the world meshes exist) it only asserts the canvas renders SOMETHING (the
// cubes). Milestone-specific assertions (ground band, grid lines, sky color,
// horizon transition) are added/enabled as M1–M4 land.
import { test } from '@playwright/test';
import {
  expect, waitForRenderedCanvas, readPixel, readRow, readRegion, distinctColors,
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

// atomos M2: the scene switch flips the cube POC (sky-blue) to atomos (near-black space).
test('atomos: scene switch flips backdrop sky-blue → near-black', async ({ page }) => {
  await expect(page.locator('#scene-toggle')).toBeVisible();

  // Cube POC backdrop is sky-blue (blue-dominant, bright).
  const skyBefore = await readPixel(page, 0.5, 0.04);
  expect(skyBefore[2]).toBeGreaterThan(skyBefore[0]); // blue > red
  expect(skyBefore[2]).toBeGreaterThan(120); // bright sky

  await page.click('#scene-toggle');
  await page.waitForTimeout(300);

  // Atomos backdrop is near-black deep space.
  const spaceAfter = await readPixel(page, 0.5, 0.04);
  expect(spaceAfter[0] + spaceAfter[1] + spaceAfter[2]).toBeLessThan(120); // dark

  // Switching back returns to the cube POC sky.
  await page.click('#scene-toggle');
  await page.waitForTimeout(300);
  const skyAgain = await readPixel(page, 0.5, 0.04);
  expect(skyAgain[2]).toBeGreaterThan(120);
});

// atomos M3: in atomos, a nucleus (proton/neutron spheres) is visible at center.
test('atomos: nucleus visible at center', async ({ page }) => {
  await page.click('#scene-toggle'); // → atomos
  await page.waitForTimeout(300);

  const space = await readPixel(page, 0.5, 0.04);   // deep-space backdrop (dark)
  const center = await readPixel(page, 0.5, 0.5);    // nucleus cluster at center

  // The center is lit nucleon geometry, clearly brighter than the dark backdrop.
  const spaceSum = space[0] + space[1] + space[2];
  const centerSum = center[0] + center[1] + center[2];
  expect(centerSum).toBeGreaterThan(spaceSum + 60);
});

// atomos: the QM orbital lobes render as a lit, stable cloud around the nucleus
// (orbitals are static probability shapes, not moving point electrons).
test('atomos: orbital lobes form a stable lit cloud', async ({ page }) => {
  await page.click('#scene-toggle'); // → atomos
  await page.fill('#element-value', '7'); // Nitrogen: filled p lobes around the core
  await page.waitForTimeout(400);

  // A band around the nucleus where the orbital lobes sit.
  const cloudA = await readRegion(page, 0.30, 0.30, 0.70, 0.70, 28, 16);
  await page.waitForTimeout(700);
  const cloudB = await readRegion(page, 0.30, 0.30, 0.70, 0.70, 28, 16);

  // The orbital structure is lit (more than one colour bucket) ...
  expect(distinctColors(cloudA)).toBeGreaterThan(1);
  // ... and static: the lobes do not sweep like the old point electrons.
  const moved = cloudA.filter((p, i) =>
    Math.abs(p[0] - cloudB[i][0]) + Math.abs(p[1] - cloudB[i][1]) + Math.abs(p[2] - cloudB[i][2]) > 30
  ).length;
  expect(moved).toBeLessThan(cloudA.length / 2);
});

// atomos M5: the element selector reconfigures the atom (nucleus changes).
test('atomos: element selector changes the atom', async ({ page }) => {
  await expect(page.locator('#element-value')).toBeVisible();
  await page.click('#scene-toggle'); // → atomos (default Carbon, Z=6)
  await page.waitForTimeout(300);

  // Tight center region = the nucleus (electrons sweep further out).
  const carbonNucleus = await readRegion(page, 0.42, 0.42, 0.58, 0.58, 18, 18);

  // Switch to Hydrogen (Z=1): a single nucleon — a much sparser nucleus.
  await page.fill('#element-value', '1');
  await page.waitForTimeout(400);
  const hydrogenNucleus = await readRegion(page, 0.42, 0.42, 0.58, 0.58, 18, 18);

  const changed = carbonNucleus.filter((p, i) =>
    Math.abs(p[0] - hydrogenNucleus[i][0]) + Math.abs(p[1] - hydrogenNucleus[i][1]) + Math.abs(p[2] - hydrogenNucleus[i][2]) > 24
  ).length;
  expect(changed).toBeGreaterThan(3);

  // Out-of-range Z must not crash: scene still renders something at center.
  await page.fill('#element-value', '999');
  await page.waitForTimeout(300);
  const afterBad = await readPixel(page, 0.5, 0.5);
  expect(afterBad[0] + afterBad[1] + afterBad[2]).toBeGreaterThan(40);
});

// overlay-text M1: the atomos element label (anime.js scramble) shows the
// element name and updates when the element changes.
test('overlay: element label shows/updates the element name', async ({ page }) => {
  const label = page.locator('#atom-label');

  // Hidden in the cube POC scene.
  await expect(label).toBeHidden();

  await page.click('#scene-toggle'); // → atomos (default Carbon, Z=6)
  await expect(label).toBeVisible();
  // Wait for the scramble to settle on "Carbon".
  await expect(label).toHaveText('Carbon', { timeout: 4000 });

  // Changing the element re-scrambles to the new name.
  await page.fill('#element-value', '8');
  await expect(label).toHaveText('Oxygen', { timeout: 4000 });

  // Switching back to the cube POC hides the label again.
  await page.click('#scene-toggle');
  await expect(label).toBeHidden();
});

// orbital-lines M2: the orbital-info overlay shows the electron configuration of
// the selected element and updates when the element changes; atomos-only.
test('overlay: orbital-info shows the electron configuration', async ({ page }) => {
  const info = page.locator('#orbital-info');

  // Hidden in the cube POC scene.
  await expect(info).toBeHidden();

  await page.click('#scene-toggle'); // → atomos (default Carbon, Z=6)
  await expect(info).toBeVisible();
  // Settles to Carbon's configuration.
  await expect(info).toHaveText('1s2 2s2 2p2', { timeout: 4000 });

  // Switching element re-scrambles to the new configuration.
  await page.fill('#element-value', '36');
  await expect(info).toHaveText('1s2 2s2 2p6 3s2 3p6 3d10 4s2 4p6', { timeout: 4000 });

  // Hidden again back in the cube POC scene.
  await page.click('#scene-toggle');
  await expect(info).toBeHidden();
});

// subshells M2: the element table now spans Z=1..36, so the selector reaches
// Krypton and the label shows the new name; the scene still renders.
test('subshells M2: element table reaches Krypton (Z=36)', async ({ page }) => {
  const label = page.locator('#atom-label');
  await page.click('#scene-toggle'); // → atomos

  // The selector accepts the extended range up to 36.
  await expect(page.locator('#element-value')).toHaveAttribute('max', '36');

  // Z=36 settles the label on "Krypton".
  await page.fill('#element-value', '36');
  await expect(label).toHaveText('Krypton', { timeout: 4000 });

  // The atom still renders lit geometry at center (no crash at the new max).
  const center = await readPixel(page, 0.5, 0.5);
  expect(center[0] + center[1] + center[2]).toBeGreaterThan(40);
});

// qm M3: every element renders a substantial lit orbital cloud (the occupied
// orbital lobes scaled by their physical radii), and out-of-range Z is safe.
test('qm M3: occupied orbitals render a lit cloud', async ({ page }) => {
  await page.click('#scene-toggle'); // → atomos

  const litCount = async () => {
    const r = await readRegion(page, 0.20, 0.20, 0.80, 0.80, 32, 20);
    return r.filter((p) => p[0] + p[1] + p[2] > 60).length;
  };

  await page.fill('#element-value', '6'); // Carbon: 1s 2s 2p
  await page.waitForTimeout(400);
  expect(await litCount()).toBeGreaterThan(3);

  await page.fill('#element-value', '36'); // Krypton: through 4p incl. 3d
  await page.waitForTimeout(400);
  expect(await litCount()).toBeGreaterThan(3);

  // Out-of-range Z must not crash: scene still renders lit geometry at center.
  await page.fill('#element-value', '999');
  await page.waitForTimeout(300);
  const center = await readPixel(page, 0.5, 0.5);
  expect(center[0] + center[1] + center[2]).toBeGreaterThan(40);
});

// qm M3: switching element reconfigures the orbital cloud (Carbon's s/p cloud
// differs from Iron's, which adds violet 3d cloverleaf lobes).
test('qm M3: element switch reconfigures the orbital cloud', async ({ page }) => {
  await page.click('#scene-toggle'); // → atomos
  await page.fill('#element-value', '6'); // Carbon
  await page.waitForTimeout(400);
  const carbon = await readRegion(page, 0.25, 0.25, 0.75, 0.75, 30, 18);

  await page.fill('#element-value', '26'); // Iron — adds 3d lobes
  await page.waitForTimeout(400);
  const iron = await readRegion(page, 0.25, 0.25, 0.75, 0.75, 30, 18);

  const changed = carbon.filter((p, i) =>
    Math.abs(p[0] - iron[i][0]) + Math.abs(p[1] - iron[i][1]) + Math.abs(p[2] - iron[i][2]) > 30
  ).length;
  expect(changed).toBeGreaterThan(4);
});

// qm M4: Hund's rule is visible — paired orbitals render brighter than singly
// occupied ones. Neon (2p all paired) shows more bright pixels than Nitrogen
// (2p singly occupied, dimmer).
test('qm M4: paired orbitals (Neon) are brighter than singly-occupied (Nitrogen)', async ({ page }) => {
  await page.click('#scene-toggle'); // → atomos

  const brightCount = async () => {
    const r = await readRegion(page, 0.25, 0.25, 0.75, 0.75, 30, 18);
    return r.filter((p) => p[0] + p[1] + p[2] > 200).length;
  };

  await page.fill('#element-value', '7'); // Nitrogen: 2p = [1,1,1] (all dim)
  await page.waitForTimeout(400);
  const nitrogenBright = await brightCount();

  await page.fill('#element-value', '10'); // Neon: 2p = [2,2,2] (all paired, bright)
  await page.waitForTimeout(400);
  const neonBright = await brightCount();

  // The fully-paired atom shows strictly more bright (paired-orbital) pixels.
  expect(neonBright).toBeGreaterThan(nitrogenBright);
});

// overlay-text M2: the scene-title banner scrambles to the current scene name.
test('overlay: scene title updates on scene switch', async ({ page }) => {
  const title = page.locator('#scene-title');
  await expect(title).toHaveText('Cube POC');

  await page.click('#scene-toggle'); // → atomos
  await expect(title).toHaveText('atomos', { timeout: 4000 });

  await page.click('#scene-toggle'); // → back to cube POC
  await expect(title).toHaveText('Cube POC', { timeout: 4000 });
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
