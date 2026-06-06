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

// atomos: discrete electrons orbit on the orbital rings — the region around the
// nucleus changes across frames as the electron dots sweep their rings.
test('atomos: electrons orbit on the rings', async ({ page }) => {
  await page.click('#scene-toggle'); // → atomos
  await page.fill('#element-value', '7'); // Nitrogen
  await page.waitForTimeout(400);

  // A band around the nucleus where the rings + electrons sit.
  const ringA = await readRegion(page, 0.30, 0.30, 0.70, 0.70, 28, 16);
  await page.waitForTimeout(700); // let the electrons advance along their rings
  const ringB = await readRegion(page, 0.30, 0.30, 0.70, 0.70, 28, 16);

  // Ring/electron structure is lit ...
  expect(distinctColors(ringA)).toBeGreaterThan(1);
  // ... and the electrons orbit: several sampled points change across frames.
  const moved = ringA.filter((p, i) =>
    Math.abs(p[0] - ringB[i][0]) + Math.abs(p[1] - ringB[i][1]) + Math.abs(p[2] - ringB[i][2]) > 30
  ).length;
  expect(moved).toBeGreaterThan(2);
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

// atomos: orbital ring lines + discrete electrons render lit structure for any
// element, and out-of-range Z is render-safe.
test('atomos: orbital rings + electrons render lit structure', async ({ page }) => {
  await page.click('#scene-toggle'); // → atomos

  const litCount = async () => {
    const r = await readRegion(page, 0.20, 0.20, 0.80, 0.80, 32, 20);
    return r.filter((p) => p[0] + p[1] + p[2] > 60).length;
  };

  await page.fill('#element-value', '6'); // Carbon
  await page.waitForTimeout(400);
  expect(await litCount()).toBeGreaterThan(3);

  await page.fill('#element-value', '36'); // Krypton (many rings)
  await page.waitForTimeout(400);
  expect(await litCount()).toBeGreaterThan(3);

  // Out-of-range Z must not crash: scene still renders lit geometry at center.
  await page.fill('#element-value', '999');
  await page.waitForTimeout(300);
  const center = await readPixel(page, 0.5, 0.5);
  expect(center[0] + center[1] + center[2]).toBeGreaterThan(40);
});

// atomos: switching element reconfigures the atom (Carbon's rings/electrons
// differ from Iron's, which adds an inner d sub-shell + more electrons).
test('atomos: element switch reconfigures the atom', async ({ page }) => {
  await page.click('#scene-toggle'); // → atomos
  await page.fill('#element-value', '6'); // Carbon
  await page.waitForTimeout(400);
  const carbon = await readRegion(page, 0.25, 0.25, 0.75, 0.75, 30, 18);

  await page.fill('#element-value', '26'); // Iron
  await page.waitForTimeout(400);
  const iron = await readRegion(page, 0.25, 0.25, 0.75, 0.75, 30, 18);

  const changed = carbon.filter((p, i) =>
    Math.abs(p[0] - iron[i][0]) + Math.abs(p[1] - iron[i][1]) + Math.abs(p[2] - iron[i][2]) > 30
  ).length;
  expect(changed).toBeGreaterThan(4);
});

// atomos: electrons are discrete — a many-electron atom (Argon, 18) lights up
// noticeably more electron/ring structure than a one-electron atom (Hydrogen).
test('atomos: more electrons show more discrete structure', async ({ page }) => {
  await page.click('#scene-toggle'); // → atomos

  const litCount = async () => {
    const r = await readRegion(page, 0.18, 0.18, 0.82, 0.82, 36, 24);
    return r.filter((p) => p[0] + p[1] + p[2] > 90).length;
  };

  await page.fill('#element-value', '1'); // Hydrogen: 1 electron, 1 ring
  await page.waitForTimeout(400);
  const hydrogen = await litCount();

  await page.fill('#element-value', '18'); // Argon: 18 electrons, more rings
  await page.waitForTimeout(400);
  const argon = await litCount();

  expect(argon).toBeGreaterThan(hydrogen);
});

// colours: each shell is a distinct colour (sub-shells lighter), so a multi-shell
// element (Krypton, 4 shells) shows many more colours than a single-shell one (Helium).
test('atomos: shells are colour-coded (multi-shell shows more colours)', async ({ page }) => {
  await page.click('#scene-toggle'); // → atomos

  const colourCount = async () => {
    const r = await readRegion(page, 0.18, 0.18, 0.82, 0.82, 36, 24);
    return distinctColors(r);
  };

  await page.fill('#element-value', '2'); // Helium: 1 shell (one ring colour)
  await page.waitForTimeout(400);
  const helium = await colourCount();

  await page.fill('#element-value', '36'); // Krypton: 4 shells (red/amber/green/blue)
  await page.waitForTimeout(400);
  const krypton = await colourCount();

  // The multi-shell atom surfaces clearly more distinct colours.
  expect(krypton).toBeGreaterThan(helium + 2);
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
