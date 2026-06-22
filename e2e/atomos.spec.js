// Atomos nucleus / electrons / selector / structure / colour specs.
// Split out of the original e2e/world.spec.js (behaviour-frozen reorganisation).
import { test } from '@playwright/test';
import {
  expect, waitForRenderedCanvas, openDrawer, readPixel, readRegion, distinctColors,
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

  // The switch is a 5-cycle (CubePoc → Atomos → Molecule → Builder → Materials → CubePoc),
  // so cycling fully around returns to the cube POC sky. Molecule, Builder, and
  // Materials all share the dark deep-space backdrop with atomos, so the next three
  // clicks keep it dark.
  await page.click('#scene-toggle'); // → molecule (still dark space backdrop)
  await page.waitForTimeout(300);
  const moleculeBackdrop = await readPixel(page, 0.5, 0.04);
  expect(moleculeBackdrop[0] + moleculeBackdrop[1] + moleculeBackdrop[2]).toBeLessThan(120);

  await page.click('#scene-toggle'); // → builder (still dark space backdrop)
  await page.waitForTimeout(300);
  const builderBackdrop = await readPixel(page, 0.5, 0.04);
  expect(builderBackdrop[0] + builderBackdrop[1] + builderBackdrop[2]).toBeLessThan(120);

  await page.click('#scene-toggle'); // → materials (still dark space backdrop)
  await page.waitForTimeout(300);
  const materialsBackdrop = await readPixel(page, 0.5, 0.04);
  expect(materialsBackdrop[0] + materialsBackdrop[1] + materialsBackdrop[2]).toBeLessThan(120);

  await page.click('#scene-toggle'); // → back to cube POC sky (5-cycle completes)
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
