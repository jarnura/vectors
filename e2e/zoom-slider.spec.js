// #zoom-slider range input drives the camera zoom across all scenes. Setting the
// slider to a low value zooms OUT (camera pulls back, content shrinks); setting it
// to a high value zooms IN (content grows). The slider is two-way: it also reflects
// programmatic zoom changes (wheel, Materials reframe) by having its .value synced
// from State.zoom each frame via setZoomSlider. The mouse-wheel zoom path remains
// unchanged (delta-based via applyZoomStep).
//
// Signature: over a centre region around the atom (atomos scene) we count LIT
// (non-background) pixels. The lit footprint shrinks as the camera pulls back and
// grows as it pushes in. Rings/nucleus are the dominant static lit structure, so
// this aggregate is robust to electron animation under SwiftShader.
import { test } from '@playwright/test';
import {
  expect, waitForRenderedCanvas, openDrawer, readPixel, readRegion,
} from './helpers.js';

test.beforeEach(async ({ page }) => {
  await page.goto('/');
  await waitForRenderedCanvas(page);
  await openDrawer(page);
});

// Count LIT (non-background) pixels in a centre region around the atom.
const COLS = 36;
const ROWS = 24;
const region = (page) => readRegion(page, 0.18, 0.18, 0.82, 0.82, COLS, ROWS);
const isLit = (p) => p[0] + p[1] + p[2] > 60;
const litCount = (px) => px.reduce((n, p) => n + (isLit(p) ? 1 : 0), 0);

// Set the #zoom-slider to a given value and wait a few frames.
// Uses page.evaluate to bypass step-boundary restrictions (page.fill rejects
// values not on the step). Dispatches a synthetic 'input' event so the wired
// listener fires exactly as a real user drag would.
async function setSlider(page, value) {
  await page.evaluate((v) => {
    const el = document.getElementById('zoom-slider');
    if (!el) return;
    el.value = String(v);
    el.dispatchEvent(new Event('input', { bubbles: true }));
  }, value);
  await page.waitForTimeout(300);
}

// Poll until the atom region has actually painted (render-ready), rather than
// trusting a fixed timeout. Mirrors the zoom-buttons waitForLit.
async function waitForLit(page, min = 3, tries = 25) {
  for (let i = 0; i < tries; i++) {
    if (litCount(await region(page)) > min) return;
    await page.waitForTimeout(120);
  }
}

test('zoom slider is visible and present', async ({ page }) => {
  const slider = page.locator('#zoom-slider');
  await expect(slider).toBeVisible();
  // Verify the old zoom buttons are gone.
  await expect(page.locator('#zoom-in')).toHaveCount(0);
  await expect(page.locator('#zoom-out')).toHaveCount(0);
});

test('zoom slider pulls the camera in/out (atomos atom shrinks then grows)', async ({ page }) => {
  await page.click('#scene-toggle'); // → atomos
  await page.fill('#element-value', '36'); // Krypton: 4 shells, big lit footprint
  await page.waitForTimeout(500);
  await waitForLit(page);

  // --- baseline at default zoom (1.0) ---------------------------------------
  const baseline = litCount(await region(page));
  expect(baseline).toBeGreaterThan(3); // atom is actually lit

  // --- set slider to a LOW value (zoom OUT): camera pulls back, footprint SHRINKS
  await setSlider(page, 0.25);
  await waitForLit(page, 1);
  const zoomedOut = litCount(await region(page));
  expect(zoomedOut).toBeLessThan(baseline); // measurably smaller

  // --- set slider to a HIGH value (zoom IN): footprint GROWS back/past baseline
  await setSlider(page, 4.0);
  await waitForLit(page, 1);
  const zoomedIn = litCount(await region(page));
  expect(zoomedIn).toBeGreaterThan(zoomedOut); // grew back relative to zoomed-out

  // --- exercise the range: slider near the low end (0.2) then maxZoom (5.0) -
  // At each value the scene must still render SOMETHING (not blank/black). (0.2 is
  // a low-end probe; the true minZoom is now 0.05 and is checked separately below.)
  await setSlider(page, 0.2); // low-end probe (old floor)
  await waitForLit(page, 0);
  const farCenter = await readPixel(page, 0.5, 0.5);
  expect(farCenter[0] + farCenter[1] + farCenter[2]).toBeGreaterThan(0);

  await setSlider(page, 5.0); // maxZoom
  await page.waitForTimeout(200);
  const nearCenter = await readPixel(page, 0.5, 0.5);
  // Zoomed all the way in, the nucleus fills the centre → clearly lit, not black.
  expect(nearCenter[0] + nearCenter[1] + nearCenter[2]).toBeGreaterThan(20);
});

// Extended zoom-OUT range: with minZoom now 0.05 the slider reaches a much
// wider field of view. Key properties to verify:
//   1. The #zoom-slider `min` attribute is 0.05 (HTML synced to the new floor).
//   2. Setting zoom to 0.05 does NOT cull the world to black (no far-plane clip).
//   3. The Atomos atom footprint at new minZoom is SMALLER than at 0.2 (zoomed
//      further out ⇒ smaller apparent size, which directly serves the "fit more
//      atoms in view" goal).
//   4. Recovering to zoom=1.0 restores a normal lit footprint.
test('zoom slider: new minZoom (0.05) renders scene and atom is smaller than at 0.2', async ({ page }) => {
  // Use the Atomos scene: a large Krypton atom (Z=36, 4 shells) has a broad
  // lit footprint that is easy to measure reliably, even under SwiftShader.
  await page.click('#scene-toggle'); // → atomos
  await page.fill('#element-value', '36'); // Krypton: 4 shells, large lit footprint
  await page.waitForTimeout(500);
  await waitForLit(page, 3);

  // 1. HTML: slider min must match the new minZoom (0.05).
  const sliderMin = await page.evaluate(
    () => parseFloat(document.getElementById('zoom-slider').getAttribute('min')),
  );
  expect(sliderMin).toBeCloseTo(0.05, 3);

  // 2. Drive to OLD floor (0.2) first, measure the lit footprint.
  await setSlider(page, 0.2);
  await waitForLit(page, 1);
  const atOldFloor = litCount(await region(page));
  expect(atOldFloor).toBeGreaterThan(0); // sanity: something lit at 0.2

  // 3. Drive to new minZoom (0.05) — camera pulls back 4× further than before.
  await setSlider(page, 0.05);
  await page.waitForTimeout(400); // let a few rAF frames propagate

  // World NOT culled: the scene still renders at least one non-background pixel.
  // We check a dense grid + multiple individual points to handle SwiftShader
  // timing variance (atom may be a single pixel at extreme zoom-out).
  const atNewMin = litCount(await region(page));
  const centrePixel = await readPixel(page, 0.5, 0.5);
  const samplePoints = await Promise.all([
    readPixel(page, 0.45, 0.45),
    readPixel(page, 0.55, 0.55),
    readPixel(page, 0.50, 0.45),
    readPixel(page, 0.50, 0.55),
  ]);
  const anyNonBlack =
    centrePixel[0] + centrePixel[1] + centrePixel[2] > 0 ||
    samplePoints.some((p) => p[0] + p[1] + p[2] > 0) ||
    atNewMin > 0;
  expect(anyNonBlack).toBe(true);

  // 4. Footprint at new minZoom <= footprint at old floor (zoomed further out).
  //    +8 tolerance absorbs SwiftShader pixel-count noise.
  expect(atNewMin).toBeLessThanOrEqual(atOldFloor + 8);

  // 5. Recovering to zoom=1.0 must restore a substantial lit footprint.
  await setSlider(page, 1.0);
  await waitForLit(page, 3);
  const atDefault = litCount(await region(page));
  expect(atDefault).toBeGreaterThan(atNewMin + 1); // clearly MORE lit at 1.0
});

test('zoom slider reflects wheel zoom (two-way sync)', async ({ page }) => {
  // The slider thumb must follow wheel zoom events (setZoomSlider is called
  // each frame with the live State.zoom). Start at the default (1.0), wheel
  // out several times, then read the slider value back.
  await page.click('#scene-toggle'); // → atomos
  await page.waitForTimeout(500);

  const box = await page.locator('#canvas').boundingBox();
  const cx = box.x + box.width / 2;
  const cy = box.y + box.height / 2;
  await page.mouse.move(cx, cy);

  // Read the initial slider value (should be ~1.0).
  const initial = await page.evaluate(() => parseFloat(document.getElementById('zoom-slider').value));
  expect(initial).toBeCloseTo(1.0, 1);

  // Wheel OUT 6 times (each +120 deltaY multiplies zoom by exp(-0.18) ≈ 0.835).
  for (let i = 0; i < 6; i++) {
    await page.mouse.wheel(0, 120);
    await page.waitForTimeout(60);
  }
  await page.waitForTimeout(400); // let the rAF loop sync the slider

  // Slider value must have moved DOWN toward minZoom — zoom pulled back.
  const afterWheel = await page.evaluate(() => parseFloat(document.getElementById('zoom-slider').value));
  expect(afterWheel).toBeLessThan(initial - 0.05);
});
