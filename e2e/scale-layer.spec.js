// scale-layer M2: the new 5th "Scale" scene and its scale-layer ladder.
//
// The scene switch (#scene-toggle) now 5-cycles
//   CubePoc → Atomos → Molecule → Builder → Scale → CubePoc
// and the #scene-title banner reads "scale" in the Scale scene. The Scale scene
// adds a NEW overlay label #scale-layer (HTML text via anime.js scramble) that
// names the active scale layer — "Sub-atomic layer" (default) or "Atomic layer".
//
// Zoom is driven by the existing on-screen #zoom-out / #zoom-in buttons. In the
// Scale scene, zooming crosses a layer ladder: from Sub-atomic, repeated
// #zoom-out clicks cross UP to "Atomic layer" (and the camera zoom RESETS to a
// mid value on the cross, so a single extra #zoom-out does NOT immediately
// re-cross back down). From Atomic, repeated #zoom-in clicks cross DOWN to
// "Sub-atomic layer".
//
// RED until M2 wires the Scale scene's #scale-layer overlay + the zoom ladder
// into Main/FRP.Loop/index.html. We assert on overlay TEXT (auto-retrying past
// the ~700ms anime.js scramble via toHaveText timeouts), never pixels.
import { test } from '@playwright/test';
import { expect, waitForRenderedCanvas, openDrawer } from './helpers.js';

test.beforeEach(async ({ page }) => {
  await page.goto('/');
  await waitForRenderedCanvas(page);
  await openDrawer(page);
});

// Reach the Scale scene by clicking #scene-toggle until #scene-title reads
// "scale". The toggle 5-cycles, so from the initial Cube POC it is at most a
// few clicks away; we loop defensively with toHaveText waits between clicks.
async function gotoScale(page) {
  const title = page.locator('#scene-title');
  for (let i = 0; i < 5; i++) {
    if ((await title.textContent())?.trim() === 'scale') return;
    await page.click('#scene-toggle');
    // The title text scrambles via anime.js (~700ms); wait for it to settle
    // before re-reading so we don't over-click through a mid-scramble frame.
    await page.waitForTimeout(900);
  }
  await expect(title).toHaveText('scale', { timeout: 4000 });
}

// Click a control N times, settling a frame or two between clicks.
async function clickN(page, selector, n) {
  for (let i = 0; i < n; i++) {
    await page.click(selector);
    await page.waitForTimeout(100);
  }
  await page.waitForTimeout(200);
}

test('scale scene: reachable and starts in the sub-atomic layer', async ({ page }) => {
  await gotoScale(page);
  await expect(page.locator('#scene-title')).toHaveText('scale', { timeout: 4000 });

  const layer = page.locator('#scale-layer');
  await expect(layer).toBeVisible();
  await expect(layer).toHaveText('Sub-atomic layer', { timeout: 4000 });
});

test('scale scene: zoom-out crosses sub-atomic → atomic (with reset)', async ({ page }) => {
  await gotoScale(page);

  const layer = page.locator('#scale-layer');
  await expect(layer).toHaveText('Sub-atomic layer', { timeout: 4000 });

  // Repeated zoom-out clicks climb the ladder UP into the atomic layer.
  await clickN(page, '#zoom-out', 8);
  await expect(layer).toHaveText('Atomic layer', { timeout: 4000 });

  // The cross RESETS the camera zoom to a mid value, so a single extra zoom-out
  // does NOT immediately re-cross back down — we stay in the atomic layer.
  await page.click('#zoom-out');
  await page.waitForTimeout(300);
  await expect(layer).toHaveText('Atomic layer', { timeout: 4000 });
});

test('scale scene: zoom-in crosses atomic → sub-atomic', async ({ page }) => {
  await gotoScale(page);

  const layer = page.locator('#scale-layer');
  await expect(layer).toHaveText('Sub-atomic layer', { timeout: 4000 });

  // Climb UP to the atomic layer first.
  await clickN(page, '#zoom-out', 8);
  await expect(layer).toHaveText('Atomic layer', { timeout: 4000 });

  // Repeated zoom-in clicks descend the ladder DOWN to the sub-atomic layer.
  await clickN(page, '#zoom-in', 8);
  await expect(layer).toHaveText('Sub-atomic layer', { timeout: 4000 });
});
