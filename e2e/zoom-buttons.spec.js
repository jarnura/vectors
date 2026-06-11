// On-screen zoom buttons (#zoom-in / #zoom-out) drive the camera zoom across all
// scenes, mirroring the mouse-wheel zoom. They push a fixed synthetic wheel delta
// (Camera.buttonZoomDelta) into the SAME zoom channel as the wheel, so clicking
// #zoom-out pulls the camera back (rendered content shrinks) and #zoom-in pushes
// it in (content grows). This spec exercises that end-to-end on the atomos atom
// and checks the clamps still render something at both extremes.
//
// Signature: over a centre region around the atom we count LIT (non-background)
// pixels. The atom's lit footprint shrinks as the camera pulls back and grows as
// it pushes in. Rings/nucleus are the dominant static lit structure (electrons
// are a few sweeping dots), so this aggregate is robust to electron animation
// under SwiftShader. We reuse the shared region sampler + drawer/poll helpers.
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

// Poll until the atom region has actually painted (render-ready), rather than
// trusting a fixed timeout — robust to slow cold-start frames under full-suite
// load. Mirrors the 2D-toggle spec's waitForLit.
async function waitForLit(page, min = 3, tries = 25) {
  for (let i = 0; i < tries; i++) {
    if (litCount(await region(page)) > min) return;
    await page.waitForTimeout(120);
  }
}

// Click a control N times, settling a frame or two between clicks.
async function clickN(page, selector, n) {
  for (let i = 0; i < n; i++) {
    await page.click(selector);
    await page.waitForTimeout(60);
  }
  await page.waitForTimeout(300);
}

test('zoom buttons pull the camera in/out (atomos atom shrinks then grows)', async ({ page }) => {
  await page.click('#scene-toggle'); // → atomos
  await page.fill('#element-value', '36'); // Krypton: 4 shells, big lit footprint
  await page.waitForTimeout(500);
  await waitForLit(page);

  await expect(page.locator('#zoom-in')).toBeVisible();
  await expect(page.locator('#zoom-out')).toBeVisible();

  // --- baseline -------------------------------------------------------------
  const baseline = litCount(await region(page));
  expect(baseline).toBeGreaterThan(3); // atom is actually lit

  // --- zoom OUT several times: camera pulls back, lit footprint SHRINKS ------
  await clickN(page, '#zoom-out', 5);
  await waitForLit(page, 1);
  const zoomedOut = litCount(await region(page));
  expect(zoomedOut).toBeLessThan(baseline); // measurably smaller

  // --- zoom IN more times than the zoom-outs: footprint GROWS back/past ------
  await clickN(page, '#zoom-in', 8);
  await waitForLit(page, 1);
  const zoomedIn = litCount(await region(page));
  expect(zoomedIn).toBeGreaterThan(zoomedOut); // grew back relative to zoomed-out

  // --- exercise the clamps: many zoom-outs then many zoom-ins ---------------
  // At each extreme the scene must still render SOMETHING (not blank/black).
  await clickN(page, '#zoom-out', 30); // hammer minZoom
  await waitForLit(page, 0);
  const farCenter = await readPixel(page, 0.5, 0.5);
  expect(farCenter[0] + farCenter[1] + farCenter[2]).toBeGreaterThan(0);

  await clickN(page, '#zoom-in', 30); // hammer maxZoom
  await page.waitForTimeout(200);
  const nearCenter = await readPixel(page, 0.5, 0.5);
  // Zoomed all the way in, the nucleus fills the centre → clearly lit, not black.
  expect(nearCenter[0] + nearCenter[1] + nearCenter[2]).toBeGreaterThan(20);
});
