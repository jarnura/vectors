// Shared helpers for canvas-pixel verification of the WebGL2 scene.
// The renderer uses `preserveDrawingBuffer: false` by default, so we read pixels
// directly from the GL context inside the page rather than via toDataURL.
import { expect } from '@playwright/test';

export const CANVAS = '#canvas';

// Open the left controls drawer (clicking the #panel-toggle icon) and wait until
// the panel has slid in (opacity ~1, snug at the left margin). The controls now
// live in an anime.js-driven left drawer that no longer auto-opens on boot, so
// tests that interact with controls open it first via this helper. Idempotent:
// if already open it leaves it open (a no-op slide).
export async function openDrawer(page) {
  await page.waitForSelector('#panel-toggle', { state: 'visible' });
  const isOpen = async () =>
    page.evaluate(() => {
      const el = document.querySelector('#controls');
      if (!el) return false;
      const r = el.getBoundingClientRect();
      const opacity = parseFloat(getComputedStyle(el).opacity);
      return opacity >= 0.9 && r.left >= 0 && r.right > 0;
    });
  if (await isOpen()) return;
  await page.click('#panel-toggle');
  await page.waitForFunction(() => {
    const el = document.querySelector('#controls');
    if (!el) return false;
    const r = el.getBoundingClientRect();
    const opacity = parseFloat(getComputedStyle(el).opacity);
    return opacity >= 0.9 && r.left >= 0 && r.left < 80;
  }, undefined, { timeout: 2500, polling: 100 });
}

// Reach the Builder scene (CubePoc → Atomos → Molecule → Builder = three
// #scene-toggle clicks) and wait for window.__builder.
export async function gotoBuilder(page) {
  await expect(page.locator('#scene-toggle')).toBeVisible();
  await page.click('#scene-toggle'); // → atomos
  await page.click('#scene-toggle'); // → molecule
  await page.click('#scene-toggle'); // → builder
  await page.waitForTimeout(700); // generous: let the builder scene boot + render
  await page.waitForFunction(() => !!window.__builder, null, { timeout: 6000 });
}

// Wait until the WebGL2 canvas exists, is sized, and has rendered ≥1 frame.
export async function waitForRenderedCanvas(page) {
  await page.waitForSelector(CANVAS, { state: 'attached' });
  await page.waitForFunction(() => {
    const c = document.querySelector('#canvas');
    return !!c && c.width > 0 && c.height > 0;
  });
  // Give the rAF loop a few frames to draw.
  await page.waitForTimeout(300);
}

// Read a single pixel [r,g,b,a] (0–255) at normalized (fx, fy) where
// (0,0) is top-left and (1,1) is bottom-right of the canvas.
export async function readPixel(page, fx, fy) {
  return page.evaluate(({ fx, fy }) => {
    const c = document.querySelector('#canvas');
    const gl = c.getContext('webgl2', { preserveDrawingBuffer: true });
    const x = Math.floor(fx * c.width);
    // WebGL readPixels origin is bottom-left → flip Y.
    const y = Math.floor((1 - fy) * c.height);
    const buf = new Uint8Array(4);
    gl.readPixels(x, y, 1, 1, gl.RGBA, gl.UNSIGNED_BYTE, buf);
    return Array.from(buf);
  }, { fx, fy });
}

// Sample a horizontal band of N pixels at vertical position fy; returns array of [r,g,b,a].
export async function readRow(page, fy, samples = 16) {
  return page.evaluate(({ fy, samples }) => {
    const c = document.querySelector('#canvas');
    const gl = c.getContext('webgl2', { preserveDrawingBuffer: true });
    const y = Math.floor((1 - fy) * c.height);
    const out = [];
    for (let i = 0; i < samples; i++) {
      const x = Math.floor((i / (samples - 1)) * (c.width - 1));
      const buf = new Uint8Array(4);
      gl.readPixels(x, y, 1, 1, gl.RGBA, gl.UNSIGNED_BYTE, buf);
      out.push(Array.from(buf));
    }
    return out;
  }, { fy, samples });
}

// Sample a rectangular region of the canvas as a grid of rows×cols pixels.
// fx0..fx1 / fy0..fy1 are normalized bounds (top-left origin). Returns [r,g,b,a][].
export async function readRegion(page, fx0, fy0, fx1, fy1, cols = 24, rows = 12) {
  return page.evaluate(({ fx0, fy0, fx1, fy1, cols, rows }) => {
    const c = document.querySelector('#canvas');
    const gl = c.getContext('webgl2', { preserveDrawingBuffer: true });
    const out = [];
    for (let r = 0; r < rows; r++) {
      const fy = fy0 + (r / (rows - 1)) * (fy1 - fy0);
      const y = Math.floor((1 - fy) * (c.height - 1));
      for (let i = 0; i < cols; i++) {
        const fx = fx0 + (i / (cols - 1)) * (fx1 - fx0);
        const x = Math.floor(fx * (c.width - 1));
        const buf = new Uint8Array(4);
        gl.readPixels(x, y, 1, 1, gl.RGBA, gl.UNSIGNED_BYTE, buf);
        out.push(Array.from(buf));
      }
    }
    return out;
  }, { fx0, fy0, fx1, fy1, cols, rows });
}

// True if two colors differ by more than `tol` in any RGB channel.
export function colorsDiffer(a, b, tol = 16) {
  return Math.abs(a[0] - b[0]) > tol || Math.abs(a[1] - b[1]) > tol || Math.abs(a[2] - b[2]) > tol;
}

// Count how many distinct color buckets appear in a list of pixels (coarse).
export function distinctColors(pixels, bucket = 24) {
  const set = new Set();
  for (const p of pixels) set.add([0, 1, 2].map((i) => Math.round(p[i] / bucket)).join(','));
  return set.size;
}

export { expect };
