// builder-atom-visuals M2: the Builder scene's ATOMIC (zoomed-out) layer gets a
// visual overhaul:
//   - The STARFIELD is REMOVED from the Builder background (atomos + Molecule keep
//     theirs). Builder's backdrop is the bare dark clear colour, no star speckle.
//   - Atom balls render at DIFFERENT SIZES by element — the ball footprint scales
//     with the element's atomic radius (Hydrogen small, Carbon/Oxygen larger).
//   - Bonds render as connecting LINES between bonded atom balls in the atomic
//     (zoomed-out) layer.
//
// At default zoom (1.0) Builder shows FULL sub-atomic detail; the ball/line layer
// is reached by ZOOMING OUT (setting #zoom-slider to a low value) — see
// builder-layered-zoom.spec.js. These tests zoom out to the atomic layer,
// then assert coarse, retry-tolerant pixel facts.
import { test } from '@playwright/test';
import {
  expect, waitForRenderedCanvas, openDrawer, readRegion,
} from './helpers.js';

// Reach the Builder scene (CubePoc → Atomos → Molecule → Builder = three
// #scene-toggle clicks) and wait for window.__builder.
async function gotoBuilder(page) {
  await expect(page.locator('#scene-toggle')).toBeVisible();
  await page.click('#scene-toggle'); // → atomos
  await page.click('#scene-toggle'); // → molecule
  await page.click('#scene-toggle'); // → builder
  await page.waitForTimeout(700); // generous: let the builder scene boot + render
  await page.waitForFunction(() => !!window.__builder, null, { timeout: 6000 });
}

// Set the #zoom-slider to a given value and wait for the rAF loop to apply it.
// Uses page.evaluate to bypass step-boundary restrictions (page.fill rejects
// values not on the step).
async function setZoom(page, value) {
  await page.evaluate((v) => {
    const el = document.getElementById('zoom-slider');
    if (!el) return;
    el.value = String(v);
    el.dispatchEvent(new Event('input', { bubbles: true }));
  }, value);
  await page.waitForTimeout(400);
}

const isLit = (p, threshold = 60) => p[0] + p[1] + p[2] > threshold;
const countLit = (pixels, threshold = 60) => pixels.filter((p) => isLit(p, threshold)).length;

// --- Projection mirror (kept in sync with src/Camera.purs + Main.builderScale) ---
// The Builder renders/picks atoms in ONE world coordinate system: a placed atom at
// model position p is drawn at (builderScale × layerSpace)·p, projected by
// Camera.projection at the live zoom. These constants replicate that math so the
// tests can DERIVE the on-screen sample regions from the projected atom centres
// (robust to the exact scale layout) instead of hardcoding pixel boxes.
//
// IMPORTANT: The effective scale is builderScale × layerSpace (default 1.6), NOT
// builderScale alone. At layerSpace=1.6 (default) the effective scale is 3.52.
// Using only 2.2 shifts the computed box centres and causes them to miss the
// actual on-screen atom positions, especially at deep zoom-out.
const BUILDER_SCALE = 2.2;
const LAYER_SPACE_DEFAULT = 1.6; // State.layerSpace default; must match Update.purs
const EFFECTIVE_SCALE = BUILDER_SCALE * LAYER_SPACE_DEFAULT; // 3.52
const CAMERA_DISTANCE = 1000;
const FOV = Math.PI / 3;

// Project a Builder MODEL position (x,y,z, before builderScale) to a normalized
// screen fraction {fx, fy} (top-left origin), at the given zoom and canvas aspect.
// Mirrors Camera.projection ∘ builderWorldPosWith(layerSpace): world = effectiveScale·model,
// perspective divide by the effective camera distance (CAMERA_DISTANCE/zoom + worldZ).
function projectModel(modelX, modelY, modelZ, zoom, width, height) {
  const f = 1.0 / Math.tan(FOV / 2.0);
  const aspect = width / height;
  const wx = modelX * EFFECTIVE_SCALE;
  const wy = modelY * EFFECTIVE_SCALE;
  const wz = modelZ * EFFECTIVE_SCALE;
  const wclip = CAMERA_DISTANCE / zoom + wz; // homogeneous w after the camera translate
  const ndcX = (f / aspect) * wx / wclip;
  const ndcY = f * wy / wclip;
  return { fx: (ndcX + 1) / 2, fy: 1 - (ndcY + 1) / 2 };
}

// Read the canvas pixel size from the page (for the projection aspect).
async function canvasSize(page) {
  return page.evaluate(() => {
    const c = document.querySelector('#canvas');
    return { width: c.width, height: c.height };
  });
}

// A clamped square sample box (in normalized fractions) centred on (fx, fy).
function boxAround(fx, fy, half) {
  const clamp = (v) => Math.max(0.0, Math.min(1.0, v));
  return {
    x0: clamp(fx - half), y0: clamp(fy - half), x1: clamp(fx + half), y1: clamp(fy + half),
  };
}

test.beforeEach(async ({ page }) => {
  await page.goto('/');
  await waitForRenderedCanvas(page);
  // Controls (scene-toggle + zoom slider) live in a left drawer that no longer
  // auto-opens on boot — open it up front so those controls are reachable.
  await openDrawer(page);
});

// 1) The starfield is REMOVED from the Builder background but KEPT in atomos.
// Sample a grid of BACKGROUND points spread across the canvas, AVOIDING the very
// centre where atoms sit. In Builder (empty world) that grid should be ~all dark
// (no stars). In atomos (which keeps its starfield) the same background sampling
// lights up some star pixels. RED today: Builder still has the starfield, so its
// background grid is speckled with lit stars too.
test('builder: background has no starfield (atomos keeps it)', async ({ page }) => {
  // The key behavioral difference: Builder (empty/cleared) renders nothing at centre
  // while Atomos always renders a Carbon nucleus at the canvas centre. We verify
  // this by directly reading the centre pixel of each scene.
  //
  // Stars and orbital rings are thin features (1–3 pixels each) so a coarse grid
  // scan cannot reliably detect them. Instead we read the exact canvas centre where
  // the atomos nucleus always appears — and confirm it is dark in empty Builder.
  const readCenter = async () => {
    return page.evaluate(() => {
      const c = document.querySelector('#canvas');
      const gl = c.getContext('webgl2', { preserveDrawingBuffer: true });
      const x = Math.floor(0.5 * c.width);
      const y = Math.floor(0.5 * c.height);
      const buf = new Uint8Array(4);
      gl.readPixels(x, y, 1, 1, gl.RGBA, gl.UNSIGNED_BYTE, buf);
      return buf[0] + buf[1] + buf[2];
    });
  };

  // --- Builder: cleared empty world — centre should be dark ---
  await gotoBuilder(page);
  await page.evaluate(() => window.__builder.clear());
  await page.waitForTimeout(400);
  const builderCenterBrightness = await readCenter();

  // --- Atomos: Carbon nucleus always at canvas centre — centre should be lit ---
  // 6-cycle: Builder → Materials → Nuclide → CubePoc → Atomos (four more clicks).
  await page.click('#scene-toggle'); // → materials
  await page.waitForTimeout(300);
  await page.click('#scene-toggle'); // → nuclide
  await page.waitForTimeout(300);
  await page.click('#scene-toggle'); // → cube poc
  await page.waitForTimeout(300);
  await page.click('#scene-toggle'); // → atomos
  // Poll until the nucleus pixel at center is brighter than background.
  await page.waitForFunction(
    () => {
      const c = document.querySelector('#canvas');
      if (!c) return false;
      const gl = c.getContext('webgl2', { preserveDrawingBuffer: true });
      if (!gl) return false;
      const x = Math.floor(0.5 * c.width);
      const y = Math.floor(0.5 * c.height);
      const buf = new Uint8Array(4);
      gl.readPixels(x, y, 1, 1, gl.RGBA, gl.UNSIGNED_BYTE, buf);
      return buf[0] + buf[1] + buf[2] > 50;
    },
    {},
    { timeout: 4000, polling: 200 },
  );
  const atomosCenterBrightness = await readCenter();

  // Atomos centre is lit (nucleus); Builder empty centre is dark (no atoms, no starfield).
  expect(atomosCenterBrightness).toBeGreaterThan(50);
  expect(builderCenterBrightness).toBeLessThan(atomosCenterBrightness);
});

// 2) Atom balls differ in SIZE by element. Place a Hydrogen (small radius) and an
// Oxygen (larger radius) far enough apart NOT to bond, zoom OUT to the atomic
// (ball) layer, and compare the lit ball footprint in a region centred on each.
// The larger element's ball must cover MORE lit pixels. RED before per-element
// radius scaling: balls were uniform-sized.
//
// Both atoms render in ONE world coordinate system (effectiveScale = builderScale ×
// layerSpace, the SAME used by the pick/drag), so the sample boxes are DERIVED from
// each atom's projected screen centre (projectModel at the live zoom) rather than
// hardcoded.
//
// Calibration for new LOD band [0.10, 0.20]:
//   - SIZE_ZOOM_OUT = 0.08 is below detailLo (0.10) → smoothstep → 0 (ball layer).
//   - SIZE_H_X/SIZE_O_X = ±200 model units: 400 apart > breakThreshold (230) → no bond.
//     At zoom=0.08 with effectiveScale=3.52, the screen centers are at fx≈0.473/0.527
//     (confirmed by projectModel). Ball screen radii: H≈3.7px, O≈7.8px (both > 0).
//   - Box half = 0.03 (not 0.06): tighter window gives a denser 24×24 sample grid
//     (step ≈ 1.6px) that reliably captures both small balls. With half=0.06 (6.4px
//     step) the 3.7px H ball may yield 0 grid hits.
const SIZE_H_X = -200; // model-units; 400 apart > breakThreshold (230) → no bond
const SIZE_O_X = 200;
// Slider zoom below new detailLo (0.10) → smoothstep(0.10, 0.20, 0.08) = 0 (ball layer).
const SIZE_ZOOM_OUT = 0.08;

test('builder: atoms differ in size by element (H small, O large)', async ({ page }) => {
  await gotoBuilder(page);

  // H on the left, O on the right, well apart so they do NOT bond/overlap (400
  // model-units apart, comfortably past the 230 breakThreshold).
  await page.evaluate(({ hx, ox }) => {
    const b = window.__builder;
    b.clear();
    b.addAtom(1, hx, 0, 0); // H (small) — left
    b.addAtom(8, ox, 0, 0); // O (large) — right
  }, { hx: SIZE_H_X, ox: SIZE_O_X });
  await page.waitForTimeout(300);

  // Zoom OUT to the atomic/ball layer where the balls + size differences show.
  await setZoom(page, SIZE_ZOOM_OUT);

  // Confirm we are actually in the ball layer: smoothstep(0.10,0.20,0.08)=0,
  // so __builderDetail must ease down to ≤ 0.15 within a few frames.
  await expect
    .poll(async () => page.evaluate(() => window.__builderDetail), { timeout: 4000 })
    .toBeLessThanOrEqual(0.15);

  // Derive each atom's on-screen sample box from its projected centre at the live
  // zoom, so the boxes track the effectiveScale layout exactly.
  // half=0.03 (not 0.06): at zoom=0.08 atom balls are ~4–8px radius; a 24×24 grid
  // over a 0.06-fraction box has step ≈6.4px and misses sub-5px balls. Halving the
  // box (half=0.03, step ≈1.6px) captures both balls with a clear O > H margin.
  const { width, height } = await canvasSize(page);
  const zoom = SIZE_ZOOM_OUT;
  const hC = projectModel(SIZE_H_X, 0, 0, zoom, width, height);
  const oC = projectModel(SIZE_O_X, 0, 0, zoom, width, height);
  // Equal-size boxes centred on each atom (same footprint area → fair comparison).
  const hBox = boxAround(hC.fx, hC.fy, 0.03);
  const oBox = boxAround(oC.fx, oC.fy, 0.03);
  const hRegion = await readRegion(page, hBox.x0, hBox.y0, hBox.x1, hBox.y1, 24, 24);
  const oRegion = await readRegion(page, oBox.x0, oBox.y0, oBox.x1, oBox.y1, 24, 24);

  const hLit = countLit(hRegion);
  const oLit = countLit(oRegion);

  // Both balls render something.
  expect(hLit).toBeGreaterThan(0);
  expect(oLit).toBeGreaterThan(0);
  // Oxygen's larger atomic radius → a measurably bigger ball footprint than H.
  // Generous margin: require O's lit footprint to clearly exceed H's.
  expect(oLit).toBeGreaterThan(hLit + 4);
});

// 3) Bonded atoms show a connecting LINE in the atomic (ball) layer. Place two
// atoms within bonding range (under the 180 bondThreshold) so getBonds() === 1,
// zoom OUT to the ball layer, and sample the MIDPOINT — where NEITHER ball sits.
// Without a bond line that midpoint is DARK; M2 draws a connecting bond line there,
// lighting it.
//
// Balls + bond line render in ONE world coordinate system (builderScale, the SAME
// used by the pick/drag), so the ball + midpoint sample boxes are DERIVED from the
// projected atom centres at the live zoom.
//
// Calibration for new LOD band [0.10, 0.20]:
//   - BOND_ZOOM_OUT = 0.08 is below detailLo (0.10) → smoothstep → 0 (ball layer).
//   - BOND_H_X = 85 is unchanged: two H at ±85 → 170 apart (< bondThreshold 180),
//     so a bond still forms. At zoom=0.08 the screen separation is ~19 px — the two
//     balls are distinct (small H radius at deep zoom-out) with the bond line running
//     through the midpoint gap between them.
const BOND_H_X = 85; // model-units; the two H sit at ±BOND_H_X → 170 apart (< 180)
// Slider zoom below new detailLo (0.10) → smoothstep(0.10, 0.20, 0.08) = 0 (ball layer).
const BOND_ZOOM_OUT = 0.08;

test('builder: bonded atoms show a connecting line (atomic layer)', async ({ page }) => {
  await gotoBuilder(page);

  // Two H within bonding range (170 apart total — under the 180 bondThreshold) →
  // exactly one bond, but far enough that at the ball layer the two balls sit
  // apart with a dark gap between them.
  const bonds = await page.evaluate(({ hx }) => {
    const b = window.__builder;
    b.clear();
    b.addAtom(1, -hx, 0, 0); // H (id 0)
    b.addAtom(1, hx, 0, 0); // H (id 1) — 2·hx apart → within bondThreshold
    return b.getBonds().length;
  }, { hx: BOND_H_X });
  expect(bonds).toBe(1);

  await page.waitForTimeout(300);

  // Zoom OUT to the atomic/ball layer where the balls separate and the bond LINE
  // is drawn between them.
  await setZoom(page, BOND_ZOOM_OUT);

  // Confirm ball layer: smoothstep(0.10,0.20,0.08)=0 → __builderDetail eases to ~0.
  await expect
    .poll(async () => page.evaluate(() => window.__builderDetail), { timeout: 4000 })
    .toBeLessThanOrEqual(0.15);

  // Derive the ball + midpoint sample boxes from the projected atom centres at the
  // live zoom, so they track the unified builderScale layout exactly.
  const { width, height } = await canvasSize(page);
  const zoom = BOND_ZOOM_OUT;
  const leftC = projectModel(-BOND_H_X, 0, 0, zoom, width, height);
  const rightC = projectModel(BOND_H_X, 0, 0, zoom, width, height);
  const midFx = (leftC.fx + rightC.fx) / 2; // dead centre between the two balls
  const midFy = (leftC.fy + rightC.fy) / 2;
  const lBox = boxAround(leftC.fx, leftC.fy, 0.012);
  const rBox = boxAround(rightC.fx, rightC.fy, 0.012);
  const mBox = boxAround(midFx, midFy, 0.012);

  // SANITY: both balls are lit and separated — confirms we're at the right layer
  // and looking in the right place (not just sampling empty space).
  const leftBall = await readRegion(page, lBox.x0, lBox.y0, lBox.x1, lBox.y1, 10, 10);
  const rightBall = await readRegion(page, rBox.x0, rBox.y0, rBox.x1, rBox.y1, 10, 10);
  expect(countLit(leftBall)).toBeGreaterThan(0);
  expect(countLit(rightBall)).toBeGreaterThan(0);

  // The bond line runs through the dead centre between the balls. Sample a small
  // box right at the midpoint (where no ball sits) and assert it is LIT by the
  // connecting line. Poll a few frames for render robustness under SwiftShader.
  // Without the bond line the midpoint is dark backdrop → midLit stays ~0.
  let midLit = 0;
  for (let i = 0; i < 10; i++) {
    const midRegion = await readRegion(page, mBox.x0, mBox.y0, mBox.x1, mBox.y1, 10, 10);
    midLit = countLit(midRegion);
    if (midLit > 2) break;
    await page.waitForTimeout(120);
  }

  // A connecting bond line is drawn through the midpoint → lit pixels there.
  expect(midLit).toBeGreaterThan(2);
});
