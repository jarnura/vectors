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
// is reached by ZOOMING OUT (clicking #zoom-out several times) — see
// builder-layered-zoom.spec.js. These tests zoom out ~10x to the atomic layer,
// then assert coarse, retry-tolerant pixel facts.
//
// RED until M2 (1) drops the starfield from the Builder render, (2) sizes atom
// balls by atomic radius, and (3) draws bond lines between balls. Today the
// Builder still paints the shared starfield, balls are uniform, and the bond
// midpoint is dark — so the Builder halves of these tests fail.
import { test } from '@playwright/test';
import {
  expect, waitForRenderedCanvas, openDrawer, readRegion,
} from './helpers.js';

// Reach the Builder scene (CubePoc → Atomos → Molecule → Builder = three
// #scene-toggle clicks) and wait for window.__builder. Mirrors world.spec.js /
// builder-layered-zoom.spec.js gotoBuilder.
async function gotoBuilder(page) {
  await expect(page.locator('#scene-toggle')).toBeVisible();
  await page.click('#scene-toggle'); // → atomos
  await page.click('#scene-toggle'); // → molecule
  await page.click('#scene-toggle'); // → builder
  await page.waitForTimeout(700); // generous: let the builder scene boot + render
  await page.waitForFunction(() => !!window.__builder, null, { timeout: 6000 });
}

// Click a control N times, settling between clicks so per-frame easing advances.
// Mirrors builder-layered-zoom.spec.js clickN.
async function clickN(page, selector, n) {
  for (let i = 0; i < n; i++) {
    await page.click(selector);
    await page.waitForTimeout(100);
  }
  await page.waitForTimeout(400);
}

const isLit = (p, threshold = 60) => p[0] + p[1] + p[2] > threshold;
const countLit = (pixels, threshold = 60) => pixels.filter((p) => isLit(p, threshold)).length;

// --- Projection mirror (kept in sync with src/Camera.purs + Main.builderScale) ---
// The Builder renders/picks atoms in ONE world coordinate system: a placed atom at
// model position p is drawn at builderScale·p, projected by Camera.projection at the
// live zoom. These constants replicate that math so the tests can DERIVE the on-screen
// sample regions from the projected atom centres (robust to the exact builderScale /
// zoom layout) instead of hardcoding pixel boxes.
const BUILDER_SCALE = 2.2;
const CAMERA_DISTANCE = 1000;
const FOV = Math.PI / 3;
const ZOOM_SENSITIVITY = 0.0015;
const BUTTON_ZOOM_DELTA = 120;
const MIN_ZOOM = 0.2;
const MAX_ZOOM = 5.0;

// The live camera zoom after `clicks` presses of #zoom-out from the default 1.0
// (each click pushes +BUTTON_ZOOM_DELTA → zoom · exp(−delta·sens), clamped).
function zoomAfterZoomOut(clicks) {
  let z = 1.0;
  for (let i = 0; i < clicks; i += 1) {
    z = Math.max(MIN_ZOOM, Math.min(MAX_ZOOM, z * Math.exp(-(BUTTON_ZOOM_DELTA * ZOOM_SENSITIVITY))));
  }
  return z;
}

// Project a Builder MODEL position (x,y,z, before builderScale) to a normalized
// screen fraction {fx, fy} (top-left origin), at the given zoom and canvas aspect.
// Mirrors Camera.projection ∘ builderPlace: world = builderScale·model, perspective
// divide by the effective camera distance (CAMERA_DISTANCE/zoom + worldZ).
function projectModel(modelX, modelY, modelZ, zoom, width, height) {
  const f = 1.0 / Math.tan(FOV / 2.0);
  const aspect = width / height;
  const wx = modelX * BUILDER_SCALE;
  const wy = modelY * BUILDER_SCALE;
  const wz = modelZ * BUILDER_SCALE;
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
  // Controls (scene-toggle + zoom buttons) live in a left drawer that no longer
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
  // Sample four background bands well away from the centre (left/right strips and
  // top/bottom strips), avoiding the central square where atoms render.
  const STAR = 50; // a star speckle is brighter than the near-black clear colour
  const sampleBackgroundLit = async () => {
    const left = await readRegion(page, 0.02, 0.05, 0.22, 0.95, 16, 24);
    const right = await readRegion(page, 0.78, 0.05, 0.98, 0.95, 16, 24);
    const top = await readRegion(page, 0.22, 0.02, 0.78, 0.20, 24, 8);
    const bottom = await readRegion(page, 0.22, 0.80, 0.78, 0.98, 24, 8);
    return (
      countLit(left, STAR)
      + countLit(right, STAR)
      + countLit(top, STAR)
      + countLit(bottom, STAR)
    );
  };

  // --- Builder background: empty world, no stars ---------------------------
  await gotoBuilder(page);
  await page.evaluate(() => window.__builder.clear());
  await page.waitForTimeout(300);
  const builderStars = await sampleBackgroundLit(page);

  // --- atomos background: KEEPS its starfield -----------------------------
  // Cycle back around to atomos (Builder → Materials → CubePoc → Atomos = three more
  // clicks; the 5-cycle adds Materials between Builder and CubePoc).
  await page.click('#scene-toggle'); // → materials
  await page.click('#scene-toggle'); // → cube poc
  await page.click('#scene-toggle'); // → atomos
  await page.waitForTimeout(400);
  const atomosStars = await sampleBackgroundLit(page);

  // atomos keeps its starfield → its background has lit star pixels.
  expect(atomosStars).toBeGreaterThan(2);
  // Builder dropped the starfield → its background is (near) all dark, and in any
  // case has far fewer lit background pixels than atomos.
  expect(builderStars).toBeLessThan(2);
  expect(builderStars).toBeLessThan(atomosStars);
});

// 2) Atom balls differ in SIZE by element. Place a Hydrogen (small radius) and an
// Oxygen (larger radius) far enough apart NOT to bond, zoom OUT to the atomic
// (ball) layer, and compare the lit ball footprint in a region centred on each.
// The larger element's ball must cover MORE lit pixels. RED before per-element
// radius scaling: balls were uniform-sized.
//
// Both atoms render in ONE world coordinate system (builderScale, the SAME used by
// the pick/drag), so the sample boxes are DERIVED from each atom's projected screen
// centre (projectModel at the live zoom) rather than hardcoded — robust to the
// unified builderScale layout where the atoms sit nearer the centre.
const SIZE_H_X = -200; // model-units; 400 apart > breakThreshold (230) → no bond
const SIZE_O_X = 200;
const SIZE_ZOOM_OUT_CLICKS = 5; // → zoom ≈ 0.41 → detail ≈ 0 (full ball layer)

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
  await clickN(page, '#zoom-out', SIZE_ZOOM_OUT_CLICKS);

  // Derive each atom's on-screen sample box from its projected centre at the live
  // zoom, so the boxes track the unified builderScale layout exactly.
  const { width, height } = await canvasSize(page);
  const zoom = zoomAfterZoomOut(SIZE_ZOOM_OUT_CLICKS);
  const hC = projectModel(SIZE_H_X, 0, 0, zoom, width, height);
  const oC = projectModel(SIZE_O_X, 0, 0, zoom, width, height);
  // Equal-size boxes centred on each atom (same footprint area → fair comparison).
  const hBox = boxAround(hC.fx, hC.fy, 0.06);
  const oBox = boxAround(oC.fx, oC.fy, 0.06);
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
// zoom OUT to the ball layer where the two balls read as SEPARATED clusters with
// empty backdrop between them, and sample the MIDPOINT — where NEITHER ball sits.
// Without a bond line that midpoint is DARK; M2 draws a connecting bond line there,
// lighting it.
//
// Balls + bond line render in ONE world coordinate system (builderScale, the SAME
// used by the pick/drag), so the ball + midpoint sample boxes are DERIVED from the
// projected atom centres at the live zoom. Coords/zoom are calibrated (170
// model-units apart, 4 zoom-out clicks) so the two balls sit apart with a dark gap
// whose centre is bridged only by the bond line.
const BOND_H_X = 85; // model-units; the two H sit at ±BOND_H_X → 170 apart (< 180)
const BOND_ZOOM_OUT_CLICKS = 4; // → zoom ≈ 0.49 → detail ≈ 0 (ball/line layer)

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
  // (M2) is drawn between them.
  await clickN(page, '#zoom-out', BOND_ZOOM_OUT_CLICKS);

  // Derive the ball + midpoint sample boxes from the projected atom centres at the
  // live zoom, so they track the unified builderScale layout exactly.
  const { width, height } = await canvasSize(page);
  const zoom = zoomAfterZoomOut(BOND_ZOOM_OUT_CLICKS);
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
