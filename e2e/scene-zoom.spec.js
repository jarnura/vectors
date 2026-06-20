// Mouse-wheel zoom specs (out shrinks / in restores; builder drag after zoom).
// Split out of the original e2e/world.spec.js (behaviour-frozen reorganisation).
import { test } from '@playwright/test';
import {
  expect, waitForRenderedCanvas, openDrawer, readRegion, gotoBuilder,
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

// camera-zoom M2 (TEST A): a mouse-wheel over the canvas zooms the live camera —
// scrolling DOWN (positive deltaY) pulls the camera farther back so on-screen
// content SHRINKS; scrolling UP (negative deltaY) brings it closer so content
// ENLARGES, recovering the original size. M1 already shipped the pure zoom math
// (Camera.projection zoom / applyZoomStep / min..maxZoom); M2 must wire a canvas
// 'wheel' listener → State.zoom → live re-projection. RED until M2 lands: today
// perspectiveProjection is fixed at Camera.projection 1.0 with no wheel handler,
// so the wheel does nothing and the lit-pixel count is unchanged (no decrease).
//
// Signature: in the Molecule scene (clear lit central content — two nuclei + the
// shared pair — over a near-black backdrop, no ground/sky to confuse the count)
// we count LIT pixels (sum RGB > threshold) in a CENTERED region (fx/fy 0.30–0.70).
// Zooming OUT shrinks that lit mass toward the centre → fewer lit pixels in the
// fixed window; zooming back IN restores it. We assert a clear decrease on zoom-out
// (with a robust SwiftShader margin) and reversibility (zoomed-in count clearly
// exceeds the zoomed-out count).
test('zoom: wheel out shrinks the scene, wheel in restores it', async ({ page }) => {
  // Molecule scene: two clicks (CubePoc → Atomos → Molecule). Clear lit central
  // content over a dark backdrop — ideal for a centred lit-pixel count.
  await expect(page.locator('#scene-toggle')).toBeVisible();
  await page.click('#scene-toggle'); // → atomos
  await page.click('#scene-toggle'); // → molecule
  await page.waitForTimeout(700);    // let the H₂ scene render + animate

  // Centred sampling window (fx/fy 0.30–0.70). Count LIT pixels (sum RGB > 60).
  const COLS = 40, ROWS = 28;
  const region = () => readRegion(page, 0.30, 0.30, 0.70, 0.70, COLS, ROWS);
  const isLit = (p) => p[0] + p[1] + p[2] > 60;
  const litCount = (px) => px.filter(isLit).length;

  // Poll until the centre region has actually painted (render-ready) rather than
  // trusting a fixed timeout — robust to slow cold-start frames under full-suite
  // load (mirrors the molecule/atomos waitForLit helpers).
  const waitForLit = async (min = 3, tries = 25) => {
    let last = 0;
    for (let i = 0; i < tries; i++) {
      last = litCount(await region());
      if (last > min) return last;
      await page.waitForTimeout(120);
    }
    return last;
  };
  await waitForLit();

  // --- baseline -------------------------------------------------------------
  const baseline = litCount(await region());
  expect(baseline).toBeGreaterThan(3); // central content is actually lit

  // Hover the mouse over the canvas centre so the wheel events land on it.
  const box = await page.locator('#canvas').boundingBox();
  const cx = box.x + box.width / 2;
  const cy = box.y + box.height / 2;
  await page.mouse.move(cx, cy);

  // --- zoom OUT: scroll DOWN (positive deltaY) several times -----------------
  // Positive deltaY = wheel down = zoom OUT = camera pulls back = content SHRINKS.
  for (let i = 0; i < 5; i++) {
    await page.mouse.wheel(0, 120);
    await page.waitForTimeout(60);
  }
  await page.waitForTimeout(300); // let a few frames render the zoomed-out camera

  const zoomedOut = litCount(await region());

  // MUST-PASS (RED today): the camera pulled back, so the same centred window now
  // carries CLEARLY FEWER lit pixels than the baseline. A robust margin guards
  // against SwiftShader noise — a no-op wheel (today) leaves the count unchanged
  // and FAILS here (this is the definitive RED assertion).
  expect(zoomedOut).toBeLessThan(baseline - 6);

  // --- zoom back IN: scroll UP (negative deltaY) the same number of times -----
  for (let i = 0; i < 5; i++) {
    await page.mouse.wheel(0, -120);
    await page.waitForTimeout(60);
  }
  await page.waitForTimeout(300); // let the zoomed-in camera render

  const zoomedIn = litCount(await region());

  // Reversibility: zooming back in RECOVERS the lit mass toward the baseline — the
  // zoomed-in count clearly exceeds the zoomed-out count. (We assert recovery vs
  // the shrunk state rather than exact baseline equality, robust to a frame of
  // animation drift in the breathing shared pair.)
  expect(zoomedIn).toBeGreaterThan(zoomedOut + 6);
});

// camera-zoom M2 (TEST B): the Builder pointer pick+drag must use the LIVE zoomed
// camera (perspectiveProjection at State.zoom) for BOTH the pick AND the unproject,
// not a fixed zoom-1 matrix. installBuilderPick unprojects the cursor at the picked
// atom's depth, so the WORLD distance a given screen drag moves the atom scales like
// 1/zoom. A zoom-blind (zoom=1) unproject would move the atom a much SMALLER world
// distance under a zoomed-out camera. This test is a DRAG-DISTANCE DISCRIMINATOR: it
// is engineered so the zoom-AWARE unproject carries the dragged atom far enough to
// BOND with its neighbour, while a zoom-BLIND (zoom=1) unproject falls far short and
// forms NO bond. It therefore genuinely FAILS if installBuilderPick ignored zoom.
//
// Projection geometry (1280×720 viewport, fov π/3, cameraDistance 1000,
// builderScale 2.2). A world point at z=0 projects to a screen-x pixel offset from
// centre of:  px = (W/2)·(f/aspect)·zoom/1000 · (model_x·builderScale)
//   ⇒ px = 1.37179 · zoom · model_x   ⇒   model_x = px / (1.37179 · zoom).
// So the model-x moved per dragged screen pixel is:
//   • zoom 0.2 (full wheel-out, minZoom): 3.6449 model-x / px  (zoom-AWARE reach)
//   • zoom 1.0 (broken, zoom-blind):      0.7290 model-x / px  (zoom-BLIND reach)
//
// Layout (model space, the coords addAtom takes):
//   • A at the ORIGIN (0,0,0) → projects to the canvas CENTRE at ANY zoom, so a
//     pointer-down at the canvas centre always picks A (zoom-invariant pick anchor).
//   • B at (Dx, 0, 0) with Dx = 440 → 440 > breakThreshold(230) ⇒ A,B start UNBONDED.
//   We then drag A RIGHT by Sx = 120 screen px (canvas centre → centre+120):
//   • zoom-AWARE: A → model x ≈ 120·3.6449 = 437.4 ⇒ |440−437.4| = 2.6 < bondThreshold
//     (180) ⇒ A and B BOND  ⇒ getBonds() == 1.
//   • zoom-BLIND: A → model x ≈ 120·0.7290 = 87.5 ⇒ |440−87.5| = 352.5 ≫ 180 ⇒ A
//     stays far from B ⇒ NO bond ⇒ getBonds() == 0.  (Margin 172 model-x.)
// At zoom 0.2, B projects to ≈121 px right of centre (> the 80px pickRadius), so the
// centre pointer-down unambiguously picks A, not B. Proven discriminating: with the
// pick hardcoded to Camera.projection 1.0 this fails (no bond); with the real
// zoom-aware impl it passes (bond forms).
test('zoom: builder drag still works after zooming', async ({ page }) => {
  await gotoBuilder(page);

  // A at the model origin (canvas centre at any zoom); B at model x = 440 — well
  // beyond breakThreshold(230), so the pair starts UNBONDED.
  const before = await page.evaluate(() => {
    const b = window.__builder;
    b.clear();
    b.addAtom(1, 0, 0, 0);   // H (id 0) — at the origin → projects to canvas centre
    b.addAtom(1, 440, 0, 0); // H (id 1) — model x 440 → far right; 440 apart → no bond
    return { bonds: b.getBonds().length, molecules: b.getMolecules().length };
  });
  // Sanity: out of range → no bond, two singleton molecules.
  expect(before.bonds).toBe(0);
  expect(before.molecules).toBe(2);

  // --- zoom OUT to minZoom (0.2) over the canvas -----------------------------
  // Ten +120 wheel ticks drive zoom to clampZoom(exp(-0.0015·120·K)); it reaches
  // minZoom 0.2 by K=9, so K=10 firmly clamps to 0.2 (the zoom-AWARE reach the
  // numbers above are computed for). Each tick = camera pulls farther back.
  const box = await page.locator('#canvas').boundingBox();
  const cx = box.x + box.width / 2;
  const cy = box.y + box.height / 2;
  await page.mouse.move(cx, cy);
  for (let i = 0; i < 10; i++) {
    await page.mouse.wheel(0, 120);
    await page.waitForTimeout(50);
  }
  await page.waitForTimeout(300); // let the zoomed-out (zoom=0.2) camera render

  // --- a REAL pointer drag over #canvas, under the zoom=0.2 camera -----------
  // Pointer-down at the canvas CENTRE picks A (at the origin → centre at any zoom).
  // Drag RIGHT by exactly Sx = 120 screen px. Through the zoom-AWARE unproject this
  // carries A by ≈437 model-x — right up against B (440) — so they BOND. A zoom-blind
  // (zoom=1) unproject would carry A only ≈87 model-x, leaving it ≈352 short → no bond.
  const Sx = 120;
  await page.mouse.move(cx, cy);
  await page.mouse.down();
  await page.mouse.move(cx + Sx * 0.33, cy, { steps: 4 });
  await page.mouse.move(cx + Sx * 0.66, cy, { steps: 4 });
  await page.mouse.move(cx + Sx, cy, { steps: 4 });
  await page.mouse.up();
  await page.waitForTimeout(400); // settle the live re-bond

  // DISCRIMINATING assertion: the zoom-AWARE unproject moved A the LARGE world
  // distance (∝ 1/zoom) needed to reach B, so they BONDED. A zoom-blind (zoom=1)
  // unproject would have moved A only ~1/5 as far — far short of B — and formed NO
  // bond. Asserting the bond formed (and the two singletons merged into one H₂
  // molecule) genuinely fails the zoom-blind pick path.
  const after = await page.evaluate(() => {
    const b = window.__builder;
    return { bonds: b.getBonds(), molecules: b.getMolecules() };
  });
  expect(after.bonds.length).toBe(1);
  // The single bond joins the two placed H (ids 0 and 1) → one 2-atom H₂ molecule.
  const ids = [after.bonds[0].a, after.bonds[0].b].sort((p, q) => p - q);
  expect(ids).toEqual([0, 1]);
  expect(after.molecules.length).toBe(1);
  const mol = after.molecules[0];
  expect(mol.ids.length).toBe(2);
  expect(mol.formula === 'H₂' || mol.formula === 'H2').toBe(true);
});
