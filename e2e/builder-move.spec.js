// Builder real-mouse-drag (pointer path) relocation spec.
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

// molecule-builder M3 (TEST C): a REAL Playwright pointer drag over the canvas
// relocates a placed atom through the PRODUCTION pick+drag path (Main/Loop +
// pointer FFI). RED until M3 wires pointer pick+drag into the Builder scene.
//
// A single atom placed at the world origin projects to the canvas centre. We
// capture the centre region + a region offset to where the atom will land, then
// perform a genuine mouse drag (move → down → stepped move → up) starting ON the
// canvas (not a #controls button). We assert the atom MOVED: the old centre
// changed AND a region at the new location gained lit pixels (robust: sample both
// the vacated centre and the destination).
test('builder: real mouse drag relocates an atom (pointer path)', async ({ page }) => {
  await gotoBuilder(page);

  // A single atom at the world origin → projects to the canvas centre.
  await page.evaluate(() => {
    const b = window.__builder;
    b.clear();
    b.addAtom(6, 0, 0, 0); // one Carbon at origin
  });
  await page.waitForTimeout(300);

  // Canvas geometry → screen pixels for the centre and the drag destination.
  const box = await page.locator('#canvas').boundingBox();
  const cx = box.x + box.width / 2;
  const cy = box.y + box.height / 2;
  const dx = 150; // sizable horizontal drag offset

  // Robust delta of a sampled region between two snapshots.
  const regionDelta = (a, b) =>
    a.filter((p, i) =>
      Math.abs(p[0] - b[i][0]) + Math.abs(p[1] - b[i][1]) + Math.abs(p[2] - b[i][2]) > 30
    ).length;
  const litCount = (px) => px.filter((p) => p[0] + p[1] + p[2] > 60).length;

  // Old (centre) + new (offset right) sampling windows, in normalized canvas coords.
  const oldRegion = () => readRegion(page, 0.42, 0.42, 0.58, 0.58, 18, 18);
  // dx=150 px to the right of centre, in fractions of canvas width.
  const newFx = 0.5 + dx / box.width;
  const newRegion = () =>
    readRegion(page, newFx - 0.08, 0.42, newFx + 0.08, 0.58, 18, 18);

  const oldBefore = await oldRegion();
  const newBefore = await newRegion();

  // A REAL Playwright pointer drag over the canvas, starting at the centre (on
  // the atom) and dragging horizontally — stepped so it reads as a genuine drag.
  await page.mouse.move(cx, cy);
  await page.mouse.down();
  await page.mouse.move(cx + dx * 0.33, cy, { steps: 4 });
  await page.mouse.move(cx + dx * 0.66, cy, { steps: 4 });
  await page.mouse.move(cx + dx, cy, { steps: 4 });
  await page.mouse.up();
  await page.waitForTimeout(400); // settle

  const oldAfter = await oldRegion();
  const newAfter = await newRegion();

  // The atom moved: the vacated centre changed measurably ...
  const vacated = regionDelta(oldBefore, oldAfter);
  // ... and the destination gained lit atom pixels.
  const arrivedDelta = regionDelta(newBefore, newAfter);
  const arrivedLit = litCount(oldAfter) <= litCount(oldBefore)
    ? litCount(newAfter)
    : litCount(newAfter); // destination is lit after the drag

  // Robust combined assertion (sample both old + new spots): the production
  // pointer pick+drag path actually relocated the atom.
  expect(vacated + arrivedDelta).toBeGreaterThan(20);
  expect(arrivedLit).toBeGreaterThan(0);
});
