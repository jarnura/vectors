// builder-atom-visuals M3: each placed Builder atom shows its atomic SYMBOL
// (H, C, O, …) as an HTML overlay LABEL — a `<div class="atom-label">` inside a
// `#atom-labels` container, positioned over the atom, text = the element symbol.
// The labels are present ONLY in the Builder scene (cleared/empty when you switch
// to another scene). These are DOM elements (NOT canvas pixels), so the tests
// assert via document queries — reliable under SwiftShader.
//
// RED until M3 (1) adds a #atom-labels overlay container to index.html, and (2)
// wires the Builder render/draw loop to sync one .atom-label per placed atom
// (symbol text, screen-positioned) and clear them when leaving the Builder scene.
// Today #atom-labels / .atom-label do not exist → these tests fail (count 0).
import { test } from '@playwright/test';
import { expect, waitForRenderedCanvas, openDrawer } from './helpers.js';

// Reach the Builder scene (CubePoc → Atomos → Molecule → Builder = three
// #scene-toggle clicks) and wait for window.__builder. Mirrors
// builder-atom-visuals.spec.js / world.spec.js gotoBuilder.
async function gotoBuilder(page) {
  await expect(page.locator('#scene-toggle')).toBeVisible();
  await page.click('#scene-toggle'); // → atomos
  await page.click('#scene-toggle'); // → molecule
  await page.click('#scene-toggle'); // → builder
  await page.waitForTimeout(700); // generous: let the builder scene boot + render
  await page.waitForFunction(() => !!window.__builder, null, { timeout: 6000 });
}

// The set of trimmed textContents of the currently-rendered atom labels.
async function labelTexts(page) {
  return page.$$eval('#atom-labels .atom-label', (els) =>
    els.map((el) => (el.textContent || '').trim()));
}

// Count of VISIBLE atom labels (rendered, not display:none / detached). A label is
// "visible" if it has a non-null offsetParent OR a non-zero client box — robust to
// the exact hide mechanism (display:none, removal, empty container).
async function visibleLabelCount(page) {
  return page.$$eval('#atom-labels .atom-label', (els) =>
    els.filter((el) => {
      const style = getComputedStyle(el);
      if (style.display === 'none' || style.visibility === 'hidden') return false;
      return el.offsetParent !== null || el.getClientRects().length > 0;
    }).length);
}

test.beforeEach(async ({ page }) => {
  await page.goto('/');
  await waitForRenderedCanvas(page);
  // Controls (scene-toggle) live in a left drawer that no longer auto-opens on
  // boot — open it up front so #scene-toggle is reachable.
  await openDrawer(page);
});

// 1) Each placed atom shows its element SYMBOL as an overlay label. Place H, C, O
// at distinct, far-apart positions (so they do not all merge into one molecule),
// let the draw loop sync the labels, then assert three .atom-label divs whose
// trimmed texts are exactly the set {H, C, O} (order-independent).
test('builder: each atom shows its element symbol label', async ({ page }) => {
  await gotoBuilder(page);

  // H (z=1), C (z=6), O (z=8) at distinct, well-separated positions — far apart
  // (≥400 model-units) so they do NOT all merge into a single molecule.
  await page.evaluate(() => {
    const b = window.__builder;
    b.clear();
    b.addAtom(1, -400, 0, 0); // H
    b.addAtom(6, 0, 0, 0); // C
    b.addAtom(8, 400, 0, 0); // O
  });

  // Let a few frames of the rAF draw loop sync the labels. Poll for robustness.
  let texts = [];
  for (let i = 0; i < 12; i += 1) {
    await page.waitForTimeout(200);
    texts = await labelTexts(page);
    if (texts.length === 3) break;
  }

  // Exactly one label per atom.
  await expect(page.locator('#atom-labels .atom-label')).toHaveCount(3);

  // The SET of label texts equals {H, C, O} (order-independent).
  expect(new Set(texts)).toEqual(new Set(['H', 'C', 'O']));
});

// 2) Atom labels are Builder-only: when you leave the Builder scene the visible
// labels are gone (container emptied, or all labels hidden / detached). Add a
// couple atoms (labels present), then click #scene-toggle to the next scene
// (Builder → CubePoc) and assert no visible atom labels remain.
test('builder: atom labels clear when leaving the Builder scene', async ({ page }) => {
  await gotoBuilder(page);

  await page.evaluate(() => {
    const b = window.__builder;
    b.clear();
    b.addAtom(1, -400, 0, 0); // H
    b.addAtom(8, 400, 0, 0); // O
  });

  // Labels present in the Builder scene (poll a few frames for the sync).
  let present = 0;
  for (let i = 0; i < 12; i += 1) {
    await page.waitForTimeout(200);
    present = await visibleLabelCount(page);
    if (present > 0) break;
  }
  expect(present).toBeGreaterThan(0);

  // Leave the Builder scene → the next scene (Builder → CubePoc).
  await page.click('#scene-toggle');

  // Let a few frames sync the cleared labels.
  let remaining = present;
  for (let i = 0; i < 12; i += 1) {
    await page.waitForTimeout(200);
    remaining = await visibleLabelCount(page);
    if (remaining === 0) break;
  }

  // No visible atom labels outside the Builder scene (hidden, detached, or empty).
  expect(remaining).toBe(0);
});

// 3) The label count TRACKS atoms as they are added/removed: clear → 0; add one H
// → 1 ("H"); add one O → 2; clear → 0. Exercises that the label overlay stays in
// sync with the live builder model each frame.
test('builder: label count tracks atoms added/removed', async ({ page }) => {
  await gotoBuilder(page);

  // A small poller that waits for the visible label count to settle on `want`.
  const waitForLabelCount = async (want) => {
    let n = -1;
    for (let i = 0; i < 12; i += 1) {
      await page.waitForTimeout(200);
      n = await visibleLabelCount(page);
      if (n === want) break;
    }
    return n;
  };

  // clear() → 0 labels.
  await page.evaluate(() => window.__builder.clear());
  expect(await waitForLabelCount(0)).toBe(0);

  // add one H → 1 label with text "H".
  await page.evaluate(() => window.__builder.addAtom(1, 0, 0, 0));
  expect(await waitForLabelCount(1)).toBe(1);
  expect(new Set(await labelTexts(page))).toEqual(new Set(['H']));

  // add one O (far from H so they don't merge) → 2 labels.
  await page.evaluate(() => window.__builder.addAtom(8, 400, 0, 0));
  expect(await waitForLabelCount(2)).toBe(2);

  // clear() → 0 labels again.
  await page.evaluate(() => window.__builder.clear());
  expect(await waitForLabelCount(0)).toBe(0);
});
