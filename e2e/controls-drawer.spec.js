// Control bar present/entrance/Add-Clear + panel-drawer toggle/icon specs.
// Split out of the original e2e/world.spec.js (behaviour-frozen reorganisation).
import { test } from '@playwright/test';
import {
  expect, waitForRenderedCanvas, openDrawer,
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

// molecule-builder M4 (TEST A): the control bar (#controls) restyle must NOT
// break or remove any control. This is a regression guard for the M4 glassy
// restyle + anime.js animations: every control the prior milestones rely on
// (scene toggle, 2D view, element selector, shear, bond, add, clear) must remain
// present and visible. Stays green before AND after M4 ships the restyle.
test('control bar: all controls are present and visible', async ({ page }) => {
  for (const sel of [
    '#scene-toggle',
    '#view-2d',
    '#element-value',
    '#shear-btn',
    '#bond-btn',
    '#add-btn',
    '#clear-btn',
  ]) {
    await expect(page.locator(sel)).toBeVisible();
  }
});

// The control bar lives in an anime.js-driven LEFT DRAWER that opens on the
// #panel-toggle click (it no longer auto-animates on boot — the animated
// hidden→shown→hidden entrance is covered deterministically by the two
// "controls: …" drawer specs below). This spec is the regression guard that the
// entrance, driven from a genuinely CLOSED state, brings the drawer to FULLY
// visible AND leaves it interactive (not pointer-blocked or transformed
// off-screen). beforeEach already opened the drawer for this spec, so we first
// close it to reach a real closed start state, then re-open and assert the
// transition deterministically (hidden → settled-visible → a control is usable).
test('control bar: drawer entrance settles fully visible and interactive', async ({ page }) => {
  const opacity = () =>
    page.evaluate(
      () => parseFloat(getComputedStyle(document.getElementById('controls')).opacity),
    );

  // Close the (beforeEach-opened) drawer to reach a real CLOSED state.
  await page.click('#panel-toggle');
  await expect.poll(opacity, { timeout: 4000 }).toBeLessThan(0.1);

  // Re-open: the anime.js entrance must bring it from hidden to FULLY visible.
  await page.click('#panel-toggle');
  await expect.poll(opacity, { timeout: 4000 }).toBeGreaterThan(0.95);
  await expect(page.locator('#controls')).toBeVisible();

  // And it is interactive once settled: a control is clickable (the entrance
  // does not leave the bar pointer-blocked or transformed off-screen).
  await page.click('#scene-toggle');
  await expect(page.locator('#scene-title')).toHaveText('atomos', { timeout: 4000 });
});

// molecule-builder M4 (TEST C): the glassy restyle + anime.js animations on the
// Add/Clear buttons must NOT break their wiring to the builder model. Reach the
// Builder scene (three #scene-toggle clicks), set the element selector to H,
// click #add-btn twice (two H spawn stepped within bond range per the M2 add
// wiring), and assert via window.__builder.getBonds() that a bond formed. Then
// #clear-btn empties the model. Regression guard: stays green before AND after
// the M4 restyle/animation lands.
test('control bar: Add/Clear still work after restyle (builder integration)', async ({ page }) => {
  await expect(page.locator('#scene-toggle')).toBeVisible();
  await page.click('#scene-toggle'); // → atomos
  await page.click('#scene-toggle'); // → molecule
  await page.click('#scene-toggle'); // → builder
  await page.waitForTimeout(700); // let the builder scene boot + render
  await page.waitForFunction(() => !!window.__builder, null, { timeout: 6000 });

  // Select Hydrogen (Z=1) so the Add button spawns H atoms.
  await page.fill('#element-value', '1');

  // Click Add twice: two H spawn stepped within bond range (M2 add wiring), so
  // the auto-bond model forms a bond between them.
  await page.click('#add-btn');
  await page.waitForTimeout(150);
  await page.click('#add-btn');
  await page.waitForTimeout(300);

  // The restyled/animated Add button still drives the builder model: a bond formed.
  await expect
    .poll(async () => page.evaluate(() => window.__builder.getBonds().length), {
      timeout: 4000,
    })
    .toBeGreaterThanOrEqual(1);

  // The restyled/animated Clear button still empties the model.
  await page.click('#clear-btn');
  await expect
    .poll(async () => page.evaluate(() => window.__builder.getBonds().length), {
      timeout: 4000,
    })
    .toBe(0);
});

// controls-drawer M2: the #controls bar becomes a LEFT DRAWER that is HIDDEN on
// load (translated off-screen left / opacity≈0) and opened by a new #panel-toggle
// icon button rendered top-left, BELOW #scene-title. Clicking #panel-toggle slides
// #controls IN from the left (anime.js); clicking again slides it OUT. This is a
// pure DOM/anime.js overlay change, so we assert via getBoundingClientRect() +
// computed opacity on #controls and the #panel-toggle button — NOT canvas pixels.
// RED until M2 adds #panel-toggle to index.html and wires the slide toggle through
// Controls/Main (today there is no #panel-toggle, and #controls is opacity:0 with
// no slide-in toggle → the toggle click + the visible assertion fail).
//
// panelState() helper (read inside page.evaluate on #controls):
//   x       = getBoundingClientRect().left  (drawer's left edge, px from viewport left)
//   right   = getBoundingClientRect().right (drawer's right edge; <=0 ⇒ fully off-screen left)
//   opacity = parseFloat(getComputedStyle(el).opacity)
//   visible = NOT hidden, where HIDDEN = right <= 0 (off-screen left) OR opacity <= 0.05.
// SHOWN criteria  : x >= 0 && x < 80 (snug at the left margin) && opacity >= 0.9.
// HIDDEN criteria : right <= 0 (off-screen left) OR opacity <= 0.05.
// Wait approach   : after each toggle click, waitForFunction polls panelState every
//   frame until it settles to the expected open/closed state (opacity>=0.9 && x>=0
//   to open; right<=0 || opacity<=0.05 to close), with a 2500ms timeout that
//   comfortably covers the ~600ms anime.js slide; a try/catch lets the explicit
//   assertions report the real RED failure rather than a bare timeout.
async function panelState(page) {
  return page.evaluate(() => {
    const el = document.querySelector('#controls');
    if (!el) return { x: NaN, right: NaN, opacity: NaN, visible: false };
    const r = el.getBoundingClientRect();
    const opacity = parseFloat(getComputedStyle(el).opacity);
    const hidden = r.right <= 0 || opacity <= 0.05;
    return { x: r.left, right: r.right, opacity, visible: !hidden };
  });
}

// Poll panelState until `pred` holds (drawer settled) or the timeout elapses.
// Swallows the timeout so the caller's explicit expect() reports the real failure.
async function waitForPanel(page, pred, timeout = 2500) {
  try {
    await page.waitForFunction(
      (predSrc) => {
        const fn = new Function('s', `return (${predSrc})(s);`);
        const el = document.querySelector('#controls');
        if (!el) return false;
        const r = el.getBoundingClientRect();
        const opacity = parseFloat(getComputedStyle(el).opacity);
        const hidden = r.right <= 0 || opacity <= 0.05;
        return fn({ x: r.left, right: r.right, opacity, visible: !hidden });
      },
      pred.toString(),
      { timeout, polling: 100 },
    );
  } catch {
    // settle timed out — let the explicit assertion below surface the RED state.
  }
}

// SPEC A — controls: panel is a left drawer toggled by the panel icon.
test('controls: panel is a left drawer toggled by the panel icon', async ({ page }) => {
  // 1. The #panel-toggle icon button exists and is visible top-left.
  await expect(page.locator('#panel-toggle')).toBeVisible();

  // 2. On load #controls is HIDDEN: off-screen left (right<=0) OR opacity≈0.
  const initial = await panelState(page);
  expect(initial.right <= 0 || initial.opacity <= 0.05).toBe(true);

  // 3. Click #panel-toggle → the drawer slides IN from the left (anime.js).
  await page.click('#panel-toggle');
  await waitForPanel(page, (s) => s.opacity >= 0.9 && s.x >= 0);
  const opened = await panelState(page);
  expect(opened.x).toBeGreaterThanOrEqual(0);
  expect(opened.x).toBeLessThan(80); // snug at the left margin
  expect(opened.opacity).toBeGreaterThanOrEqual(0.9);

  // 4. Click #panel-toggle again → the drawer slides back OUT (hidden again).
  await page.click('#panel-toggle');
  await waitForPanel(page, (s) => s.right <= 0 || s.opacity <= 0.05);
  const closed = await panelState(page);
  expect(closed.right <= 0 || closed.opacity <= 0.05).toBe(true);
});

// SPEC B — controls: drawer icon is below the scene title; open drawer keeps
// controls working. Asserts the icon geometry (below #scene-title, near the left)
// and that, once the drawer is open, an existing control still functions through
// it. RED until M2 adds #panel-toggle.
test('controls: drawer icon is below the scene title; open drawer keeps controls working', async ({ page }) => {
  await expect(page.locator('#panel-toggle')).toBeVisible();

  // 1. Geometry: #panel-toggle sits BELOW #scene-title's bottom and near the left.
  const geo = await page.evaluate(() => {
    const toggle = document.querySelector('#panel-toggle');
    const title = document.querySelector('#scene-title');
    const t = toggle.getBoundingClientRect();
    const s = title.getBoundingClientRect();
    return { toggleTop: t.top, toggleLeft: t.left, titleBottom: s.bottom };
  });
  expect(geo.toggleTop).toBeGreaterThanOrEqual(geo.titleBottom - 4); // below title (small tol)
  expect(geo.toggleLeft).toBeLessThan(80); // near the left edge

  // 2. Open the drawer, then confirm an existing control still works through it.
  await page.click('#panel-toggle');
  await waitForPanel(page, (s) => s.opacity >= 0.9 && s.x >= 0);
  const opened = await panelState(page);
  expect(opened.visible).toBe(true);

  // The #scene-toggle inside the open drawer still cycles scenes (title changes).
  const title = page.locator('#scene-title');
  await expect(title).toHaveText('Cube POC');
  await page.click('#scene-toggle');
  await expect(title).toHaveText('atomos', { timeout: 4000 });

  // NOTE: a closed-drawer-doesn't-block-canvas check (Builder drag / wheel zoom
  // with the drawer closed) is already covered by the existing pointer-drag and
  // zoom specs once M2 lands; we deliberately avoid a flaky synthetic-drag test here.
});
