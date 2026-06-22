// E2E spec for the Materials scene cards gallery (M3).
//
// The #materials-cards gallery is data-driven from Lattice.structures (one
// card per entry). At M3 there are 2 entries:
//   Diamond (index 0, 64 atoms — 2×2×2 cubic supercell, sp³)
//   Graphene (index 1, 32 atoms — 4×4 honeycomb supercell, sp²)
// Adding a 3rd material requires zero code change — only a new Lattice.structures entry.
//
// Assertions:
//   1. Navigating to Materials shows #materials-cards with exactly 2
//      `.material-card` elements (one per Lattice.structures entry).
//   2. Clicking the Graphene card (index 1) loads Graphene: getAtoms().length == 32.
//   3. Clicking the Diamond card (index 0) loads Diamond: getAtoms().length == 64.
//   4. The two loaded structures are geometrically distinct (different atom counts).
//   5. #materials-cards is hidden when leaving the Materials scene.
//
// Seam: window.__builder (same as Builder + Materials scenes):
//   getAtoms()       → [{ id, z, pos }]
//   loadStructure(i) → replace world with structure i
//
// Timing: retries:2 (playwright.config.js), waitForTimeout for anime.js.
import { test } from '@playwright/test';
import {
  expect,
  waitForRenderedCanvas,
  openDrawer,
  gotoMaterials,
} from './helpers.js';

test.beforeEach(async ({ page }) => {
  await page.goto('/');
  await waitForRenderedCanvas(page);
  await openDrawer(page);
});

// ────────────────────────────────────────────────────────────────────────────
// GATE 1 — Cards gallery appears in the Materials scene
// ────────────────────────────────────────────────────────────────────────────

// 1a. Navigating to Materials renders exactly 2 .material-card elements
//     (data-driven: one per Lattice.structures entry — Diamond + Graphene).
test('materials-cards: Materials scene shows 2 cards (one per Lattice.structures entry)', async ({ page }) => {
  await gotoMaterials(page);

  // Give anime.js stagger entrance time to create the DOM nodes.
  await page.waitForTimeout(600);

  const cardCount = await page.evaluate(() => {
    const container = document.getElementById('materials-cards');
    if (!container) return -1;
    return container.querySelectorAll('.material-card').length;
  });

  // 2 structures in Lattice.structures at M3: Diamond + Graphene.
  // This assertion is data-driven: if a 3rd entry is added to Lattice.structures,
  // the card count becomes 3 without any FFI/PureScript code change.
  expect(cardCount).toBe(2);
});

// 1b. #materials-cards container is visible (display != 'none') in Materials.
test('materials-cards: #materials-cards container is visible in Materials scene', async ({ page }) => {
  await gotoMaterials(page);
  await page.waitForTimeout(500);

  const visible = await page.evaluate(() => {
    const el = document.getElementById('materials-cards');
    if (!el) return false;
    return getComputedStyle(el).display !== 'none';
  });

  expect(visible).toBe(true);
});

// 1c. Each card shows the correct name text (data-driven from the registry).
test('materials-cards: cards show correct names (Diamond and Graphene)', async ({ page }) => {
  await gotoMaterials(page);
  await page.waitForTimeout(600);

  const names = await page.evaluate(() => {
    return Array.from(
      document.querySelectorAll('.material-card .material-card-name')
    ).map((el) => el.textContent);
  });

  expect(names).toContain('Diamond');
  expect(names).toContain('Graphene');
});

// ────────────────────────────────────────────────────────────────────────────
// GATE 2 — Card clicks load the correct structure (via loadStructure seam)
// ────────────────────────────────────────────────────────────────────────────

// 2a. Clicking the Graphene card (index 1) loads Graphene: 32 atoms (4×4 supercell).
test('materials-cards: clicking Graphene card loads Graphene (32 atoms)', async ({ page }) => {
  await gotoMaterials(page);
  await page.waitForTimeout(700);

  // Click the card at data-index="1" (Graphene, the second entry).
  await page.click('.material-card[data-index="1"]');

  // Wait for the loadStructure call and eager re-render.
  await page.waitForTimeout(400);

  const atomCount = await page.evaluate(() => window.__builder.getAtoms().length);
  expect(atomCount).toBe(32);
});

// 2b. Clicking the Diamond card (index 0) loads Diamond: 64 atoms (2×2×2 supercell).
test('materials-cards: clicking Diamond card loads Diamond (64 atoms)', async ({ page }) => {
  await gotoMaterials(page);
  await page.waitForTimeout(700);

  // First switch to Graphene so there is a visible state change.
  await page.click('.material-card[data-index="1"]');
  await page.waitForTimeout(300);

  // Now click Diamond.
  await page.click('.material-card[data-index="0"]');
  await page.waitForTimeout(400);

  const atomCount = await page.evaluate(() => window.__builder.getAtoms().length);
  expect(atomCount).toBe(64);
});

// 2c. Diamond and Graphene atom counts differ (structures are geometrically distinct).
// Diamond is a 2×2×2 cubic supercell (64 atoms); Graphene is a 4×4 honeycomb (32 atoms).
test('materials-cards: Diamond (64) and Graphene (32) atom counts are distinct', async ({ page }) => {
  await gotoMaterials(page);
  await page.waitForTimeout(700);

  const result = await page.evaluate(async () => {
    // Click Diamond card (index 0).
    const d = document.querySelector('.material-card[data-index="0"]');
    if (d) d.click();
    await new Promise((r) => setTimeout(r, 200));
    const diamondCount = window.__builder.getAtoms().length;

    // Click Graphene card (index 1).
    const g = document.querySelector('.material-card[data-index="1"]');
    if (g) g.click();
    await new Promise((r) => setTimeout(r, 200));
    const grapheneCount = window.__builder.getAtoms().length;

    return { diamondCount, grapheneCount };
  });

  expect(result.diamondCount).toBe(64);
  expect(result.grapheneCount).toBe(32);
  expect(result.diamondCount).not.toBe(result.grapheneCount);
  // Both are proper supercells (well above old 5/18 baseline).
  expect(result.diamondCount).toBeGreaterThan(30);
  expect(result.grapheneCount).toBeGreaterThan(25);
});

// ────────────────────────────────────────────────────────────────────────────
// GATE 3 — Gallery hidden when leaving Materials
// ────────────────────────────────────────────────────────────────────────────

// 3a. Navigating away from Materials hides the cards gallery.
test('materials-cards: #materials-cards hidden after leaving Materials scene', async ({ page }) => {
  await gotoMaterials(page);
  await page.waitForTimeout(500);

  // Confirm visible first.
  const visibleBefore = await page.evaluate(() => {
    const el = document.getElementById('materials-cards');
    return el ? getComputedStyle(el).display !== 'none' : false;
  });
  expect(visibleBefore).toBe(true);

  // Navigate away (one more scene-toggle brings us to CubePoc).
  await page.click('#scene-toggle');
  await page.waitForTimeout(400);

  const visibleAfter = await page.evaluate(() => {
    const el = document.getElementById('materials-cards');
    return el ? getComputedStyle(el).display !== 'none' : false;
  });
  expect(visibleAfter).toBe(false);
});

// 3b. Returning to Materials again shows the gallery (toggle show/hide correctly).
test('materials-cards: gallery re-appears when returning to Materials', async ({ page }) => {
  await gotoMaterials(page);
  await page.waitForTimeout(500);

  // Navigate away to CubePoc.
  await page.click('#scene-toggle');
  await page.waitForTimeout(300);

  // Confirm hidden.
  const hiddenMid = await page.evaluate(() => {
    const el = document.getElementById('materials-cards');
    return el ? getComputedStyle(el).display !== 'none' : true;
  });
  expect(hiddenMid).toBe(false);

  // Cycle back through the 5-scene cycle to Materials (4 clicks from CubePoc).
  for (let i = 0; i < 4; i++) {
    await page.click('#scene-toggle');
    await page.waitForTimeout(200);
  }
  await page.waitForTimeout(500);

  const visibleAfterReturn = await page.evaluate(() => {
    const el = document.getElementById('materials-cards');
    return el ? getComputedStyle(el).display !== 'none' : false;
  });
  expect(visibleAfterReturn).toBe(true);
});
