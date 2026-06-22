// E2E spec for the Materials scene info panel (M4).
//
// The #materials-info panel is data-driven from Lattice.structureOf(selected).properties.
// It shows the selected structure's Name, Formula, Hybridization, and the entries
// in the `properties` array (Coordination, Bond, Note). It is visible only in the
// Materials scene and hidden everywhere else.
//
// The panel is also updated via the seam: window.__builder.loadStructure(i) must
// update both the rendered crystal AND the panel text (via the selectCallbackRef
// path in Main.purs that routes through selectStructureUi).
//
// Seam: window.__builder (same as Builder + Materials scenes):
//   getAtoms()       → [{ id, z, pos }]
//   loadStructure(i) → replace world with structure i AND update panel + highlight
//   setOrbit(yaw, pitch) → write orbit Ref
//   getOrbit()       → { yaw, pitch }
//
// Timing: retries:2 (playwright.config.js), waitForTimeout for anime.js.
import { test } from '@playwright/test';
import {
  expect,
  waitForRenderedCanvas,
  openDrawer,
  gotoMaterials,
  readRegion,
  distinctColors,
} from './helpers.js';

test.beforeEach(async ({ page }) => {
  await page.goto('/');
  await waitForRenderedCanvas(page);
  await openDrawer(page);
});

// ────────────────────────────────────────────────────────────────────────────
// GATE 1 — Panel visibility
// ────────────────────────────────────────────────────────────────────────────

// 1a. #materials-info is hidden in the initial Cube POC scene.
test('materials-panel: hidden in Cube POC scene', async ({ page }) => {
  const panel = page.locator('#materials-info');
  await expect(panel).toBeHidden();
});

// 1b. #materials-info is hidden in Atomos.
test('materials-panel: hidden in Atomos scene', async ({ page }) => {
  await page.click('#scene-toggle'); // → Atomos
  await page.waitForTimeout(300);
  const panel = page.locator('#materials-info');
  await expect(panel).toBeHidden();
});

// 1c. #materials-info is visible in the Materials scene.
test('materials-panel: visible in Materials scene', async ({ page }) => {
  await gotoMaterials(page);
  // Give the draw-loop scene-change handler time to fire showMaterialsPanel(true).
  await page.waitForTimeout(500);
  const panel = page.locator('#materials-info');
  await expect(panel).toBeVisible();
});

// 1d. #materials-info is hidden after leaving Materials.
test('materials-panel: hidden after leaving Materials scene', async ({ page }) => {
  await gotoMaterials(page);
  await page.waitForTimeout(500);

  // Confirm visible first.
  await expect(page.locator('#materials-info')).toBeVisible();

  // One more scene-toggle moves to CubePoc.
  await page.click('#scene-toggle'); // Materials → CubePoc
  await page.waitForTimeout(400);

  await expect(page.locator('#materials-info')).toBeHidden();
});

// ────────────────────────────────────────────────────────────────────────────
// GATE 2 — Panel content on default load (Diamond)
// ────────────────────────────────────────────────────────────────────────────

// 2a. On default load the panel contains Diamond's structure-specific text.
// Exact strings from Lattice.purs: hybridization "sp³", coordination value "4",
// bond value "C–C ~154 pm".
test('materials-panel: shows sp3 hybridization for Diamond on default load', async ({ page }) => {
  await gotoMaterials(page);
  await page.waitForTimeout(700);

  // Poll for the panel text to settle (anime.js stagger entrance).
  await expect
    .poll(
      async () => {
        const txt = (await page.locator('#materials-info').textContent()) ?? '';
        return txt.includes('Diamond');
      },
      { timeout: 5000 },
    )
    .toBe(true);

  const text = (await page.locator('#materials-info').textContent()) ?? '';
  // Diamond-specific: hybridization "sp³", coordination number "4",
  // bond length text "154 pm" (from "C–C ~154 pm"), and the name "Diamond".
  expect(text).toContain('Diamond');
  expect(text).toContain('sp³');
  expect(text).toContain('4');
  expect(text).toContain('154');
});

// 2b. Panel contains Diamond's coordination number (4).
test('materials-panel: shows coordination number for Diamond', async ({ page }) => {
  await gotoMaterials(page);
  await page.waitForTimeout(700);

  await expect
    .poll(
      async () => {
        const txt = (await page.locator('#materials-info').textContent()) ?? '';
        return txt.length > 5;
      },
      { timeout: 5000 },
    )
    .toBe(true);

  const text = (await page.locator('#materials-info').textContent()) ?? '';
  // Diamond coordination is 4.
  expect(text).toContain('4');
});

// 2c. Panel has at least one .mol-row element (data-driven rows rendered).
test('materials-panel: has at least one .mol-row after default load', async ({ page }) => {
  await gotoMaterials(page);
  await page.waitForTimeout(700);

  const rowCount = await page.evaluate(() => {
    const panel = document.getElementById('materials-info');
    if (!panel) return 0;
    return panel.querySelectorAll('.mol-row').length;
  });

  // Name + Formula + Hybridization + Coordination + Bond + Note = 6 rows minimum.
  expect(rowCount).toBeGreaterThan(2);
});

// ────────────────────────────────────────────────────────────────────────────
// GATE 3 — Panel updates when a different card is selected
// ────────────────────────────────────────────────────────────────────────────

// 3a. Clicking the Graphene card updates the panel to show Graphene-specific text.
// Exact strings from Lattice.purs: hybridization "sp²", coordination value "3",
// bond value "C–C ~142 pm", and name "Graphene".
test('materials-panel: updates to Graphene text after clicking Graphene card', async ({ page }) => {
  await gotoMaterials(page);
  await page.waitForTimeout(700);

  // Click the Graphene card (data-index="1").
  await page.click('.material-card[data-index="1"]');
  await page.waitForTimeout(500);

  await expect
    .poll(
      async () => {
        const txt = (await page.locator('#materials-info').textContent()) ?? '';
        // Graphene has hybridization sp² and coordination 3.
        return txt.includes('Graphene');
      },
      { timeout: 5000 },
    )
    .toBe(true);

  const text = (await page.locator('#materials-info').textContent()) ?? '';
  // Graphene-specific: hybridization "sp²", coordination number "3",
  // bond length text "142 pm" (from "C–C ~142 pm"), and the name "Graphene".
  expect(text).toContain('Graphene');
  expect(text).toContain('sp²');
  expect(text).toContain('3');
  expect(text).toContain('142');
});

// 3b. Selecting Diamond card then Graphene card changes the panel text.
test('materials-panel: panel text changes when switching between Diamond and Graphene', async ({ page }) => {
  await gotoMaterials(page);
  await page.waitForTimeout(700);

  // Ensure Diamond is loaded (default).
  await page.click('.material-card[data-index="0"]');
  await page.waitForTimeout(500);
  const diamondText = (await page.locator('#materials-info').textContent()) ?? '';

  // Switch to Graphene.
  await page.click('.material-card[data-index="1"]');
  await page.waitForTimeout(500);
  const grapheneText = (await page.locator('#materials-info').textContent()) ?? '';

  // The two texts must differ (different structure properties).
  expect(diamondText).not.toBe(grapheneText);
});

// ────────────────────────────────────────────────────────────────────────────
// GATE 4 — Seam sync: loadStructure(i) updates the panel
// ────────────────────────────────────────────────────────────────────────────

// 4a. window.__builder.loadStructure(1) (Graphene) updates the panel text.
test('materials-panel: loadStructure seam updates panel text', async ({ page }) => {
  await gotoMaterials(page);
  await page.waitForTimeout(700);

  // Read the panel after Diamond default-load.
  const beforeText = (await page.locator('#materials-info').textContent()) ?? '';

  // Load Graphene via seam.
  await page.evaluate(() => window.__builder.loadStructure(1));
  await page.waitForTimeout(500);

  const afterText = (await page.locator('#materials-info').textContent()) ?? '';

  // Panel must have changed (Graphene has different properties from Diamond).
  expect(afterText).not.toBe(beforeText);
});

// 4b. loadStructure(0) then loadStructure(1) → panel text differs between the two.
test('materials-panel: loadStructure(0) and loadStructure(1) produce different panel text', async ({ page }) => {
  await gotoMaterials(page);
  await page.waitForTimeout(700);

  await page.evaluate(() => window.__builder.loadStructure(0));
  await page.waitForTimeout(400);
  const diamondText = (await page.locator('#materials-info').textContent()) ?? '';

  await page.evaluate(() => window.__builder.loadStructure(1));
  await page.waitForTimeout(400);
  const grapheneText = (await page.locator('#materials-info').textContent()) ?? '';

  expect(diamondText).not.toBe(grapheneText);
});

// ────────────────────────────────────────────────────────────────────────────
// GATE 5 — Controls still work in Materials (orbit, zoom, valence-only)
// ────────────────────────────────────────────────────────────────────────────

// 5a. Orbit via setOrbit changes rendered pixels in Materials.
test('materials-panel: orbit changes rendered pixels in Materials', async ({ page }) => {
  await gotoMaterials(page);
  await page.waitForFunction(
    () => typeof window.__builder?.setOrbit === 'function',
    null,
    { timeout: 6000 },
  );
  await page.waitForTimeout(400);

  const before = await readRegion(page, 0.2, 0.2, 0.8, 0.8, 16, 8);

  await page.evaluate(() => window.__builder.setOrbit(1.5, 0.0));
  await page.waitForTimeout(300);
  await page.evaluate(
    () =>
      new Promise((r) =>
        requestAnimationFrame(() => requestAnimationFrame(() => requestAnimationFrame(r))),
      ),
  );
  await page.waitForTimeout(400);

  const after = await readRegion(page, 0.2, 0.2, 0.8, 0.8, 16, 8);

  const anyChanged = before.some(
    (p, i) =>
      Math.abs(p[0] - after[i][0]) > 8 ||
      Math.abs(p[1] - after[i][1]) > 8 ||
      Math.abs(p[2] - after[i][2]) > 8,
  );
  expect(anyChanged).toBe(true);
});

// 5b. Valence-only toggle is reachable and toggling it does not crash in Materials.
test('materials-panel: valence-only toggle works in Materials without crash', async ({ page }) => {
  await gotoMaterials(page);
  await page.waitForTimeout(400);

  // Toggle valence-only on.
  await page.check('#valence-only');
  await page.waitForTimeout(300);

  // Check the world is still populated (no crash / clear side-effect).
  const atoms = await page.evaluate(() => window.__builder.getAtoms());
  expect(atoms.length).toBeGreaterThan(0);

  // Toggle valence-only off.
  await page.uncheck('#valence-only');
  await page.waitForTimeout(300);

  const atomsAfter = await page.evaluate(() => window.__builder.getAtoms());
  expect(atomsAfter.length).toBeGreaterThan(0);
});

// 5c. Canvas still shows lit pixels after toggling valence-only in Materials.
test('materials-panel: canvas still has lit pixels after valence-only toggle in Materials', async ({ page }) => {
  await gotoMaterials(page);
  await page.waitForTimeout(400);

  await page.check('#valence-only');
  await page.waitForTimeout(400);

  const pixels = await readRegion(page, 0.1, 0.1, 0.9, 0.9, 24, 12);
  const litCount = pixels.filter((p) => p[0] > 30 || p[1] > 30 || p[2] > 30).length;
  expect(litCount).toBeGreaterThan(2);

  const distinct = distinctColors(pixels, 20);
  expect(distinct).toBeGreaterThan(1);
});

// ────────────────────────────────────────────────────────────────────────────
// GATE 6 — Layout: #materials-info and #pe-overlay must not overlap
// ────────────────────────────────────────────────────────────────────────────

// 6a. In the Materials scene, #materials-info and #pe-overlay are both visible
// and their bounding rects do NOT intersect. This guards Fix 1 (layout).
test('materials-panel: #materials-info and #pe-overlay do not overlap in Materials scene', async ({ page }) => {
  await gotoMaterials(page);
  await page.waitForTimeout(700);

  // Confirm both elements are visible.
  await expect(page.locator('#materials-info')).toBeVisible();
  await expect(page.locator('#pe-overlay')).toBeVisible();

  // Read bounding rects of both overlays.
  const rects = await page.evaluate(() => {
    const info = document.getElementById('materials-info');
    const pe = document.getElementById('pe-overlay');
    if (!info || !pe) return null;
    const ir = info.getBoundingClientRect();
    const pr = pe.getBoundingClientRect();
    return {
      info: { top: ir.top, right: ir.right, bottom: ir.bottom, left: ir.left },
      pe:   { top: pr.top, right: pr.right, bottom: pr.bottom, left: pr.left },
    };
  });

  expect(rects).not.toBeNull();

  // Two axis-aligned rectangles do NOT intersect when one is fully left of,
  // fully right of, fully above, or fully below the other.
  const { info: ir, pe: pr } = rects;
  const noOverlap =
    ir.right  <= pr.left   || // info is entirely to the left of pe
    ir.left   >= pr.right  || // info is entirely to the right of pe
    ir.bottom <= pr.top    || // info is entirely above pe
    ir.top    >= pr.bottom;   // info is entirely below pe

  expect(noOverlap).toBe(true);
});
