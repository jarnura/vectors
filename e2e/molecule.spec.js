// H₂ molecule scene specs (two nuclei / shared pair / properties / bond animation).
// Split out of the original e2e/world.spec.js (behaviour-frozen reorganisation).
import { test } from '@playwright/test';
import {
  expect, waitForRenderedCanvas, openDrawer, readRegion,
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

// molecule-platform M2: a third scene `Molecule` renders the H₂ molecule — two
// hydrogen nuclei separated along x with a shared electron pair in the bond
// between them. Reached by clicking #scene-toggle TWICE from the initial Cube
// POC (CubePoc → Atomos → Molecule). RED until M2 wires the Molecule scene into
// Main/Scene/index.html.
//
// Signature: H₂ is two nuclei flanking a central shared-electron cloud, so a
// horizontal strip through the vertical centre is lit in THREE separated places
// — left nucleus, shared electrons in the middle, right nucleus. We split the
// central strip into left/middle/right thirds and assert all three thirds carry
// lit pixels. This distinguishes the molecule from a single centred atom (where
// only the middle third would be densely lit, the outer thirds dark backdrop) —
// the two flanking nuclei are the load-bearing difference.
test('molecule: H₂ shows two nuclei flanking a shared electron pair', async ({ page }) => {
  await expect(page.locator('#scene-toggle')).toBeVisible();

  // The molecule scene reuses the atomos starfield, so a bare "is this third
  // lit?" check is hollow — the 140 background stars light every third whether
  // or not the nuclei render. Instead we isolate the nuclei by DELTA against the
  // single-atom atomos scene, which shares the identical static starfield: the
  // stars cancel, leaving only the structural difference. A single centred atom
  // concentrates its dense lit mass in the MIDDLE third (outer thirds = stars
  // only); H₂ moves two dense nucleus clusters out into the LEFT and RIGHT
  // thirds. So the molecule's outer thirds must gain lit mass over atomos's.
  const COLS = 60, ROWS = 15;
  const isLit = (p) => p[0] + p[1] + p[2] > 60;
  const thirds = (strip) => {
    const t = Math.floor(COLS / 3);
    let left = 0, middle = 0, right = 0;
    for (let r = 0; r < ROWS; r++) {
      for (let i = 0; i < COLS; i++) {
        if (!isLit(strip[r * COLS + i])) continue;
        if (i < t) left++; else if (i < 2 * t) middle++; else right++;
      }
    }
    return { left, middle, right };
  };
  const strip = () => readRegion(page, 0.10, 0.36, 0.90, 0.64, COLS, ROWS);
  // Poll until the centre band has actually painted (the middle third is lit in
  // both the centred atom and H₂), rather than trusting a fixed timeout — robust
  // to slow cold-start frames under full-suite/CI load.
  const sampleWhenReady = async () => {
    for (let i = 0; i < 25; i++) {
      const s = thirds(await strip());
      if (s.middle > 0) return s;
      await page.waitForTimeout(120);
    }
    return thirds(await strip());
  };

  // --- atomos baseline: a single Hydrogen atom (sparsest, centred) -----------
  await page.click('#scene-toggle'); // → atomos
  await page.fill('#element-value', '1'); // Hydrogen: 1 centred proton
  await page.waitForTimeout(500);
  const atom = await sampleWhenReady();

  // --- molecule: H₂ ----------------------------------------------------------
  await page.click('#scene-toggle'); // → molecule
  await page.waitForTimeout(700); // let the H₂ scene render + animate
  const mol = await sampleWhenReady();

  // H₂ lights all three thirds: left nucleus, shared electrons, right nucleus.
  expect(mol.left).toBeGreaterThan(0);
  expect(mol.middle).toBeGreaterThan(0);
  expect(mol.right).toBeGreaterThan(0);

  // The decisive, star-robust signal: the two flanking nuclei add lit mass to
  // BOTH outer thirds that the single centred atom does not. The single-atom
  // baseline confirms this narrow centre-band's outer thirds are essentially
  // dark (the starfield does NOT fill them), so a broken/centred/absent nucleus
  // render would leave the outer thirds at the baseline and fail here. The
  // identical starfield cancels in the comparison.
  expect(mol.left).toBeGreaterThan(atom.left);
  expect(mol.right).toBeGreaterThan(atom.right);
  expect(mol.left + mol.right).toBeGreaterThan(atom.left + atom.right + 1);
});

// molecule-platform M3 (TEST A): a data-driven `#molecule-info` properties panel
// is shown ONLY in the Molecule scene, populated from `Molecule.properties` (one
// row per property), animated in via anime.js. Reached by two #scene-toggle
// clicks (CubePoc → Atomos → Molecule). RED until M3 adds #molecule-info to
// index.html + wires its visibility/content through Main/Controls.
//
// Signature: the panel's settled text must surface H₂'s data-driven property
// values from Molecule.purs — the formula ("H₂"/"H2"), the covalent bond
// ("covalent"), the bond length ("~74 pm" → "74") and bond energy
// ("~436 kJ/mol" → "436"). It must be HIDDEN in the other two scenes (initial
// Cube POC and atomos), mirroring how #atom-label/#orbital-info visibility is
// asserted. anime.js scramble animates the text, so we wait for it to settle
// (generous timeout) and assert the load-bearing digits/words.
test('molecule: properties panel shows H₂\'s data-driven properties', async ({ page }) => {
  const info = page.locator('#molecule-info');

  // HIDDEN in the initial Cube POC scene (mirrors #atom-label/#orbital-info).
  await expect(info).toBeHidden();

  // HIDDEN in atomos (one click) — only the molecule scene shows it.
  await page.click('#scene-toggle'); // → atomos
  await page.waitForTimeout(300);
  await expect(info).toBeHidden();

  // VISIBLE in the molecule scene (second click).
  await page.click('#scene-toggle'); // → molecule
  await page.waitForTimeout(300);
  await expect(info).toBeVisible();

  // Wait for the anime.js scramble to settle, then read the settled textContent.
  // We poll because scrambleText animates the characters before resolving on the
  // final values; assert the load-bearing digits/words from Molecule.properties.
  await expect
    .poll(
      async () => {
        const txt = (await info.textContent()) ?? '';
        const hasFormula = txt.includes('H₂') || txt.includes('H2');
        return (
          hasFormula &&
          txt.includes('covalent') &&
          txt.includes('74') && // bond length "~74 pm"
          txt.includes('436') // bond energy "~436 kJ/mol"
        );
      },
      { timeout: 6000 },
    )
    .toBe(true);

  // Switching fully around (one more click) back to the Cube POC hides it again.
  await page.click('#scene-toggle'); // → back to cube POC
  await page.waitForTimeout(300);
  await expect(info).toBeHidden();
});

// molecule-platform M3 (TEST B): a bond-formation control (`#bond-btn`), wired
// through a new anime.js Controls FFI, drives a State `bondProgress`. Clicking it
// runs an anime.js animation that moves the two H atoms together / coalesces the
// shared pair — a measurable render change in the molecule region. RED until M3
// adds #bond-btn to index.html + wires Controls → State.bondProgress → Main.
//
// Signature: capture a horizontal strip across the vertical centre where the two
// nuclei + shared pair sit (like the molecule M2 spec), click #bond-btn, wait for
// the anime.js bond-formation animation, then assert the strip's pixels changed
// measurably (region pixel-delta over a clear, non-trivial threshold) — the atoms
// actually moved together / the pair coalesced.
test('molecule: bond control animates the atoms together', async ({ page }) => {
  // Reach the molecule scene (two clicks) and let it render.
  await page.click('#scene-toggle'); // → atomos
  await page.click('#scene-toggle'); // → molecule
  await page.waitForTimeout(700); // let the H₂ scene render + animate

  // Horizontal strip across the vertical centre spanning both nuclei + the pair.
  const COLS = 60, ROWS = 15;
  const strip = () => readRegion(page, 0.10, 0.36, 0.90, 0.64, COLS, ROWS);
  const isLit = (p) => p[0] + p[1] + p[2] > 60;

  // Pixel-delta between two strip snapshots (scalar of how much changed).
  const regionDelta = (a, b) =>
    a.filter((p, i) =>
      Math.abs(p[0] - b[i][0]) + Math.abs(p[1] - b[i][1]) + Math.abs(p[2] - b[i][2]) > 30
    ).length;

  // Poll until the molecule strip has actually painted (render-ready) rather than
  // trusting a fixed timeout — robust to slow cold-start frames under full-suite
  // load (mirrors the atomos 2D-toggle spec's waitForLit).
  const litCount = (px) => px.filter(isLit).length;
  const waitForLit = async (min = 3, tries = 25) => {
    for (let i = 0; i < tries; i++) {
      if (litCount(await strip()) > min) return;
      await page.waitForTimeout(120);
    }
  };

  await waitForLit();

  // Baseline strip BEFORE triggering bond formation. Also measure the ambient
  // animation noise between two consecutive frames (the shared pair breathes +
  // electrons sweep, rings static) so the bond delta must clearly exceed it.
  const beforeA = await strip();
  await page.waitForTimeout(300);
  const beforeB = await strip();
  const animNoise = regionDelta(beforeA, beforeB);

  // The bond-formation control is visible in the molecule scene.
  await expect(page.locator('#bond-btn')).toBeVisible();

  // Click it; wait for the anime.js bond-formation animation to play out.
  await page.click('#bond-btn');
  await page.waitForTimeout(1200); // generous: let the bond animation drive State

  const after = await strip();
  const bondDelta = regionDelta(beforeB, after);

  // The molecule region changed measurably: the atoms moved together / the shared
  // pair coalesced. The threshold is robust but non-trivial — the animation must
  // actually relocate pixels — and must clearly exceed mere animation jitter.
  expect(bondDelta).toBeGreaterThan(20);
  expect(bondDelta).toBeGreaterThan(animNoise * 2 + 8);
});
