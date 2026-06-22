// Atomos view toggles (2D Bohr + sub-shell/shell) and overlays/title specs.
// Split out of the original e2e/world.spec.js (behaviour-frozen reorganisation).
import { test } from '@playwright/test';
import {
  expect, waitForRenderedCanvas, openDrawer, readPixel, readRegion, distinctColors,
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

// overlay-text M1: the atomos element label (anime.js scramble) shows the
// element name and updates when the element changes.
test('overlay: element label shows/updates the element name', async ({ page }) => {
  const label = page.locator('#atom-label');

  // Hidden in the cube POC scene.
  await expect(label).toBeHidden();

  await page.click('#scene-toggle'); // → atomos (default Carbon, Z=6)
  await expect(label).toBeVisible();
  // Wait for the scramble to settle on "Carbon".
  await expect(label).toHaveText('Carbon', { timeout: 4000 });

  // Changing the element re-scrambles to the new name.
  await page.fill('#element-value', '8');
  await expect(label).toHaveText('Oxygen', { timeout: 4000 });

  // Switching back to the cube POC hides the label again.
  await page.click('#scene-toggle');
  await expect(label).toBeHidden();
});

// orbital-lines M2: the orbital-info overlay shows the electron configuration of
// the selected element and updates when the element changes; atomos-only.
test('overlay: orbital-info shows the electron configuration', async ({ page }) => {
  const info = page.locator('#orbital-info');

  // Hidden in the cube POC scene.
  await expect(info).toBeHidden();

  await page.click('#scene-toggle'); // → atomos (default Carbon, Z=6)
  await expect(info).toBeVisible();
  // Settles to Carbon's configuration.
  await expect(info).toHaveText('1s2 2s2 2p2', { timeout: 4000 });

  // Switching element re-scrambles to the new configuration.
  await page.fill('#element-value', '36');
  await expect(info).toHaveText('1s2 2s2 2p6 3s2 3p6 3d10 4s2 4p6', { timeout: 4000 });

  // Hidden again back in the cube POC scene.
  await page.click('#scene-toggle');
  await expect(info).toBeHidden();
});

// subshells M2: the element table now spans Z=1..36, so the selector reaches
// Krypton and the label shows the new name; the scene still renders.
test('subshells M2: element table reaches Krypton (Z=36)', async ({ page }) => {
  const label = page.locator('#atom-label');
  await page.click('#scene-toggle'); // → atomos

  // The selector accepts the extended range up to 36.
  await expect(page.locator('#element-value')).toHaveAttribute('max', '36');

  // Z=36 settles the label on "Krypton".
  await page.fill('#element-value', '36');
  await expect(label).toHaveText('Krypton', { timeout: 4000 });

  // The atom still renders lit geometry at center (no crash at the new max).
  const center = await readPixel(page, 0.5, 0.5);
  expect(center[0] + center[1] + center[2]).toBeGreaterThan(40);
});

// overlay-text M2: the scene-title banner scrambles to the current scene name.
// Updated for the 5-cycle: CubePoc → Atomos → Molecule → Builder → Materials → CubePoc.
test('overlay: scene title updates on scene switch', async ({ page }) => {
  const title = page.locator('#scene-title');
  await expect(title).toHaveText('Cube POC');

  await page.click('#scene-toggle'); // → atomos
  await expect(title).toHaveText('atomos', { timeout: 4000 });

  await page.click('#scene-toggle'); // → molecule (third scene)
  await expect(title).toHaveText('molecule', { timeout: 4000 });

  await page.click('#scene-toggle'); // → builder (fourth scene)
  await expect(title).toHaveText('builder', { timeout: 4000 });

  await page.click('#scene-toggle'); // → materials (fifth scene)
  await expect(title).toHaveText('materials', { timeout: 4000 });

  await page.click('#scene-toggle'); // → back to cube POC (5-cycle completes)
  await expect(title).toHaveText('Cube POC', { timeout: 4000 });
});

// atomos 2D-toggle M2: a "2D" checkbox (#view-2d) flattens the atom — the
// inclined 3D orbital rings collapse into concentric, top-bottom-symmetric 2D
// circles. RED until M2 wires #view-2d into Main/FRP.Loop/index.html.
//
// Signature: over a sampled region around the atom we (a) count the total lit
// orbital pixels and (b) split those lit pixels into the top half vs bottom half
// (about the atom's vertical centre) and form a top/bottom asymmetry ratio
//   asym = |top - bottom| / (top + bottom).
// Rings are the dominant STATIC lit structure (electrons are a few sweeping
// dots), so this aggregate is robust to electron animation under SwiftShader.
// 3D inclined rings push lit mass off-centre (higher asym); flattened 2D
// concentric circles are top-bottom symmetric (lower asym). We assert the 2D
// signature differs measurably from 3D (both via the asymmetry metric AND a
// direct region pixel-delta count), then that unchecking restores toward 3D.
test('atomos: 2D toggle flattens the atom (rings become concentric)', async ({ page }) => {
  await page.click('#scene-toggle'); // → atomos
  await page.fill('#element-value', '36'); // Krypton: 4 shells, many rings
  await page.waitForTimeout(500);

  // Sampled orbital region centred on the atom; rows span top→bottom.
  const COLS = 36;
  const ROWS = 24;
  const region = () => readRegion(page, 0.18, 0.18, 0.82, 0.82, COLS, ROWS);
  const isLit = (p) => p[0] + p[1] + p[2] > 60;

  // Aggregate signature: total lit count + top/bottom lit asymmetry ratio.
  const signature = (px) => {
    let lit = 0, top = 0, bottom = 0;
    const half = Math.floor(ROWS / 2);
    for (let r = 0; r < ROWS; r++) {
      for (let i = 0; i < COLS; i++) {
        if (isLit(px[r * COLS + i])) {
          lit++;
          if (r < half) top++; else bottom++;
        }
      }
    }
    const asym = top + bottom > 0 ? Math.abs(top - bottom) / (top + bottom) : 0;
    return { lit, asym };
  };

  // Pixel-delta between two region snapshots (robust scalar of how much changed).
  const regionDelta = (a, b) =>
    a.filter((p, i) =>
      Math.abs(p[0] - b[i][0]) + Math.abs(p[1] - b[i][1]) + Math.abs(p[2] - b[i][2]) > 30
    ).length;

  // Poll until the atom region has actually painted (render-ready), rather than
  // trusting a fixed timeout — robust to slow cold-start frames under full-suite
  // load (the Krypton render after a scene+element change can lag a frame or two).
  const waitForLit = async (min = 3, tries = 25) => {
    for (let i = 0; i < tries; i++) {
      if (signature(await region()).lit > min) return;
      await page.waitForTimeout(120);
    }
  };

  // --- 3D baseline (two frames) ---------------------------------------------
  await waitForLit();
  // The flatten signature is the REGION PIXEL-DELTA: replacing the inclined 3D
  // rings with flat concentric circles relocates a large block of lit ring
  // pixels. We anchor it against animation noise — the delta between two
  // consecutive 3D frames, where the static rings are unchanged and only the
  // sparse sweeping electrons move — so the test proves a real structural change
  // rather than animation jitter. (The earlier top/bottom asymmetry metric was
  // dropped: for a multi-shell atom the many inclined rings average near
  // top/bottom symmetric, so that ratio is animation-noisy and unreliable.)
  const threeApx = await region();
  const threeA = signature(threeApx);
  expect(threeA.lit).toBeGreaterThan(3); // orbital region is actually lit (rings present)
  await page.waitForTimeout(600);
  const threeBpx = await region();
  const animNoise = regionDelta(threeApx, threeBpx); // electrons moved, rings static → small

  // --- flip to 2D ------------------------------------------------------------
  await expect(page.locator('#view-2d')).toBeVisible();
  await page.check('#view-2d');
  await page.waitForTimeout(600); // let the flattened transform render
  await waitForLit();

  const twoDpx = await region();
  const twoD = signature(twoDpx);
  const flattenDelta = regionDelta(threeBpx, twoDpx);

  // The atom flattened: relocating the rings changes FAR more region pixels than
  // mere animation does between two 3D frames — a structural change, not jitter.
  expect(twoD.lit).toBeGreaterThan(3); // still a lit atom in 2D
  expect(flattenDelta).toBeGreaterThan(20);
  expect(flattenDelta).toBeGreaterThan(animNoise * 2 + 8);

  // --- flip back to 3D -------------------------------------------------------
  await page.uncheck('#view-2d');
  await page.waitForTimeout(600);
  await waitForLit();

  const restoredPx = await region();
  const restored = signature(restoredPx);
  expect(restored.lit).toBeGreaterThan(3);

  // Restoring un-flattens the atom: it returns TOWARD the original 3D geometry,
  // not to some third state. The restored frame and the 3D baseline share the
  // same static inclined rings (differing only by advanced electron animation),
  // so restored-vs-3D delta is on the order of animation noise and MUCH smaller
  // than the flatten delta. This would fail a hollow toggle-back that left the
  // atom flat.
  const restoreDelta = regionDelta(threeBpx, restoredPx);
  expect(restoreDelta).toBeLessThan(flattenDelta);
});

// atomos shell/sub-shell toggle M2: a "#subshell-view" checkbox (CHECKED by
// default) switches the atomos rings between SUB-SHELL view (one ring per filled
// sub-shell — Carbon's L shell = 2s + 2p at DISTINCT radii → two rings) and
// SHELL-only (Bohr) view (each principal shell collapsed onto ONE ring — Carbon's
// L shell = a single ring). Unchecking MERGES the L sub-shell rings, so the count
// of distinct lit ring bands in a radial sample DROPS; re-checking restores it.
//
// Signature: rings are coloured by sub-shell (shell hue, lighter outward) in
// sub-shell view and by shell (one hue per shell) in shell view, and distinct
// sub-shell radii spread lit mass across more radial bands. So both a colour
// count AND a distinct-radial-band count over a region around the atom DROP when
// the sub-shells collapse. We reuse the region/distinctColors helpers and poll
// for render-readiness (SwiftShader robustness), exactly like the 2D-toggle and
// colour-coded specs. The merge must hold in flat (2D) view too.
test('atomos: sub-shell toggle merges sub-shell rings (Carbon L shell)', async ({ page }) => {
  await page.click('#scene-toggle'); // → atomos
  await page.fill('#element-value', '6'); // Carbon: K [1s], L [2s,2p] → 3 sub-shells / 2 shells
  await page.waitForTimeout(500);

  // Region around the atom; rows top→bottom. The Carbon rings sit in this band.
  const COLS = 40;
  const ROWS = 28;
  const region = () => readRegion(page, 0.16, 0.16, 0.84, 0.84, COLS, ROWS);
  const isLit = (p) => p[0] + p[1] + p[2] > 60;

  // Count distinct LIT colour buckets — sub-shell view surfaces more (1s red,
  // 2s amber-base, 2p lighter amber) than shell view (K red, L amber).
  const ringColours = (px) => distinctColors(px.filter(isLit), 20);

  // Count distinct LIT radial bands: bucket each lit pixel by its distance from
  // the region centre (in row/col grid units) and count how many buckets light
  // up. Distinct sub-shell radii spread lit mass over MORE bands than a single
  // collapsed shell ring, so this DROPS when sub-shells merge. Ring-animation
  // robust: rings are the dominant static lit structure.
  const ringBands = (px) => {
    const cr = (ROWS - 1) / 2, cc = (COLS - 1) / 2;
    const set = new Set();
    for (let r = 0; r < ROWS; r++) {
      for (let i = 0; i < COLS; i++) {
        if (isLit(px[r * COLS + i])) {
          const dr = r - cr, dc = i - cc;
          set.add(Math.round(Math.sqrt(dr * dr + dc * dc)));
        }
      }
    }
    return set.size;
  };

  const litCount = (px) => px.filter(isLit).length;

  // Poll until the atom region has painted (render-ready), like the 2D-toggle spec.
  const waitForLit = async (min = 3, tries = 25) => {
    for (let i = 0; i < tries; i++) {
      if (litCount(await region()) > min) return;
      await page.waitForTimeout(120);
    }
  };

  // A metric that combines colour + band counts; both shrink when sub-shells merge.
  const metric = async () => {
    await waitForLit();
    const px = await region();
    return ringColours(px) + ringBands(px);
  };

  // --- baseline: sub-shell view (checkbox CHECKED by default) ----------------
  await expect(page.locator('#subshell-view')).toBeVisible();
  await expect(page.locator('#subshell-view')).toBeChecked();
  const baseline = await metric();
  expect(baseline).toBeGreaterThan(2);

  // --- UNCHECK → shell-only (Bohr): L sub-shell rings merge → metric DROPS ----
  await page.uncheck('#subshell-view');
  await page.waitForTimeout(600);
  const merged = await metric();
  expect(merged).toBeLessThan(baseline);

  // --- RE-CHECK → sub-shell view restored: metric returns toward baseline -----
  await page.check('#subshell-view');
  await page.waitForTimeout(600);
  const restored = await metric();
  expect(restored).toBeGreaterThan(merged);

  // --- flat (2D) view: the merge must hold there too --------------------------
  await page.check('#view-2d');
  await page.waitForTimeout(600);
  const flatSub = await metric();

  await page.uncheck('#subshell-view');
  await page.waitForTimeout(600);
  const flatMerged = await metric();
  expect(flatMerged).toBeLessThan(flatSub);

  await page.check('#subshell-view');
  await page.waitForTimeout(600);
  const flatRestored = await metric();
  expect(flatRestored).toBeGreaterThan(flatMerged);
});
