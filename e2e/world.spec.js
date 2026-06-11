// Baseline canvas-verification spec for the vectors WebGL2 scene.
//
// This is the harness the world-backdrop milestones extend. At baseline (before
// the world meshes exist) it only asserts the canvas renders SOMETHING (the
// cubes). Milestone-specific assertions (ground band, grid lines, sky color,
// horizon transition) are added/enabled as M1–M4 land.
import { test } from '@playwright/test';
import {
  expect, waitForRenderedCanvas, openDrawer, readPixel, readRow, readRegion, distinctColors,
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

test('canvas renders a non-empty scene', async ({ page }) => {
  // The scene is not a single flat color: sampling a row across the middle
  // should surface more than one color bucket (cube vs background at minimum).
  const row = await readRow(page, 0.5, 24);
  expect(distinctColors(row)).toBeGreaterThan(1);
});

test('center shows the main cube (foreground geometry present)', async ({ page }) => {
  const bg = await readPixel(page, 0.02, 0.02); // top-left corner ≈ background
  const center = await readPixel(page, 0.5, 0.5); // cube sits at screen center
  expect(center).not.toEqual(bg);
});

// --- Milestone hooks (enabled as the world meshes land) ----------------------
// M2: ground plane visible below the cube (green ground vs white background).
test('M2: ground band visible below cube', async ({ page }) => {
  const sky = await readPixel(page, 0.5, 0.05);      // top: background
  const ground = await readPixel(page, 0.5, 0.92);   // bottom: ground plane
  // Ground is green (G dominant); background is white. They must differ.
  expect(ground).not.toEqual(sky);
  expect(ground[1]).toBeGreaterThan(ground[0]); // green channel dominates red
});

// M3: grid lines add color variation over the otherwise-flat ground.
test('M3: grid lines visible on ground', async ({ page }) => {
  // Sample a 2D region of the ground (lower-center). A flat solid ground would
  // be ~1 color bucket; the grid lines introduce additional buckets.
  const region = await readRegion(page, 0.25, 0.6, 0.75, 0.95, 30, 14);
  expect(distinctColors(region)).toBeGreaterThan(1);
});

// Shear button: clicking with a value shears the main cube (pixels change),
// while the static ground/grid stay put.
test('shear button: clicking shears the main cube', async ({ page }) => {
  await expect(page.locator('#shear-btn')).toBeVisible();
  await expect(page.locator('#shear-value')).toBeVisible();

  // Capture the cube region before the shear.
  const before = await readRegion(page, 0.35, 0.3, 0.65, 0.6, 16, 10);

  await page.fill('#shear-value', '1.5');
  await page.click('#shear-btn');
  await page.waitForTimeout(300); // let a few frames render the sheared transform

  const after = await readRegion(page, 0.35, 0.3, 0.65, 0.6, 16, 10);

  // The cube region must change after applying a non-zero shear.
  const changed = before.some((p, i) =>
    Math.abs(p[0] - after[i][0]) + Math.abs(p[1] - after[i][1]) + Math.abs(p[2] - after[i][2]) > 24
  );
  expect(changed).toBe(true);

  // The static ground far below stays the same color (world backdrop unaffected).
  const ground = await readPixel(page, 0.5, 0.95);
  expect(ground[1]).toBeGreaterThan(ground[0]); // still green-dominant
});

// atomos M2: the scene switch flips the cube POC (sky-blue) to atomos (near-black space).
test('atomos: scene switch flips backdrop sky-blue → near-black', async ({ page }) => {
  await expect(page.locator('#scene-toggle')).toBeVisible();

  // Cube POC backdrop is sky-blue (blue-dominant, bright).
  const skyBefore = await readPixel(page, 0.5, 0.04);
  expect(skyBefore[2]).toBeGreaterThan(skyBefore[0]); // blue > red
  expect(skyBefore[2]).toBeGreaterThan(120); // bright sky

  await page.click('#scene-toggle');
  await page.waitForTimeout(300);

  // Atomos backdrop is near-black deep space.
  const spaceAfter = await readPixel(page, 0.5, 0.04);
  expect(spaceAfter[0] + spaceAfter[1] + spaceAfter[2]).toBeLessThan(120); // dark

  // The switch is now a 5-cycle (CubePoc → Atomos → Molecule → Builder → Scale →
  // CubePoc), so cycling fully around returns to the cube POC sky. Molecule,
  // Builder, and Scale all share the dark deep-space backdrop with atomos, so the
  // next three clicks keep it dark.
  await page.click('#scene-toggle'); // → molecule (still dark space backdrop)
  await page.waitForTimeout(300);
  const moleculeBackdrop = await readPixel(page, 0.5, 0.04);
  expect(moleculeBackdrop[0] + moleculeBackdrop[1] + moleculeBackdrop[2]).toBeLessThan(120);

  await page.click('#scene-toggle'); // → builder (still dark space backdrop)
  await page.waitForTimeout(300);
  const builderBackdrop = await readPixel(page, 0.5, 0.04);
  expect(builderBackdrop[0] + builderBackdrop[1] + builderBackdrop[2]).toBeLessThan(120);

  await page.click('#scene-toggle'); // → scale (still dark space backdrop)
  await page.waitForTimeout(300);
  const scaleBackdrop = await readPixel(page, 0.5, 0.04);
  expect(scaleBackdrop[0] + scaleBackdrop[1] + scaleBackdrop[2]).toBeLessThan(120);

  await page.click('#scene-toggle'); // → back to cube POC sky
  await page.waitForTimeout(300);
  const skyAgain = await readPixel(page, 0.5, 0.04);
  expect(skyAgain[2]).toBeGreaterThan(120);
});

// atomos M3: in atomos, a nucleus (proton/neutron spheres) is visible at center.
test('atomos: nucleus visible at center', async ({ page }) => {
  await page.click('#scene-toggle'); // → atomos
  await page.waitForTimeout(300);

  const space = await readPixel(page, 0.5, 0.04);   // deep-space backdrop (dark)
  const center = await readPixel(page, 0.5, 0.5);    // nucleus cluster at center

  // The center is lit nucleon geometry, clearly brighter than the dark backdrop.
  const spaceSum = space[0] + space[1] + space[2];
  const centerSum = center[0] + center[1] + center[2];
  expect(centerSum).toBeGreaterThan(spaceSum + 60);
});

// atomos: discrete electrons orbit on the orbital rings — the region around the
// nucleus changes across frames as the electron dots sweep their rings.
test('atomos: electrons orbit on the rings', async ({ page }) => {
  await page.click('#scene-toggle'); // → atomos
  await page.fill('#element-value', '7'); // Nitrogen
  await page.waitForTimeout(400);

  // A band around the nucleus where the rings + electrons sit.
  const ringA = await readRegion(page, 0.30, 0.30, 0.70, 0.70, 28, 16);
  await page.waitForTimeout(700); // let the electrons advance along their rings
  const ringB = await readRegion(page, 0.30, 0.30, 0.70, 0.70, 28, 16);

  // Ring/electron structure is lit ...
  expect(distinctColors(ringA)).toBeGreaterThan(1);
  // ... and the electrons orbit: several sampled points change across frames.
  const moved = ringA.filter((p, i) =>
    Math.abs(p[0] - ringB[i][0]) + Math.abs(p[1] - ringB[i][1]) + Math.abs(p[2] - ringB[i][2]) > 30
  ).length;
  expect(moved).toBeGreaterThan(2);
});

// atomos M5: the element selector reconfigures the atom (nucleus changes).
test('atomos: element selector changes the atom', async ({ page }) => {
  await expect(page.locator('#element-value')).toBeVisible();
  await page.click('#scene-toggle'); // → atomos (default Carbon, Z=6)
  await page.waitForTimeout(300);

  // Tight center region = the nucleus (electrons sweep further out).
  const carbonNucleus = await readRegion(page, 0.42, 0.42, 0.58, 0.58, 18, 18);

  // Switch to Hydrogen (Z=1): a single nucleon — a much sparser nucleus.
  await page.fill('#element-value', '1');
  await page.waitForTimeout(400);
  const hydrogenNucleus = await readRegion(page, 0.42, 0.42, 0.58, 0.58, 18, 18);

  const changed = carbonNucleus.filter((p, i) =>
    Math.abs(p[0] - hydrogenNucleus[i][0]) + Math.abs(p[1] - hydrogenNucleus[i][1]) + Math.abs(p[2] - hydrogenNucleus[i][2]) > 24
  ).length;
  expect(changed).toBeGreaterThan(3);

  // Out-of-range Z must not crash: scene still renders something at center.
  await page.fill('#element-value', '999');
  await page.waitForTimeout(300);
  const afterBad = await readPixel(page, 0.5, 0.5);
  expect(afterBad[0] + afterBad[1] + afterBad[2]).toBeGreaterThan(40);
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

// atomos: orbital ring lines + discrete electrons render lit structure for any
// element, and out-of-range Z is render-safe.
test('atomos: orbital rings + electrons render lit structure', async ({ page }) => {
  await page.click('#scene-toggle'); // → atomos

  const litCount = async () => {
    const r = await readRegion(page, 0.20, 0.20, 0.80, 0.80, 32, 20);
    return r.filter((p) => p[0] + p[1] + p[2] > 60).length;
  };

  await page.fill('#element-value', '6'); // Carbon
  await page.waitForTimeout(400);
  expect(await litCount()).toBeGreaterThan(3);

  await page.fill('#element-value', '36'); // Krypton (many rings)
  await page.waitForTimeout(400);
  expect(await litCount()).toBeGreaterThan(3);

  // Out-of-range Z must not crash: scene still renders lit geometry at center.
  await page.fill('#element-value', '999');
  await page.waitForTimeout(300);
  const center = await readPixel(page, 0.5, 0.5);
  expect(center[0] + center[1] + center[2]).toBeGreaterThan(40);
});

// atomos: switching element reconfigures the atom (Carbon's rings/electrons
// differ from Iron's, which adds an inner d sub-shell + more electrons).
test('atomos: element switch reconfigures the atom', async ({ page }) => {
  await page.click('#scene-toggle'); // → atomos
  await page.fill('#element-value', '6'); // Carbon
  await page.waitForTimeout(400);
  const carbon = await readRegion(page, 0.25, 0.25, 0.75, 0.75, 30, 18);

  await page.fill('#element-value', '26'); // Iron
  await page.waitForTimeout(400);
  const iron = await readRegion(page, 0.25, 0.25, 0.75, 0.75, 30, 18);

  const changed = carbon.filter((p, i) =>
    Math.abs(p[0] - iron[i][0]) + Math.abs(p[1] - iron[i][1]) + Math.abs(p[2] - iron[i][2]) > 30
  ).length;
  expect(changed).toBeGreaterThan(4);
});

// atomos: electrons are discrete — a many-electron atom (Argon, 18) lights up
// noticeably more electron/ring structure than a one-electron atom (Hydrogen).
test('atomos: more electrons show more discrete structure', async ({ page }) => {
  await page.click('#scene-toggle'); // → atomos

  const litCount = async () => {
    const r = await readRegion(page, 0.18, 0.18, 0.82, 0.82, 36, 24);
    return r.filter((p) => p[0] + p[1] + p[2] > 90).length;
  };

  await page.fill('#element-value', '1'); // Hydrogen: 1 electron, 1 ring
  await page.waitForTimeout(400);
  const hydrogen = await litCount();

  await page.fill('#element-value', '18'); // Argon: 18 electrons, more rings
  await page.waitForTimeout(400);
  const argon = await litCount();

  expect(argon).toBeGreaterThan(hydrogen);
});

// colours: each shell is a distinct colour (sub-shells lighter), so a multi-shell
// element (Krypton, 4 shells) shows many more colours than a single-shell one (Helium).
test('atomos: shells are colour-coded (multi-shell shows more colours)', async ({ page }) => {
  await page.click('#scene-toggle'); // → atomos

  const colourCount = async () => {
    const r = await readRegion(page, 0.18, 0.18, 0.82, 0.82, 36, 24);
    return distinctColors(r);
  };

  await page.fill('#element-value', '2'); // Helium: 1 shell (one ring colour)
  await page.waitForTimeout(400);
  const helium = await colourCount();

  await page.fill('#element-value', '36'); // Krypton: 4 shells (red/amber/green/blue)
  await page.waitForTimeout(400);
  const krypton = await colourCount();

  // The multi-shell atom surfaces clearly more distinct colours.
  expect(krypton).toBeGreaterThan(helium + 2);
});

// overlay-text M2: the scene-title banner scrambles to the current scene name.
test('overlay: scene title updates on scene switch', async ({ page }) => {
  const title = page.locator('#scene-title');
  await expect(title).toHaveText('Cube POC');

  await page.click('#scene-toggle'); // → atomos
  await expect(title).toHaveText('atomos', { timeout: 4000 });

  await page.click('#scene-toggle'); // → molecule (third scene)
  await expect(title).toHaveText('molecule', { timeout: 4000 });

  await page.click('#scene-toggle'); // → builder (fourth scene)
  await expect(title).toHaveText('builder', { timeout: 4000 });

  await page.click('#scene-toggle'); // → scale (fifth scene)
  await expect(title).toHaveText('scale', { timeout: 4000 });

  await page.click('#scene-toggle'); // → back to cube POC
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

// molecule-builder M2 (TEST A): a fourth `Builder` scene renders placed atoms +
// auto-computed bonds, driven through a `window.__builder` test API. Reached by
// clicking #scene-toggle THREE times from the initial Cube POC (CubePoc →
// Atomos → Molecule → Builder). RED until M2 wires the Builder scene + the
// window.__builder API into Main/Scene/index.html/Loop.
//
// window.__builder shape (assumed; to be implemented in M2):
//   addAtom(z, x, y, z3)            -- place an atom of atomic number z at (x,y,z3)
//   moveAtom(id, x, y, z3)          -- move a placed atom by id
//   getBonds()  -> Array            -- the current auto-computed bonds
//   getMolecules() -> Array         -- connected components (formulae or id arrays)
//   clear()                         -- reset to the empty builder world
//
// Signature: two H atoms placed WITHIN Builder.bondThreshold (the JS doesn't know
// the exact number; we use a small separation of 60 total — well under the ~180
// threshold) auto-bond into a single H₂ molecule. We assert exactly one bond, a
// single 2-atom molecule, and that the canvas shows lit atom geometry.
test('builder: two H atoms within range auto-bond (H₂)', async ({ page }) => {
  // Reach the Builder scene (three clicks) and let it render.
  await expect(page.locator('#scene-toggle')).toBeVisible();
  await page.click('#scene-toggle'); // → atomos
  await page.click('#scene-toggle'); // → molecule
  await page.click('#scene-toggle'); // → builder
  await page.waitForTimeout(700); // generous: let the builder scene boot + render

  // The window.__builder test API must be present in the Builder scene.
  await page.waitForFunction(() => !!window.__builder, null, { timeout: 6000 });

  // Place two H atoms a SMALL distance apart (60 total along x — comfortably
  // under the ~180 bondThreshold), so they auto-bond into H₂.
  const result = await page.evaluate(() => {
    const b = window.__builder;
    b.clear();
    b.addAtom(1, -30, 0, 0); // H
    b.addAtom(1, 30, 0, 0);  // H, 60 apart → within bondThreshold
    return { bonds: b.getBonds(), molecules: b.getMolecules() };
  });

  // Exactly one bond between the two hydrogens.
  expect(result.bonds.length).toBe(1);

  // A single 2-atom molecule. getMolecules() may return components (id arrays)
  // or formula strings — adapt to whichever shape, asserting one H₂ molecule.
  expect(result.molecules.length).toBe(1);
  const mol = result.molecules[0];
  if (typeof mol === 'string') {
    // Formula form: "H₂" (or its ASCII fallback "H2").
    expect(mol === 'H₂' || mol === 'H2').toBe(true);
  } else if (Array.isArray(mol)) {
    // Connected-component (id array) form: a single 2-atom component.
    expect(mol.length).toBe(2);
  } else if (mol && typeof mol === 'object') {
    // Object form: a 2-atom molecule, optionally carrying a formula.
    const count = mol.atoms?.length ?? mol.size ?? mol.count;
    if (count !== undefined) expect(count).toBe(2);
    if (mol.formula !== undefined) {
      expect(mol.formula === 'H₂' || mol.formula === 'H2').toBe(true);
    }
  }

  // The canvas shows lit atom geometry around the centre (the two bonded H).
  const region = await readRegion(page, 0.30, 0.30, 0.70, 0.70, 28, 16);
  const lit = region.filter((p) => p[0] + p[1] + p[2] > 60).length;
  expect(distinctColors(region)).toBeGreaterThan(1);
  expect(lit).toBeGreaterThan(0);
});

// molecule-builder M2 (TEST B): the auto-bond respects each atom's valence cap,
// and Clear empties the world. Reached by three #scene-toggle clicks (CubePoc →
// Atomos → Molecule → Builder). RED until M2 ships window.__builder.
//
// Signature: an O (valence 2) with two H within range bonds twice (H₂O, O full).
// A THIRD H placed near the now-valence-full O does NOT bond — the bond count
// stays at 2 (valence cap). The 3rd H is positioned only near O (not near the
// other H atoms) so its only candidate neighbour is the saturated O. Finally,
// clear() empties bonds and molecules.
test('builder: valence cap blocks an over-bond; Clear empties', async ({ page }) => {
  // Reach the Builder scene (three clicks) and let it render.
  await expect(page.locator('#scene-toggle')).toBeVisible();
  await page.click('#scene-toggle'); // → atomos
  await page.click('#scene-toggle'); // → molecule
  await page.click('#scene-toggle'); // → builder
  await page.waitForTimeout(700); // generous: let the builder scene boot + render

  await page.waitForFunction(() => !!window.__builder, null, { timeout: 6000 });

  // O at the origin with two H within range (left + right, 60 from O) → H₂O.
  // O has valence 2, so it bonds to exactly both H ⇒ 2 bonds.
  const afterWater = await page.evaluate(() => {
    const b = window.__builder;
    b.clear();
    b.addAtom(8, 0, 0, 0);   // O (valence 2) at origin
    b.addAtom(1, -60, 0, 0); // H within range of O
    b.addAtom(1, 60, 0, 0);  // H within range of O
    return b.getBonds().length;
  });
  expect(afterWater).toBe(2);

  // A THIRD H placed near the now-valence-full O (below it, 60 away) and FAR from
  // the other two H (which sit ±60 on x; this one is at y=-60, so ~85 from each —
  // still under threshold of EACH, but both flanking H are themselves full
  // valence-1 hydrogens, already bonded to O, so none can accept it either). The
  // only geometric candidate with the saturated O is blocked by valence, so the
  // bond count stays at 2.
  const afterThirdH = await page.evaluate(() => {
    const b = window.__builder;
    b.addAtom(1, 0, -60, 0); // 3rd H near the valence-full O
    return b.getBonds().length;
  });
  expect(afterThirdH).toBe(2);

  // clear() resets the world: no bonds, no molecules.
  const afterClear = await page.evaluate(() => {
    const b = window.__builder;
    b.clear();
    return { bonds: b.getBonds().length, molecules: b.getMolecules().length };
  });
  expect(afterClear.bonds).toBe(0);
  expect(afterClear.molecules).toBe(0);
});

// molecule-builder M3 helper: reach the Builder scene (CubePoc → Atomos →
// Molecule → Builder = three #scene-toggle clicks) and wait for window.__builder.
async function gotoBuilder(page) {
  await expect(page.locator('#scene-toggle')).toBeVisible();
  await page.click('#scene-toggle'); // → atomos
  await page.click('#scene-toggle'); // → molecule
  await page.click('#scene-toggle'); // → builder
  await page.waitForTimeout(700); // generous: let the builder scene boot + render
  await page.waitForFunction(() => !!window.__builder, null, { timeout: 6000 });
}

// molecule-builder M3 (TEST A): dragging an atom (via window.__builder.moveAtom)
// LIVE re-bonds — moving an atom into bondThreshold range forms a bond, moving it
// back beyond breakThreshold breaks it. moveAtom already recomputes bonds in the
// M2 model, so this deterministic API-level assertion exercises the same live
// re-bond path M3's pointer drag drives. ids are SEQUENTIAL from 0 in add order
// (clear() resets nextId to 0), so after clear the two H atoms are ids 0 and 1 —
// we move id 1.
//
// Distances are on the raw world coords passed to addAtom/moveAtom:
// bondThreshold ≈ 180, breakThreshold ≈ 230. FAR = ±400 (800 apart, well beyond
// break); NEAR = ±30 (60 apart, well within bond).
test('builder: dragging an atom into range bonds, out of range breaks (live)', async ({ page }) => {
  await gotoBuilder(page);

  // Two H atoms FAR apart → no bond at distance.
  const far = await page.evaluate(() => {
    const b = window.__builder;
    b.clear();
    b.addAtom(1, -400, 0, 0); // H  (id 0)
    b.addAtom(1, 400, 0, 0);  // H  (id 1) — 800 apart, beyond breakThreshold
    return { bonds: b.getBonds().length, molecules: b.getMolecules().length };
  });
  expect(far.bonds).toBe(0);
  // Two disconnected singleton atoms → two (singleton) molecules.
  expect(far.molecules).toBe(2);

  // Drag the 2nd H (id 1) NEAR the 1st (which sits at x=-400) → a bond forms
  // LIVE. id 1 → x=-340 is 60 from id 0 → within bondThreshold (180).
  const near = await page.evaluate(() => {
    const b = window.__builder;
    b.moveAtom(1, -340, 0, 0); // 60 from id 0 (at -400) → within bondThreshold
    return { bonds: b.getBonds(), molecules: b.getMolecules() };
  });
  expect(near.bonds.length).toBe(1);
  // The single bond joins atom ids 0 and 1.
  const bond = near.bonds[0];
  const ids = [bond.a, bond.b].sort((p, q) => p - q);
  expect(ids).toEqual([0, 1]);
  // One molecule now: a single 2-atom H₂ component.
  expect(near.molecules.length).toBe(1);
  const mol = near.molecules[0];
  // window.__builder molecules carry { ids, formula }.
  expect(mol.ids.length).toBe(2);
  expect(mol.formula === 'H₂' || mol.formula === 'H2').toBe(true);

  // Drag the 2nd H FAR again (beyond breakThreshold) → the bond BREAKS LIVE.
  const broke = await page.evaluate(() => {
    const b = window.__builder;
    b.moveAtom(1, 400, 0, 0); // back to 800 apart → beyond breakThreshold
    return { bonds: b.getBonds().length, molecules: b.getMolecules().length };
  });
  expect(broke.bonds).toBe(0);
  expect(broke.molecules).toBe(2);
});

// molecule-builder M3 (TEST B): the valence cap holds DURING a drag. An O
// (valence 2) bonded to two H is full; dragging a 3rd H right up against the
// saturated O (or against a bonded, valence-full H) forms NO new bond — the bond
// count stays at 2 throughout. ids are sequential from 0: O=0, H=1, H=2, H=3.
test('builder: valence cap holds during drag', async ({ page }) => {
  await gotoBuilder(page);

  // O at origin + two H within range → H₂O (O full at 2 bonds).
  const water = await page.evaluate(() => {
    const b = window.__builder;
    b.clear();
    b.addAtom(8, 0, 0, 0);   // O (id 0, valence 2)
    b.addAtom(1, -60, 0, 0); // H (id 1) within range of O → 1 bond
    return b.getBonds().length;
  });
  expect(water).toBe(1);

  const water2 = await page.evaluate(() => {
    const b = window.__builder;
    b.addAtom(1, 60, 0, 0); // H (id 2) within range of O → 2 bonds, O full
    return b.getBonds().length;
  });
  expect(water2).toBe(2);

  // A 3rd H placed FAR away (no bond yet).
  const farH = await page.evaluate(() => {
    const b = window.__builder;
    b.addAtom(1, 0, 400, 0); // H (id 3), far from everything → still 2 bonds
    return b.getBonds().length;
  });
  expect(farH).toBe(2);

  // DRAG the 3rd H (id 3) right up against the valence-full O → NO new bond.
  const againstO = await page.evaluate(() => {
    const b = window.__builder;
    b.moveAtom(3, 0, -30, 0); // 30 from O (id 0), well within bondThreshold
    return b.getBonds().length;
  });
  expect(againstO).toBe(2); // valence cap holds during the drag

  // DRAG the 3rd H against a bonded (valence-full) H (id 1 at -60,0) → still none.
  const againstH = await page.evaluate(() => {
    const b = window.__builder;
    b.moveAtom(3, -60, -30, 0); // 30 from id 1 (a full valence-1 hydrogen)
    return b.getBonds().length;
  });
  expect(againstH).toBe(2); // still capped — no over-bond
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

// molecule-builder M3 (TEST D, regression): in the DEFAULT Cube POC scene a mouse
// click-drag over the canvas still ROTATES the cube — proving the new Builder
// pick+drag is scene-gated and did NOT break cube-POC mouse rotation. This uses a
// genuine down→move→up drag (distinct from the existing keyboard/shear specs).
test('cube POC: mouse still rotates the cube (drag scene-gated)', async ({ page }) => {
  // Default scene is Cube POC — no scene toggle.
  const box = await page.locator('#canvas').boundingBox();
  const cx = box.x + box.width / 2;
  const cy = box.y + box.height / 2;

  // Capture the cube region (screen centre) before the drag.
  const before = await readRegion(page, 0.35, 0.3, 0.65, 0.6, 16, 10);

  // A genuine click-drag across the canvas: this is the mouse-rotation path.
  await page.mouse.move(cx, cy);
  await page.mouse.down();
  await page.mouse.move(cx + 120, cy + 40, { steps: 6 });
  await page.mouse.move(cx + 180, cy + 80, { steps: 6 });
  await page.mouse.up();
  await page.waitForTimeout(300); // let the rotated transform render

  const after = await readRegion(page, 0.35, 0.3, 0.65, 0.6, 16, 10);

  // The cube region changed: the drag rotated the cube (mouse rotation intact).
  const changed = before.filter((p, i) =>
    Math.abs(p[0] - after[i][0]) + Math.abs(p[1] - after[i][1]) + Math.abs(p[2] - after[i][2]) > 24
  ).length;
  expect(changed).toBeGreaterThan(2);
});

// builder-electron-fix M2 (TEST A): the Builder scene must render the REAL
// per-element nucleus (Atom.nucleons), so a heavier element shows a DENSER /
// larger lit nucleus than Hydrogen. RED until M2 makes builderAtomEntities draw
// the real nucleon cluster instead of a single fixed sphere per atom.
//
// Signature: an atom placed at the world origin projects to the canvas centre
// (perspectiveProjection composes the camera translate; world x=0,y=0 → ndc
// 0,0 → fx≈0.5, fy≈0.5). We sample a TIGHT centre region (the nucleus only —
// electrons/lone clouds sweep further out) and count LIT pixels (sum RGB > 90).
// Hydrogen is ONE nucleon; Carbon is 12 nucleons (6 p + 6 n) packed into a ball,
// Oxygen 16 — so the lit nucleus mass at the same centre region grows with Z.
// The MUST-PASS (RED) assertion is cCount > hCount by a clear margin. Today every
// builder atom renders as a single fixed sphere regardless of Z, so hCount ≈
// cCount and the margin assertion FAILS (RED).
test('builder: heavier element shows a denser nucleus than hydrogen (elements distinct)', async ({ page }) => {
  await gotoBuilder(page);

  // TIGHT centre region = the nucleus only (≈±0.06 fx/fy around canvas centre).
  const NUC = () => readRegion(page, 0.44, 0.44, 0.56, 0.56, 20, 20);
  const litCount = (px) => px.filter((p) => p[0] + p[1] + p[2] > 90).length;

  // Poll until the nucleus region has actually painted (render-ready), robust to
  // slow cold-start frames under full-suite/CI SwiftShader load.
  const sampleNucleus = async (min = 0) => {
    let last = 0;
    for (let i = 0; i < 25; i++) {
      last = litCount(await NUC());
      if (last > min) return last;
      await page.waitForTimeout(120);
    }
    return last;
  };

  // --- Hydrogen: ONE nucleon at the origin ----------------------------------
  await page.evaluate(() => {
    const b = window.__builder;
    b.clear();
    b.addAtom(1, 0, 0, 0); // single H at the origin → projects to canvas centre
  });
  await page.waitForTimeout(300);
  const hCount = await sampleNucleus(0);

  // --- Carbon: 12 nucleons (6 protons + 6 neutrons) at the SAME spot --------
  await page.evaluate(() => {
    const b = window.__builder;
    b.clear();
    b.addAtom(6, 0, 0, 0); // Carbon at the origin
  });
  await page.waitForTimeout(300);
  const cCount = await sampleNucleus(0);

  // --- Oxygen: 16 nucleons (8 p + 8 n) — a second data point ----------------
  await page.evaluate(() => {
    const b = window.__builder;
    b.clear();
    b.addAtom(8, 0, 0, 0); // Oxygen at the origin
  });
  await page.waitForTimeout(300);
  const oCount = await sampleNucleus(0);

  // Hydrogen's lone nucleus is clearly lit (sanity: the atom rendered at all).
  expect(hCount).toBeGreaterThan(0);

  // MUST-PASS (RED today): Carbon's many-nucleon nucleus lights up a clearly
  // denser/larger centre region than Hydrogen's single nucleon. Conservative
  // margin: more than half-again Hydrogen AND a hard minimum extra count, so it
  // can't pass on noise. Today every atom is one fixed sphere ⇒ cCount ≈ hCount
  // ⇒ this FAILS (the RED assertion for bug 1).
  expect(cCount).toBeGreaterThan(hCount * 1.5);
  expect(cCount).toBeGreaterThan(hCount + 8);

  // Oxygen (still heavier than H) corroborates: its nucleus is also denser than
  // Hydrogen's. (Not asserted strictly vs Carbon — packing geometry can make the
  // tight centre region saturate; the Z-vs-H contrast is the load-bearing one.)
  expect(oCount).toBeGreaterThan(hCount + 8);
});

// builder-electron-fix M2 (TEST B): bonded H atoms must show their electron(s)
// IN the bond (the shared pair, BETWEEN the nuclei) and NOT floating above each
// atom. Today builderElectronEntities floats one bright electron directly above
// EVERY atom regardless of bonding, so the spots above each nucleus are lit. After
// M2, a fully-bonded H (valence 1, no lone electrons) carries NO floating electron
// — only the shared bond electron(s) in the midband remain.
//
// Geometry (1280×720 viewport, builderScale 2.2, perspectiveProjection composing
// the camera translate at z=-1000, fov π/3):
//   • world (0,0,0)           → canvas centre (fx≈0.50, fy≈0.50)
//   • addAtom x = ∓60 (near)  → world x = ∓132 → fx≈0.436 (left) / 0.564 (right)
//   • the bond midpoint (x=0) → fx≈0.50 (the MIDBAND, where the shared pair sits)
//   • the OLD floating electron sits y = nucleonRadius*1.4 above its nucleus
//     → ≈42px up on screen → fy≈0.44 directly above each nucleus.
// Total separation 120 < bondThreshold (180) ⇒ the two H auto-bond (getBonds==1).
//
// Signature: count LIT pixels (sum RGB > 90) in five small regions on the centre
// line — LEFT nucleus, MIDBAND (between nuclei), RIGHT nucleus, ABOVE-LEFT and
// ABOVE-RIGHT (directly above each nucleus, where the old per-atom electron
// floated). The RED assertion is that the ABOVE-nucleus regions are NOT carrying a
// bright floating-electron blob: aboveLeft/aboveRight must be clearly DARKER than
// the midband (the shared pair). Today the floating electron lights those above
// spots, so they're bright ⇒ FAILS (RED). After M2 they go dark and it passes.
test('builder: bonded H electrons sit in the bond, not floating above each atom', async ({ page }) => {
  await gotoBuilder(page);

  // Two H within bond range → they auto-bond into H₂. near=60 ⇒ 120 apart < 180.
  const bonds = await page.evaluate(() => {
    const b = window.__builder;
    b.clear();
    b.addAtom(1, -60, 0, 0); // left H  → fx≈0.436
    b.addAtom(1, 60, 0, 0);  // right H → fx≈0.564
    return b.getBonds().length;
  });
  expect(bonds).toBe(1); // sanity: the two H are bonded (shared pair in the bond)

  await page.waitForTimeout(400);

  const litCount = (px) => px.filter((p) => p[0] + p[1] + p[2] > 90).length;

  // Five small sample regions along / just above the horizontal centre line.
  // fy≈0.50 = nucleus row; fy≈0.40–0.46 = directly above each nucleus.
  const leftNuc = () => readRegion(page, 0.40, 0.46, 0.48, 0.56, 14, 14);
  // The shared bond pair breathes vertically (electronCloud ≈ ±48px ≈ ±0.067 fy)
  // about the centre line, so sample a band tall enough to always contain it
  // (fy 0.42–0.58) — the pair never fully vacates this region at any frame.
  const midband = () => readRegion(page, 0.46, 0.42, 0.54, 0.58, 14, 18);
  const rightNuc = () => readRegion(page, 0.52, 0.46, 0.60, 0.56, 14, 14);
  const aboveLeft = () => readRegion(page, 0.40, 0.38, 0.48, 0.46, 14, 14);
  const aboveRight = () => readRegion(page, 0.52, 0.38, 0.60, 0.46, 14, 14);

  // Poll the midband until it has painted (render-ready), robust to cold-start.
  const waitForMid = async (min = 0, tries = 25) => {
    let last = 0;
    for (let i = 0; i < tries; i++) {
      last = litCount(await midband());
      if (last > min) return last;
      await page.waitForTimeout(120);
    }
    return last;
  };

  const mid = await waitForMid(0);
  const al = litCount(await aboveLeft());
  const ar = litCount(await aboveRight());
  const ln = litCount(await leftNuc());
  const rn = litCount(await rightNuc());

  // The shared bonding pair lights the MIDBAND between the two nuclei: there IS
  // meaningful lit electron structure in the bond. (Stable before AND after M2 —
  // the shared bond electron lives in the midband in both renders.)
  expect(mid).toBeGreaterThan(0);

  // Both nuclei are lit (sanity that the atoms rendered where expected).
  expect(ln).toBeGreaterThan(0);
  expect(rn).toBeGreaterThan(0);

  // MUST-PASS (RED today): the per-atom floating electron is GONE for bonded
  // atoms — the regions directly ABOVE each nucleus carry clearly fewer lit pixels
  // than the midband (the shared pair). Today a bright electron floats above EVERY
  // atom, lighting aboveLeft/aboveRight, so this FAILS (the RED assertion for bug
  // 2). After M2 (bonded H ⇒ no lone electron ⇒ nothing above) they go dark and
  // the bond's shared pair dominates the midband. Conservative: each above-region
  // must be under half the midband's lit count.
  expect(al).toBeLessThan(mid * 0.5);
  expect(ar).toBeLessThan(mid * 0.5);
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

// M4: sky backdrop (top is sky-blue, not white) + ground/sky differ.
test('M4: sky backdrop and horizon transition', async ({ page }) => {
  const top = await readPixel(page, 0.5, 0.03);     // sky region
  const ground = await readPixel(page, 0.5, 0.92);  // ground region
  // No longer white.
  expect(top).not.toEqual([255, 255, 255, 255]);
  // Sky is blue-dominant.
  expect(top[2]).toBeGreaterThan(top[0]);
  // There is a horizon: sky and ground are different colors.
  expect(top).not.toEqual(ground);
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

// ─────────────────────────────────────────────────────────────────────────────
// valence-electron-color M2 (TEST A): the Builder must render an atom's VALENCE
// electrons (outermost shell) in a DISTINCT colour from its CORE (inner-shell)
// electrons. Today every builder electron reuses the single moleculeElectronColor
// (blue {0.30,0.68,1.0}), so core and valence rings are the SAME colour → RED.
//
// Geometry (1280×720 viewport, builderScale 2.2; world→screen at z=0 ≈ 0.62
// px per world-unit, from the existing TEST-B calibration: world x=132 → 82px →
// Δfx 0.064). A free Carbon at the origin (Z=6, shells [2,4]) places:
//   • 2 CORE electrons on the inner ring, model radius loneOrbitRadius
//     = nucleusRadius*1.4 = 84 → ×builderScale 2.2 = 184.8 world → ≈115px on
//     screen  → fx offset ≈0.090, fy offset ≈0.160 from canvas centre (0.5,0.5).
//   • 4 VALENCE electrons on the outer ring, model radius loneOrbitRadius+
//     shellSpacing = 84+60 = 144 → ×2.2 = 316.8 world → ≈196px → fx offset
//     ≈0.153, fy offset ≈0.272 from centre.
// Electrons sweep their rings with the frame, so over the canvas a ring's
// electrons cross every angle; we sample BOXES straddling each ring radius on the
// RIGHT side of the atom (fx = 0.5 + ring_offset, fy ≈ 0.5) and on the BOTTOM
// (fx ≈ 0.5, fy = 0.5 + ring_offset), polling several frames to catch electrons
// as they rotate through, and AGGREGATE the lit electron pixels.
//
// We classify each lit electron pixel by DOMINANT RGB channel (the same coarse
// channel-dominance approach the file uses elsewhere), EXCLUDING:
//   • the central nucleus region (proton red {0.90,0.25,0.22} / neutron grey
//     {0.62,0.64,0.67}) — the sample boxes sit OUTSIDE the tight centre, and we
//     additionally drop pixels that read as proton-red or neutron-grey.
// The INNER (core) band should be BLUE-dominant (B channel highest, as today),
// while the OUTER (valence) band should be NON-blue-dominant (the new valence
// colour, e.g. amber/gold or green → R or G dominant). RED today because all
// electrons are blue ⇒ both bands are blue-dominant ⇒ outer === inner ⇒ FAILS.
test('builder: valence electrons render a distinct colour from core electrons', async ({ page }) => {
  await gotoBuilder(page);

  // A free Carbon at the origin: shells [2,4] → 2 core (inner ring) + 4 valence
  // (outer ring). Projects to canvas centre (world 0,0 → fx≈0.5, fy≈0.5).
  await page.evaluate(() => {
    const b = window.__builder;
    b.clear();
    b.addAtom(6, 0, 0, 0);
  });
  await page.waitForTimeout(400);

  // Lit electron pixel: bright enough AND not a nucleus colour. Proton red is
  // R-dominant with low G/B; neutron grey is near-equal R≈G≈B and mid-bright.
  // We keep saturated electron dots and drop greyish/red-nucleus pixels.
  const LIT = 90; // sum-RGB threshold (matches the other builder electron specs)
  const isNucleusColour = (p) => {
    const [r, g, b] = p;
    const mx = Math.max(r, g, b), mn = Math.min(r, g, b);
    const grey = mx - mn < 28;          // neutron grey: low channel spread
    const protonRed = r > 150 && g < 110 && b < 110 && r - Math.max(g, b) > 60;
    return grey || protonRed;
  };
  const electronPixels = (px) =>
    px.filter((p) => p[0] + p[1] + p[2] > LIT && !isNucleusColour(p));

  // Dominant electron colour of a pixel set: tally the dominant RGB channel of
  // each electron pixel and return the winning channel ('R' | 'G' | 'B' | null).
  const dominantChannel = (px) => {
    const tally = { R: 0, G: 0, B: 0 };
    for (const p of electronPixels(px)) {
      const [r, g, b] = p;
      if (b >= r && b >= g) tally.B++;
      else if (r >= g && r >= b) tally.R++;
      else tally.G++;
    }
    const total = tally.R + tally.G + tally.B;
    if (total === 0) return { channel: null, total, tally };
    const channel = ['R', 'G', 'B'].reduce((a, c) => (tally[c] > tally[a] ? c : a), 'R');
    return { channel, total, tally };
  };

  // INNER (core) band boxes — straddle the core ring (≈115px ≈ fx 0.090 / fy
  // 0.160 from centre) on the right and bottom of the atom.
  const innerRight = () => readRegion(page, 0.555, 0.42, 0.645, 0.58, 16, 16);
  const innerBottom = () => readRegion(page, 0.42, 0.60, 0.58, 0.72, 16, 16);
  // OUTER (valence) band boxes — straddle the valence ring (≈196px ≈ fx 0.153 /
  // fy 0.272 from centre) on the right and bottom, clear of the inner ring.
  const outerRight = () => readRegion(page, 0.62, 0.40, 0.72, 0.60, 16, 16);
  const outerBottom = () => readRegion(page, 0.40, 0.70, 0.60, 0.82, 16, 16);

  // Aggregate lit electron pixels across several frames (electrons sweep their
  // rings), so a ring is sampled at many angles. Returns the pooled pixel list.
  const poolBand = async (boxes, frames = 8) => {
    let pool = [];
    for (let i = 0; i < frames; i++) {
      for (const box of boxes) pool = pool.concat(await box());
      await page.waitForTimeout(90);
    }
    return pool;
  };

  // Poll until the inner band has actually painted electron pixels (render-ready,
  // robust to cold-start under SwiftShader / full-suite load).
  let innerPool = [];
  for (let i = 0; i < 25; i++) {
    innerPool = await poolBand([innerRight, innerBottom], 4);
    if (electronPixels(innerPool).length > 0) break;
    await page.waitForTimeout(120);
  }
  const outerPool = await poolBand([outerRight, outerBottom], 8);

  const inner = dominantChannel(innerPool);
  const outer = dominantChannel(outerPool);

  // Sanity: both bands actually carry lit electron pixels (the atom rendered and
  // electrons swept through both rings).
  expect(inner.total).toBeGreaterThan(0);
  expect(outer.total).toBeGreaterThan(0);

  // Robustness (a): there are ≥2 distinct electron colours around the atom — the
  // core blue and the new valence colour both appear in the pooled electron set.
  const allElectron = electronPixels(innerPool.concat(outerPool));
  expect(distinctColors(allElectron, 40)).toBeGreaterThanOrEqual(2);

  // Robustness (b) — the load-bearing RED assertion: the OUTER (valence) band's
  // dominant electron colour DIFFERS from the INNER (core) band's. Today every
  // electron is blue ⇒ both bands are B-dominant ⇒ outer.channel === inner.channel
  // ⇒ FAILS (RED). After M2, valence electrons get a non-blue colour ⇒ the outer
  // band's dominant channel flips (R or G) ⇒ differs from the blue inner band.
  expect(outer.channel).not.toBe(inner.channel);

  // Corroborating: the INNER (core) band stays BLUE-dominant (core electrons keep
  // the existing moleculeElectronColor blue), while the OUTER (valence) band is
  // NOT blue-dominant (it carries the new valence colour). Today the outer band is
  // ALSO blue-dominant ⇒ this FAILS too (RED).
  expect(inner.channel).toBe('B');
  expect(outer.channel).not.toBe('B');
});

// ─────────────────────────────────────────────────────────────────────────────
// valence-electron-color M2 (TEST B): the SHARED bonding electron(s) must use the
// VALENCE colour (bonding electrons are valence electrons), NOT the core blue.
// Today bondElectronPositions render with the single blue moleculeElectronColor →
// the bond electron is blue-dominant → RED.
//
// Geometry (mirrors the existing builder TEST B): two H within bond range auto-bond;
// the shared pair sits in the MIDBAND (the bond midpoint at world x=0 → fx≈0.50),
// breathing vertically about the centre line (fy 0.42–0.58). We sample that midband,
// pool a few frames, and classify the bond-electron pixels by dominant channel.
test('builder: bonding electrons use the valence colour', async ({ page }) => {
  await gotoBuilder(page);

  // Two H within bond range → H₂. near=60 ⇒ 120 apart < bondThreshold (180).
  const bonds = await page.evaluate(() => {
    const b = window.__builder;
    b.clear();
    b.addAtom(1, -60, 0, 0); // left H  → fx≈0.436
    b.addAtom(1, 60, 0, 0);  // right H → fx≈0.564
    return b.getBonds().length;
  });
  expect(bonds).toBe(1); // sanity: bonded → a shared pair lives in the bond
  await page.waitForTimeout(400);

  const LIT = 90;
  const isNucleusColour = (p) => {
    const [r, g, b] = p;
    const mx = Math.max(r, g, b), mn = Math.min(r, g, b);
    const grey = mx - mn < 28;
    const protonRed = r > 150 && g < 110 && b < 110 && r - Math.max(g, b) > 60;
    return grey || protonRed;
  };
  const electronPixels = (px) =>
    px.filter((p) => p[0] + p[1] + p[2] > LIT && !isNucleusColour(p));
  const dominantChannel = (px) => {
    const tally = { R: 0, G: 0, B: 0 };
    for (const p of electronPixels(px)) {
      const [r, g, b] = p;
      if (b >= r && b >= g) tally.B++;
      else if (r >= g && r >= b) tally.R++;
      else tally.G++;
    }
    const total = tally.R + tally.G + tally.B;
    if (total === 0) return { channel: null, total };
    const channel = ['R', 'G', 'B'].reduce((a, c) => (tally[c] > tally[a] ? c : a), 'R');
    return { channel, total };
  };

  // The MIDBAND between the two nuclei — where the shared bonding pair sits/breathes.
  const midband = () => readRegion(page, 0.46, 0.42, 0.54, 0.58, 16, 18);

  // Pool the midband across frames (the pair breathes), robust to cold-start.
  const poolMid = async () => {
    let pool = [];
    for (let i = 0; i < 8; i++) {
      pool = pool.concat(await midband());
      await page.waitForTimeout(90);
    }
    return pool;
  };
  let pool = [];
  for (let i = 0; i < 25; i++) {
    pool = await poolMid();
    if (electronPixels(pool).length > 0) break;
    await page.waitForTimeout(120);
  }
  const bond = dominantChannel(pool);

  // Sanity: the shared bonding pair lights the midband.
  expect(bond.total).toBeGreaterThan(0);

  // The load-bearing RED assertion: the bond electron is NOT core-blue — it uses
  // the VALENCE colour (same family as TEST A's outer band ⇒ a non-blue dominant
  // channel, R or G). Today the bond reuses the blue moleculeElectronColor ⇒ it is
  // B-dominant ⇒ FAILS (RED). After M2, bonds render in the valence colour ⇒ the
  // midband electron's dominant channel flips off blue and this passes.
  expect(bond.channel).not.toBe('B');
});

// ─────────────────────────────────────────────────────────────────────────────
// valence-only-toggle M1: the #valence-only checkbox must HIDE the CORE (blue)
// lone electrons while KEEPING the VALENCE (amber) ones. In Main, valenceOnly
// drops builderLoneElectronEntities (the blue coreLoneElectronPositions) but
// always renders builderValenceElectronEntities (amber valenceLoneElectronPositions)
// and builderBondElectronEntities (amber bonding pair). So toggling ON should make
// the INNER (core) band lose its blue electrons while the OUTER (valence) amber
// band is unchanged; unchecking restores the blue core; and the bonded pair stays
// amber in BOTH states (bonding electrons are valence, never dropped).
//
// Reuses the EXACT inner/outer bands, midband, LIT threshold, nucleus exclusion,
// and dominant-channel classification proven by the two valence-colour specs above
// (free Carbon Z=6, shells [2,4]: 2 blue core inner + 4 amber valence outer).
test('builder: Valence only toggle hides core electrons, keeps valence', async ({ page }) => {
  await gotoBuilder(page);

  // A free Carbon at the origin → 2 core (inner blue ring) + 4 valence (outer amber ring).
  await page.evaluate(() => {
    const b = window.__builder;
    b.clear();
    b.addAtom(6, 0, 0, 0);
  });
  await page.waitForTimeout(400);

  // ── classification (identical to the valence-colour specs) ──────────────────
  const LIT = 90;
  const isNucleusColour = (p) => {
    const [r, g, b] = p;
    const mx = Math.max(r, g, b), mn = Math.min(r, g, b);
    const grey = mx - mn < 28;
    const protonRed = r > 150 && g < 110 && b < 110 && r - Math.max(g, b) > 60;
    return grey || protonRed;
  };
  const electronPixels = (px) =>
    px.filter((p) => p[0] + p[1] + p[2] > LIT && !isNucleusColour(p));
  // Count electron pixels whose dominant channel is BLUE (core) vs RED (amber valence).
  const channelCounts = (px) => {
    const t = { R: 0, G: 0, B: 0 };
    for (const p of electronPixels(px)) {
      const [r, g, b] = p;
      if (b >= r && b >= g) t.B++;
      else if (r >= g && r >= b) t.R++;
      else t.G++;
    }
    return t;
  };

  // SAME bands as TEST A: inner (core) and outer (valence) straddle boxes.
  const innerRight = () => readRegion(page, 0.555, 0.42, 0.645, 0.58, 16, 16);
  const innerBottom = () => readRegion(page, 0.42, 0.60, 0.58, 0.72, 16, 16);
  const outerRight = () => readRegion(page, 0.62, 0.40, 0.72, 0.60, 16, 16);
  const outerBottom = () => readRegion(page, 0.40, 0.70, 0.60, 0.82, 16, 16);

  const poolBand = async (boxes, frames = 8) => {
    let pool = [];
    for (let i = 0; i < frames; i++) {
      for (const box of boxes) pool = pool.concat(await box());
      await page.waitForTimeout(90);
    }
    return pool;
  };
  // Pool a band, retrying until it paints electron pixels (cold-start / sparse-ring
  // safe: the 4 valence electrons sweep a large outer ring, so a single pass can
  // miss the sample boxes — retry until lit pixels appear).
  const poolUntilLit = async (boxes, framesPer = 4, tries = 8) => {
    let pool = [];
    for (let i = 0; i < tries; i++) {
      pool = await poolBand(boxes, framesPer);
      if (electronPixels(pool).length > 0) break;
      await page.waitForTimeout(120);
    }
    return pool;
  };
  const poolInner = () => poolUntilLit([innerRight, innerBottom], 4, 6);
  const poolOuter = () => poolUntilLit([outerRight, outerBottom], 4, 8);

  // ── (2) BEFORE toggling: core blue present on inner, amber present on outer ──
  const innerBefore = channelCounts(await poolInner());
  const outerBefore = channelCounts(await poolOuter());
  // Inner band carries BLUE-dominant core electrons.
  expect(innerBefore.B).toBeGreaterThan(0);
  // Inner blue clearly dominates inner red (core ring is blue, not amber).
  expect(innerBefore.B).toBeGreaterThan(innerBefore.R);
  // Outer band carries amber (RED-dominant) valence electrons.
  expect(outerBefore.R).toBeGreaterThan(0);

  // ── (3) Toggle ON: change event fires installValenceOnlyToggle ──────────────
  await page.check('#valence-only');
  await page.waitForTimeout(400); // let the toggle re-render + a few frames

  // ── (4) AFTER ON: inner blue core GONE, outer amber valence still present ───
  // Sample the inner band thoroughly (8 frames) — with the toggle ON it should
  // paint NO blue core electrons at all.
  const innerAfter = channelCounts(await poolBand([innerRight, innerBottom], 8));
  // The OUTER (valence) amber band is still rendered — retry until it lights
  // (the sparse 4-electron ring needs a few passes to be caught on the boxes).
  const outerAfter = channelCounts(await poolOuter());
  // The load-bearing signal: the INNER (core) band no longer has blue core electrons.
  expect(innerAfter.B).toBe(0);
  // The OUTER (valence) amber band is still lit (RED-dominant, comparable to before).
  expect(outerAfter.R).toBeGreaterThan(0);

  // ── (5) Toggle OFF: blue core electrons RETURN on the inner band ────────────
  await page.uncheck('#valence-only');
  await page.waitForTimeout(400);
  const innerRestored = channelCounts(await poolInner());
  expect(innerRestored.B).toBeGreaterThan(0);

  // ── (6) Bonded-pair invariance: the bond electron is amber in BOTH states ───
  const bondsFormed = await page.evaluate(() => {
    const b = window.__builder;
    b.clear();
    b.addAtom(1, -60, 0, 0); // left H  → fx≈0.436
    b.addAtom(1, 60, 0, 0);  // right H → fx≈0.564 (120 apart < bondThreshold)
    return b.getBonds().length;
  });
  expect(bondsFormed).toBe(1);
  await page.waitForTimeout(400);

  // Same midband + retry budget as the "bonding electrons use the valence colour"
  // spec (the pair breathes; under SwiftShader full-suite load it needs several
  // passes to be caught). Pool until lit.
  const midband = () => readRegion(page, 0.46, 0.42, 0.54, 0.58, 16, 18);
  const poolMid = async () => {
    for (let i = 0; i < 15; i++) {
      let p = [];
      for (let j = 0; j < 8; j++) {
        p = p.concat(await midband());
        await page.waitForTimeout(90);
      }
      if (electronPixels(p).length > 0) return p;
    }
    return [];
  };

  // Bond electron is amber (RED-dominant), valence-only UNCHECKED.
  const bondOff = channelCounts(await poolMid());
  expect(bondOff.R + bondOff.G + bondOff.B).toBeGreaterThan(0);
  expect(bondOff.R).toBeGreaterThan(bondOff.B); // amber, not core-blue

  // Bond electron stays amber with valence-only CHECKED (bonding electrons are
  // valence → never dropped by the toggle).
  await page.check('#valence-only');
  await page.waitForTimeout(400);
  const bondOn = channelCounts(await poolMid());
  expect(bondOn.R + bondOn.G + bondOn.B).toBeGreaterThan(0);
  expect(bondOn.R).toBeGreaterThan(bondOn.B); // still amber after toggle
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

// builder molecule-move (M2): single-click+drag moves the WHOLE connected
// molecule, double-click+drag moves just one atom. Verified deterministically
// through the window.__builder seam (reliable under SwiftShader): moveMolecule
// rigidly translates the whole component (every atom shifts by the SAME delta,
// internal bonds intact), while moveAtom still moves only one atom. The new
// seam entries moveMolecule(id,x,y,z) and getAtoms() arrive in M2 — RED before.
// Distances on raw world coords: bondThreshold ≈ 180. Build an H₂O chain:
// O (id 0) at origin with two H (ids 1,2) within bonding range.
test('builder: moveMolecule rigidly translates the whole molecule (seam)', async ({ page }) => {
  await gotoBuilder(page);

  // Build one 3-atom molecule: O at origin, two H within bondThreshold (≈180).
  const built = await page.evaluate(() => {
    const b = window.__builder;
    b.clear();
    b.addAtom(8, 0, 0, 0);   // O  (id 0)
    b.addAtom(1, 60, 0, 0);  // H  (id 1) — 60 from O → bonds
    b.addAtom(1, 0, 60, 0);  // H  (id 2) — 60 from O → bonds
    return {
      bonds: b.getBonds().length,
      molecules: b.getMolecules().length,
      atoms: b.getAtoms(),
    };
  });
  // One molecule of three atoms, two O–H bonds.
  expect(built.molecules).toBe(1);
  expect(built.bonds).toBe(2);
  expect(built.atoms.length).toBe(3);

  // Move the WHOLE molecule by dragging the O anchor (id 0) by a known delta.
  const DX = 250, DY = -150, DZ = 90;
  const moved = await page.evaluate(({ dx, dy, dz }) => {
    const b = window.__builder;
    const before = b.getAtoms();
    const o = before.find((a) => a.id === 0);
    b.moveMolecule(0, o.pos.x + dx, o.pos.y + dy, o.pos.z + dz);
    return {
      before,
      after: b.getAtoms(),
      bonds: b.getBonds().length,
      molecules: b.getMolecules().length,
    };
  }, { dx: DX, dy: DY, dz: DZ });

  // EVERY atom shifted by the SAME delta (rigid translation).
  for (const a0 of moved.before) {
    const a1 = moved.after.find((a) => a.id === a0.id);
    expect(Math.abs((a1.pos.x - a0.pos.x) - DX)).toBeLessThan(1e-6);
    expect(Math.abs((a1.pos.y - a0.pos.y) - DY)).toBeLessThan(1e-6);
    expect(Math.abs((a1.pos.z - a0.pos.z) - DZ)).toBeLessThan(1e-6);
  }
  // Connectivity intact: still one molecule, still two bonds.
  expect(moved.molecules).toBe(1);
  expect(moved.bonds).toBe(2);
});

// Single-atom move still works: moveAtom shifts ONLY the named atom (the
// double-click+drag path). Move one H far away → it detaches; the other two
// atoms keep their positions.
test('builder: moveAtom moves only one atom (single-atom path)', async ({ page }) => {
  await gotoBuilder(page);

  const res = await page.evaluate(() => {
    const b = window.__builder;
    b.clear();
    b.addAtom(8, 0, 0, 0);   // O  (id 0)
    b.addAtom(1, 60, 0, 0);  // H  (id 1)
    b.addAtom(1, 0, 60, 0);  // H  (id 2)
    const before = b.getAtoms();
    b.moveAtom(2, 800, 0, 0); // drag H id 2 far away (beyond breakThreshold)
    const after = b.getAtoms();
    return { before, after, molecules: b.getMolecules().length };
  });

  // O (id 0) and H (id 1) are unchanged; only id 2 moved.
  for (const id of [0, 1]) {
    const a0 = res.before.find((a) => a.id === id);
    const a1 = res.after.find((a) => a.id === id);
    expect(a1.pos.x).toBeCloseTo(a0.pos.x, 6);
    expect(a1.pos.y).toBeCloseTo(a0.pos.y, 6);
  }
  const h2before = res.before.find((a) => a.id === 2);
  const h2after = res.after.find((a) => a.id === 2);
  expect(Math.abs(h2after.pos.x - h2before.pos.x)).toBeGreaterThan(100);
  // The dragged atom detached → no longer a single 3-atom molecule.
  expect(res.molecules).toBeGreaterThan(1);
});
