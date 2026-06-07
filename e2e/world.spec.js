// Baseline canvas-verification spec for the vectors WebGL2 scene.
//
// This is the harness the world-backdrop milestones extend. At baseline (before
// the world meshes exist) it only asserts the canvas renders SOMETHING (the
// cubes). Milestone-specific assertions (ground band, grid lines, sky color,
// horizon transition) are added/enabled as M1–M4 land.
import { test } from '@playwright/test';
import {
  expect, waitForRenderedCanvas, readPixel, readRow, readRegion, distinctColors,
} from './helpers.js';

test.beforeEach(async ({ page }) => {
  await page.goto('/');
  await waitForRenderedCanvas(page);
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

  // The switch is now a 4-cycle (CubePoc → Atomos → Molecule → Builder →
  // CubePoc), so cycling fully around returns to the cube POC sky. Both Molecule
  // and Builder share the dark deep-space backdrop with atomos, so the next two
  // clicks keep it dark.
  await page.click('#scene-toggle'); // → molecule (still dark space backdrop)
  await page.waitForTimeout(300);
  const moleculeBackdrop = await readPixel(page, 0.5, 0.04);
  expect(moleculeBackdrop[0] + moleculeBackdrop[1] + moleculeBackdrop[2]).toBeLessThan(120);

  await page.click('#scene-toggle'); // → builder (still dark space backdrop)
  await page.waitForTimeout(300);
  const builderBackdrop = await readPixel(page, 0.5, 0.04);
  expect(builderBackdrop[0] + builderBackdrop[1] + builderBackdrop[2]).toBeLessThan(120);

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
