// Builder valence-vs-core electron colour + bonding-pair colour + Valence-only
// toggle specs.
// Split out of the original e2e/world.spec.js (behaviour-frozen reorganisation).
import { test } from '@playwright/test';
import {
  expect, waitForRenderedCanvas, openDrawer, readRegion, distinctColors, gotoBuilder,
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

// ─────────────────────────────────────────────────────────────────────────────
// valence-electron-color M2 (TEST A): the Builder must render an atom's VALENCE
// electrons (outermost shell) in a DISTINCT colour from its CORE (inner-shell)
// electrons. Today every builder electron reuses the single moleculeElectronColor
// (blue {0.30,0.68,1.0}), so core and valence rings are the SAME colour → RED.
//
// Geometry (1280×720 viewport, effectiveScale = builderScale×layerSpace = 2.2×1.6
// = 3.52; projectModel mirrors builder-atom-visuals.spec.js). A free Carbon at
// the origin (Z=6, shells [2,4]) places:
//   • 2 CORE electrons on the inner ring, model radius loneOrbitRadius
//     = nucleusRadius*1.4 = 84 → effective world 84×3.52 = 295.7 → via perspective
//     (f/aspect × world / 1000) → fx offset ≈0.144 from centre (0.5).
//     On-screen positions: right side at fx≈0.644, top side at fy≈0.244.
//   • 4 VALENCE electrons on the outer ring, model radius loneOrbitRadius+
//     shellSpacing = 84+60 = 144 → effective world 144×3.52 = 506.9 → fx offset
//     ≈0.247 from centre. On-screen: right at fx≈0.747, top at fy≈0.061.
// Electrons sweep their rings with the frame, so over the canvas a ring's
// electrons cross every angle; we sample BOXES straddling each ring radius on the
// RIGHT side of the atom (fx = 0.5 + ring_offset, fy ≈ 0.5) and on the TOP
// (fx ≈ 0.5, fy = 0.5 − ring_offset), polling several frames to catch electrons
// as they rotate through, and AGGREGATE the lit electron pixels.
// (The BOTTOM of the atom is off-canvas for the outer ring at the effective scale
// 3.52, so we sample the TOP instead — same ring, symmetric coverage.)
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

  // INNER (core) band boxes — straddle the core ring (model r=84, effective scale
  // 3.52 → fx offset ≈0.144 from centre: right at fx≈0.644, top at fy≈0.244).
  const innerRight = () => readRegion(page, 0.60, 0.455, 0.69, 0.545, 16, 16);
  const innerTop   = () => readRegion(page, 0.46, 0.20, 0.54, 0.28, 16, 16);
  // OUTER (valence) band boxes — straddle the valence ring (model r=144, effective
  // scale 3.52 → fx offset ≈0.247: right at fx≈0.747, top at fy≈0.061). The bottom
  // of the outer ring falls near/beyond the canvas edge at this scale, so we sample
  // the TOP of the atom (symmetric coverage, box clamped at fy=0.0).
  const outerRight = () => readRegion(page, 0.70, 0.455, 0.79, 0.545, 16, 16);
  const outerTop   = () => readRegion(page, 0.46, 0.00, 0.54, 0.12, 16, 16);

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
    innerPool = await poolBand([innerRight, innerTop], 4);
    if (electronPixels(innerPool).length > 0) break;
    await page.waitForTimeout(120);
  }
  const outerPool = await poolBand([outerRight, outerTop], 8);

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

  // SAME bands as TEST A: inner (core) and outer (valence) straddle boxes,
  // updated for EFFECTIVE_SCALE = 3.52 (builderScale 2.2 × layerSpace 1.6).
  // Inner ring (model r=84): right at fx≈0.644, top at fy≈0.244.
  // Outer ring (model r=144): right at fx≈0.747, top at fy≈0.061.
  // (Bottom samples replaced by top samples — at scale 3.52 the outer ring bottom
  // is near/beyond the canvas lower edge, so the symmetric top is used instead.)
  const innerRight = () => readRegion(page, 0.60, 0.455, 0.69, 0.545, 16, 16);
  const innerTop   = () => readRegion(page, 0.46, 0.20, 0.54, 0.28, 16, 16);
  const outerRight = () => readRegion(page, 0.70, 0.455, 0.79, 0.545, 16, 16);
  const outerTop   = () => readRegion(page, 0.46, 0.00, 0.54, 0.12, 16, 16);

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
  const poolInner = () => poolUntilLit([innerRight, innerTop], 4, 6);
  const poolOuter = () => poolUntilLit([outerRight, outerTop], 4, 8);

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
  const innerAfter = channelCounts(await poolBand([innerRight, innerTop], 8));
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
