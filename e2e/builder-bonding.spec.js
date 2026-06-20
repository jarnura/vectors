// Builder auto-bonding / valence-cap / live bond-break / element-distinct /
// electron-placement specs.
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
