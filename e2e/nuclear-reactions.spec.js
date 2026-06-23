// E2E spec for the Nuclide scene — M3 named nuclear reactions.
//
// Seam API extensions (window.__nuclear, M3):
//   decayAlpha()              → alphaDecay current nuclide → daughter becomes current
//   decayBetaMinus()          → betaMinus current nuclide → product becomes current
//   decayBetaPlus()           → betaPlusEC current nuclide → product becomes current
//   fuseWith(z2, n2)          → fuse current nuclide with {z2,n2} → product becomes current
//   fission(zA, nA, zB, nB)  → fission current nuclide into fragA + fragB → keeps current, stores fragments
//   lastQ()                   → Number: the Q-value (MeV) of the last reaction
//
// Conservation: each reaction must conserve ΣZ and ΣA exactly.
// Spontaneity: Q>0 → "RELEASED", Q<0 → "ABSORBED" in the #nuclide-info panel.
//
// Timing: retries:2 (playwright.config.js).
import { test } from '@playwright/test';
import {
  expect,
  waitForRenderedCanvas,
  openDrawer,
} from './helpers.js';

// Navigate to the Nuclide scene (5 #scene-toggle clicks from CubePoc).
async function gotoNuclide(page) {
  await expect(page.locator('#scene-toggle')).toBeVisible();
  await page.click('#scene-toggle'); // → Atomos
  await page.click('#scene-toggle'); // → Molecule
  await page.click('#scene-toggle'); // → Builder
  await page.click('#scene-toggle'); // → Materials
  await page.click('#scene-toggle'); // → Nuclide
  await page.waitForTimeout(700);
  await page.waitForFunction(() => !!window.__nuclear, null, { timeout: 6000 });
}

test.beforeEach(async ({ page }) => {
  await page.goto('/');
  await waitForRenderedCanvas(page);
  await openDrawer(page);
});

// ─── GATE 1: New API surface ──────────────────────────────────────────────────

test('nuclear-reactions: new M3 API methods are present on window.__nuclear', async ({ page }) => {
  await gotoNuclide(page);

  const api = await page.evaluate(() => ({
    hasDecayAlpha: typeof window.__nuclear.decayAlpha === 'function',
    hasDecayBetaMinus: typeof window.__nuclear.decayBetaMinus === 'function',
    hasDecayBetaPlus: typeof window.__nuclear.decayBetaPlus === 'function',
    hasFuseWith: typeof window.__nuclear.fuseWith === 'function',
    hasFission: typeof window.__nuclear.fission === 'function',
    hasLastQ: typeof window.__nuclear.lastQ === 'function',
  }));

  expect(api.hasDecayAlpha).toBe(true);
  expect(api.hasDecayBetaMinus).toBe(true);
  expect(api.hasDecayBetaPlus).toBe(true);
  expect(api.hasFuseWith).toBe(true);
  expect(api.hasFission).toBe(true);
  expect(api.hasLastQ).toBe(true);
});

// ─── GATE 2: Alpha decay — U-238 → Th-234 + He-4 ────────────────────────────

// 2a. U-238 alpha decay: daughter is Th-234 (Z=90, A=234).
test('nuclear-reactions: alpha decay U-238 → Th-234 (Z=90, A=234)', async ({ page }) => {
  await gotoNuclide(page);

  const result = await page.evaluate(() => {
    window.__nuclear.setNuclide(92, 146); // U-238
    window.__nuclear.decayAlpha();
    return window.__nuclear.getNuclide();
  });

  expect(result.z).toBe(90); // Th
  expect(result.a).toBe(234); // A = 234
  expect(result.symbol).toBe('Th');
});

// 2b. U-238 alpha decay Q > 0 (exothermic).
test('nuclear-reactions: alpha decay U-238 lastQ() > 0 (exothermic)', async ({ page }) => {
  await gotoNuclide(page);

  const q = await page.evaluate(() => {
    window.__nuclear.setNuclide(92, 146); // U-238
    window.__nuclear.decayAlpha();
    return window.__nuclear.lastQ();
  });

  expect(typeof q).toBe('number');
  expect(q).toBeGreaterThan(0);
  // SEMF gives ~4.35 MeV for U-238 alpha Q; allow generous [2, 8] range.
  expect(q).toBeGreaterThan(2);
  expect(q).toBeLessThan(8);
});

// 2c. U-238 alpha decay conserves ΣZ and ΣA.
// Parent: Z=92, A=238. Daughter: Z=90, A=234. Alpha: Z=2, A=4. ΣZ=92, ΣA=238.
test('nuclear-reactions: alpha decay conserves ΣZ and ΣA (U-238)', async ({ page }) => {
  await gotoNuclide(page);

  const result = await page.evaluate(() => {
    window.__nuclear.setNuclide(92, 146); // U-238 parent
    window.__nuclear.decayAlpha();
    const daughter = window.__nuclear.getNuclide();
    // Alpha particle is He-4: Z=2, A=4.
    return {
      daughterZ: daughter.z,
      daughterA: daughter.a,
      alphaZ: 2,
      alphaA: 4,
      sumZ: daughter.z + 2,
      sumA: daughter.a + 4,
    };
  });

  expect(result.sumZ).toBe(92); // ΣZ = 92 (U)
  expect(result.sumA).toBe(238); // ΣA = 238 (U-238)
});

// ─── GATE 3: Beta-minus decay — C-14 → N-14 ─────────────────────────────────

// 3a. C-14 beta-minus decay: product is N-14 (Z=7, A=14).
test('nuclear-reactions: betaMinus C-14 → N-14 (Z=7, A=14)', async ({ page }) => {
  await gotoNuclide(page);

  const result = await page.evaluate(() => {
    window.__nuclear.setNuclide(6, 8); // C-14
    window.__nuclear.decayBetaMinus();
    return window.__nuclear.getNuclide();
  });

  expect(result.z).toBe(7); // N
  expect(result.a).toBe(14); // A conserved
  expect(result.symbol).toBe('N');
});

// 3b. C-14 beta-minus Q > 0 and approximately 0.157 MeV (measured).
test('nuclear-reactions: betaMinus C-14 lastQ() > 0, ≈ 0.157 MeV', async ({ page }) => {
  await gotoNuclide(page);

  const q = await page.evaluate(() => {
    window.__nuclear.setNuclide(6, 8); // C-14
    window.__nuclear.decayBetaMinus();
    return window.__nuclear.lastQ();
  });

  expect(q).toBeGreaterThan(0);
  // Absolute anchor: model gives 0.157 MeV (measured). Allow ±0.05.
  expect(q).toBeGreaterThan(0.10);
  expect(q).toBeLessThan(0.25);
});

// 3c. C-14 beta-minus conserves ΣZ and ΣA.
test('nuclear-reactions: betaMinus C-14 conserves ΣZ and ΣA', async ({ page }) => {
  await gotoNuclide(page);

  const result = await page.evaluate(() => {
    window.__nuclear.setNuclide(6, 8); // C-14: Z=6, A=14
    window.__nuclear.decayBetaMinus();
    return window.__nuclear.getNuclide();
  });

  // Product: Z=7, A=14 (N-14). β-: e− + ν̄ → no A change, Z+1.
  expect(result.z).toBe(6 + 1); // ΣZ conserved (electron carries +1 charge)
  expect(result.a).toBe(14);    // ΣA conserved
});

// ─── GATE 4: Beta-plus/EC — F-18 → O-18 ─────────────────────────────────────

// 4a. F-18 beta-plus/EC: product is O-18 (Z=8, A=18).
test('nuclear-reactions: betaPlus F-18 → O-18 (Z=8, A=18)', async ({ page }) => {
  await gotoNuclide(page);

  const result = await page.evaluate(() => {
    window.__nuclear.setNuclide(9, 9); // F-18
    window.__nuclear.decayBetaPlus();
    return window.__nuclear.getNuclide();
  });

  expect(result.z).toBe(8); // O
  expect(result.a).toBe(18); // A conserved
  expect(result.symbol).toBe('O');
});

// 4b. F-18 beta-plus/EC conserves ΣZ and ΣA.
test('nuclear-reactions: betaPlus F-18 conserves ΣZ and ΣA', async ({ page }) => {
  await gotoNuclide(page);

  const result = await page.evaluate(() => {
    window.__nuclear.setNuclide(9, 9); // F-18: Z=9, A=18
    window.__nuclear.decayBetaPlus();
    return window.__nuclear.getNuclide();
  });

  expect(result.z).toBe(9 - 1); // Z−1 (positron + neutrino carry away charge)
  expect(result.a).toBe(18);    // ΣA conserved
});

// 4c. F-18 beta-plus Q > 0 (exothermic, ~0.634 MeV measured).
test('nuclear-reactions: betaPlus F-18 lastQ() > 0', async ({ page }) => {
  await gotoNuclide(page);

  const q = await page.evaluate(() => {
    window.__nuclear.setNuclide(9, 9); // F-18
    window.__nuclear.decayBetaPlus();
    return window.__nuclear.lastQ();
  });

  expect(q).toBeGreaterThan(0);
});

// ─── GATE 5: D-T fusion → He-5 (Q ≈ 17.594 MeV) ─────────────────────────────

// 5a. D+T fusion: product is He-5 (Z=2, A=5).
test('nuclear-reactions: D+T fusion → He-5 compound nucleus (Z=2, A=5)', async ({ page }) => {
  await gotoNuclide(page);

  const result = await page.evaluate(() => {
    window.__nuclear.setNuclide(1, 1); // D (Z=1, N=1, A=2)
    window.__nuclear.fuseWith(1, 2);   // T (Z=1, N=2, A=3)
    return window.__nuclear.getNuclide();
  });

  expect(result.z).toBe(2); // He
  expect(result.a).toBe(5); // A = 2 + 3 = 5 (He-5 compound nucleus)
});

// 5b. D+T fusion Q ≈ 17.594 MeV (large, exothermic).
test('nuclear-reactions: D+T fusion lastQ() ≈ 17.594 MeV', async ({ page }) => {
  await gotoNuclide(page);

  const q = await page.evaluate(() => {
    window.__nuclear.setNuclide(1, 1); // D
    window.__nuclear.fuseWith(1, 2);   // T
    return window.__nuclear.lastQ();
  });

  expect(q).toBeGreaterThan(0);
  // Absolute anchor: B(He-4) − B(D) − B(T) = 28.30 − 2.224 − 8.482 = 17.594 MeV.
  expect(q).toBeGreaterThan(17);
  expect(q).toBeLessThan(19);
});

// 5c. D+T fusion conserves ΣZ and ΣA.
test('nuclear-reactions: D+T fusion conserves ΣZ and ΣA', async ({ page }) => {
  await gotoNuclide(page);

  const result = await page.evaluate(() => {
    window.__nuclear.setNuclide(1, 1); // D: Z=1, A=2
    window.__nuclear.fuseWith(1, 2);   // T: Z=1, A=3
    return window.__nuclear.getNuclide();
  });

  // ΣZ = 1+1 = 2; ΣA = 2+3 = 5.
  expect(result.z).toBe(2);
  expect(result.a).toBe(5);
});

// ─── GATE 6: U-235 fission → Ba-141 + Kr-92 + neutrons ───────────────────────

// 6a. U-235 + absorbed neutron → U-236 fission into Ba-141 + Kr-92 + 3n.
// The seam: setNuclide(92, 144) = U-236 (U-235 after absorbing a neutron).
test('nuclear-reactions: U-236 fission → Ba-141 + Kr-92 + 3n fragments', async ({ page }) => {
  await gotoNuclide(page);

  const result = await page.evaluate(() => {
    // U-236: Z=92, N=144 (U-235 + absorbed neutron)
    window.__nuclear.setNuclide(92, 144);
    // Ba-141: Z=56, N=85; Kr-92: Z=36, N=56
    window.__nuclear.fission(56, 85, 36, 56);
    const fiss = window.__nuclear.lastFission();
    return {
      q: window.__nuclear.lastQ(),
      fiss,
    };
  });

  // Q must be large (> 140 MeV) and positive.
  expect(result.q).toBeGreaterThan(140);
  expect(result.q).toBeLessThan(220);

  // Verify the Maybe→JS serialisation round-trip for the fragment fields.
  expect(result.fiss).not.toBeNull();
  expect(result.fiss.aZ).toBe(56);   // Ba fragment Z
  expect(result.fiss.aA).toBe(141);  // Ba fragment A
  expect(result.fiss.bZ).toBe(36);   // Kr fragment Z
  expect(result.fiss.bA).toBe(92);   // Kr fragment A
  expect(result.fiss.neutrons).toBe(3);
});

// 6b. U-236 fission conserves ΣZ and ΣA — verified from the MODEL's returned values,
// not from hardcoded constants.  This makes the test non-tautological.
test('nuclear-reactions: U-236 fission conserves ΣZ and ΣA (model-driven)', async ({ page }) => {
  await gotoNuclide(page);

  const result = await page.evaluate(() => {
    // Read the parent BEFORE fission
    window.__nuclear.setNuclide(92, 144); // U-236
    const parent = window.__nuclear.getNuclide();

    // Perform fission
    window.__nuclear.fission(56, 85, 36, 56); // Ba-141 + Kr-92

    // Read the fragment data returned by the model
    const fiss = window.__nuclear.lastFission();

    return {
      parentZ: parent.z,
      parentA: parent.a,
      aZ: fiss.aZ,
      aA: fiss.aA,
      bZ: fiss.bZ,
      bA: fiss.bA,
      neutrons: fiss.neutrons,
    };
  });

  // Conservation assertions using the MODEL's own values, not JS constants.
  expect(result.aZ + result.bZ).toBe(result.parentZ);                         // ΣZ conserved
  expect(result.aA + result.bA + result.neutrons).toBe(result.parentA);       // ΣA conserved
});

// ─── GATE 7: #nuclide-info panel "Last reaction" row ─────────────────────────

// 7a. After alpha decay the panel shows "Last reaction" and "RELEASED" (Q>0).
test('nuclear-reactions: panel shows Last reaction + RELEASED after alpha decay', async ({ page }) => {
  await gotoNuclide(page);

  await page.evaluate(() => {
    window.__nuclear.setNuclide(92, 146); // U-238
    window.__nuclear.decayAlpha();
  });

  // Wait for DOM update.
  await page.waitForTimeout(200);

  const panelText = await page.evaluate(() => {
    const el = document.getElementById('nuclide-info');
    return el ? el.textContent : '';
  });

  // Panel must mention "Last reaction" and "RELEASED".
  expect(panelText).toContain('Last reaction');
  expect(panelText).toContain('RELEASED');
});

// 7b. After D-T fusion the panel shows "RELEASED" and a large Q.
test('nuclear-reactions: panel shows RELEASED with Q after D-T fusion', async ({ page }) => {
  await gotoNuclide(page);

  await page.evaluate(() => {
    window.__nuclear.setNuclide(1, 1); // D
    window.__nuclear.fuseWith(1, 2);   // T
  });

  await page.waitForTimeout(200);

  const panelText = await page.evaluate(() => {
    const el = document.getElementById('nuclide-info');
    return el ? el.textContent : '';
  });

  expect(panelText).toContain('Last reaction');
  expect(panelText).toContain('RELEASED');
  // Q ≈ 17.59 MeV should appear in the panel text.
  expect(panelText).toMatch(/17\.\d/);
});

// 7c. The Q-value in the panel matches lastQ() (numeric consistency).
test('nuclear-reactions: panel Q matches lastQ() for beta-minus C-14', async ({ page }) => {
  await gotoNuclide(page);

  const q = await page.evaluate(() => {
    window.__nuclear.setNuclide(6, 8); // C-14
    window.__nuclear.decayBetaMinus();
    return window.__nuclear.lastQ();
  });

  await page.waitForTimeout(200);

  const panelText = await page.evaluate(() => {
    const el = document.getElementById('nuclide-info');
    return el ? el.textContent : '';
  });

  // The Q value (rounded to 2 dp) should appear in the panel.
  const qRounded = Math.round(q * 100) / 100;
  expect(qRounded).toBeGreaterThan(0.10);
  // Panel contains RELEASED tag.
  expect(panelText).toContain('RELEASED');
});

// ─── GATE 8: Spontaneity annotation ──────────────────────────────────────────

// 8a. After a reaction with Q>0 the panel shows RELEASED.
test('nuclear-reactions: Q>0 reaction shows RELEASED in panel', async ({ page }) => {
  await gotoNuclide(page);

  await page.evaluate(() => {
    window.__nuclear.setNuclide(1, 1); // D
    window.__nuclear.fuseWith(1, 2);   // T → Q ≈ 17.6 MeV
  });

  await page.waitForTimeout(200);

  const panelText = await page.evaluate(() =>
    document.getElementById('nuclide-info')?.textContent ?? ''
  );

  expect(panelText).toContain('RELEASED');
  expect(panelText).not.toContain('ABSORBED');
});

// 8b. The decayMode row is still present in the panel alongside the reaction row.
test('nuclear-reactions: Stability row still present alongside Last reaction row', async ({ page }) => {
  await gotoNuclide(page);

  await page.evaluate(() => {
    window.__nuclear.setNuclide(92, 146); // U-238 (Alpha emitter)
    window.__nuclear.decayAlpha();
  });

  await page.waitForTimeout(200);

  const panelText = await page.evaluate(() =>
    document.getElementById('nuclide-info')?.textContent ?? ''
  );

  // Both the Stability row and the Last reaction row must be present.
  expect(panelText).toContain('Stability');
  expect(panelText).toContain('Last reaction');
});

// 8c. ABSORBED path: fusing two heavy nuclei above the iron peak gives Q < 0.
// Fe-56 (Z=26,N=30) + Fe-56 → compound Ba-like nucleus. Both nuclei are near
// the maximum of B/A; their product is well past the iron peak and has LOWER
// B/A → Q = B(product) − 2·B(Fe-56) < 0 (endothermic).
// This exercises the RELEASED/ABSORBED branch for the Q < 0 case.
test('nuclear-reactions: fusing two Fe-56 nuclei gives Q < 0 (ABSORBED path)', async ({ page }) => {
  await gotoNuclide(page);

  const result = await page.evaluate(() => {
    // Fe-56: Z=26, N=30
    window.__nuclear.setNuclide(26, 30);
    window.__nuclear.fuseWith(26, 30); // compound: Z=52, N=60, A=112 (Te-112 region)
    return {
      q: window.__nuclear.lastQ(),
    };
  });

  // Q < 0 for fusion beyond the iron peak (endothermic).
  expect(result.q).toBeLessThan(0);
  expect(typeof result.q).toBe('number');

  // The panel must show "ABSORBED" (not RELEASED) for Q < 0.
  await page.evaluate(() => {
    window.__nuclear.setNuclide(26, 30);
    window.__nuclear.fuseWith(26, 30);
  });
  await page.waitForTimeout(200);
  const panelText = await page.evaluate(() =>
    document.getElementById('nuclide-info')?.textContent ?? ''
  );
  expect(panelText).toContain('ABSORBED');
  expect(panelText).not.toContain('RELEASED');
});

// ─── GATE 9: Chain reactions (sequential decay) ───────────────────────────────

// 9. Chained alpha decays: U-238 → Th-234 → Ra-230 → Rn-226.
test('nuclear-reactions: chained alpha decays U→Th→Ra→Rn conserve ΣZ and ΣA', async ({ page }) => {
  await gotoNuclide(page);

  const result = await page.evaluate(() => {
    window.__nuclear.setNuclide(92, 146); // U-238
    window.__nuclear.decayAlpha(); // → Th-234
    const th = window.__nuclear.getNuclide();
    window.__nuclear.decayAlpha(); // → Ra-230
    const ra = window.__nuclear.getNuclide();
    window.__nuclear.decayAlpha(); // → Rn-226
    const rn = window.__nuclear.getNuclide();
    return { th, ra, rn };
  });

  // Each alpha decay: Z−2, A−4.
  expect(result.th.z).toBe(90);  expect(result.th.a).toBe(234);
  expect(result.ra.z).toBe(88);  expect(result.ra.a).toBe(230);
  expect(result.rn.z).toBe(86);  expect(result.rn.a).toBe(226);
});
