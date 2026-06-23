// DOM-only FFI for the Nuclide scene's `window.__nuclear` automation API.
// NEVER touches WebGL: it only stashes the PureScript bridge's effectful
// closures onto the window global as plain JS functions. Each PureScript
// `Effect a` arrives as a thunk `() => a`; effectful closures taking arguments
// are curried (`a => b => () => r`), so we unwrap them here for an ergonomic JS
// signature like `setNuclide(z, n)`.

export const installWindowNuclear = (bridge) => () => {
  window.__nuclear = {
    // ── M2 sandbox ops ───────────────────────────────────────────────────────
    setNuclide: (z, n) => bridge.setNuclide(z)(n)(),
    addProton: () => bridge.addProton(),
    removeProton: () => bridge.removeProton(),
    addNeutron: () => bridge.addNeutron(),
    removeNeutron: () => bridge.removeNeutron(),
    getNuclide: () => bridge.getNuclide(),
    getBinding: () => bridge.getBinding(),
    getStability: () => bridge.getStability(),
    // ── M3 named-reaction ops ─────────────────────────────────────────────────
    decayAlpha: () => bridge.decayAlpha(),
    decayBetaMinus: () => bridge.decayBetaMinus(),
    decayBetaPlus: () => bridge.decayBetaPlus(),
    fuseWith: (z2, n2) => bridge.fuseWith(z2)(n2)(),
    fission: (zA, nA, zB, nB) => bridge.fissionReact(zA)(nA)(zB)(nB)(),
    lastQ: () => bridge.lastQ(),
    lastFission: () => {
      const m = bridge.lastFission();
      // Unwrap PureScript Maybe: Nothing → null, Just r → r.value0 (the JsFissionResult record).
      if (!m || m.tag === 'Nothing') return null;
      return m.value0 !== undefined ? m.value0 : m;
    },
  };
};
