// DOM-only FFI for the Builder scene's `window.__builder` automation API.
// NEVER touches WebGL: it only stashes the PureScript bridge's effectful
// closures onto the window global as plain JS functions. Each PureScript
// `Effect a` arrives as a thunk `() => a`; effectful closures taking arguments
// are curried (`a => b => () => r`), so we unwrap them here for an ergonomic JS
// signature like `addAtom(z, x, y, z3)`.

export const installWindowBuilder = (bridge) => () => {
  window.__builder = {
    addAtom: (z, x, y, z3) => bridge.addAtom(z)(x)(y)(z3)(),
    moveAtom: (id, x, y, z3) => bridge.moveAtom(id)(x)(y)(z3)(),
    moveAtomWith: (strength, id, x, y, z3) =>
      bridge.moveAtomWith(strength)(id)(x)(y)(z3)(),
    moveMolecule: (id, x, y, z3) => bridge.moveMolecule(id)(x)(y)(z3)(),
    clear: () => bridge.clear(),
    getBonds: () => bridge.getBonds(),
    getMolecules: () => bridge.getMolecules(),
    getAtoms: () => bridge.getAtoms(),
  };
};
