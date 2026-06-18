// DOM-only FFI that extends the existing `window.__builder` automation API with
// the Builder camera-orbit seam (setOrbit / getOrbit). Camera orbit is NOT part
// of the pure Builder model, so it lives in Main's own Ref rather than in
// BuilderState; this FFI just stashes Main's effectful get/set closures onto the
// already-installed `window.__builder` object. NEVER touches WebGL.
//
// `setOrbit` arrives curried (yaw => pitch => () => unit); `getOrbit` is a thunk
// (() => { yaw, pitch }). We unwrap them for an ergonomic JS signature.

export const installWindowOrbit = (bridge) => () => {
  if (!window.__builder) window.__builder = {};
  window.__builder.setOrbit = (yaw, pitch) => bridge.setOrbit(yaw)(pitch)();
  window.__builder.getOrbit = () => bridge.getOrbit();
};
