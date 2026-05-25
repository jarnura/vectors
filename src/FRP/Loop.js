// PureScript Effect-typed FFI: each export returns a thunk (`() => ...`)
// so the compiler can see the side effects, matching `Effect Unit`.

export const installKeyUpListener = (cb) => () => {
  window.addEventListener("keyup", (e) => cb(e.key)());
};

export const installMouseMoveListener = (cb) => () => {
  window.addEventListener("mousemove", (e) => cb(e.clientX)(e.clientY)());
};

export const requestAnimationFrame = (effect) => () => {
  window.requestAnimationFrame(() => effect());
};
