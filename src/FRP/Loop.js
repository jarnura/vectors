// PureScript Effect-typed FFI: each export returns a thunk (`() => ...`)
// so the compiler can see the side effects, matching `Effect Unit`.

export const installKeyUpListener = (cb) => () => {
  window.addEventListener("keyup", (e) => cb(e.key)());
};

export const installMouseMoveListener = (cb) => () => {
  window.addEventListener("mousemove", (e) => cb(e.clientX)(e.clientY)());
};

export const installShearButton = (cb) => () => {
  const button = document.getElementById("shear-btn");
  const input = document.getElementById("shear-value");
  if (!button || !input) return;
  button.addEventListener("click", () => {
    const k = parseFloat(input.value);
    if (!Number.isNaN(k)) cb(k)();
  });
};

export const requestAnimationFrame = (effect) => () => {
  window.requestAnimationFrame(() => effect());
};
