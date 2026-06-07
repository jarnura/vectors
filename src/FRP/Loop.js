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

export const installSceneToggle = (effect) => () => {
  const button = document.getElementById("scene-toggle");
  if (!button) return;
  button.addEventListener("click", () => effect());
};

export const installView2DToggle = (effect) => () => {
  const el = document.getElementById("view-2d");
  if (!el) return;
  el.addEventListener("change", () => effect());
};

export const installElementInput = (cb) => () => {
  const input = document.getElementById("element-value");
  if (!input) return;
  const fire = () => {
    const z = parseInt(input.value, 10);
    if (!Number.isNaN(z)) cb(z)();
  };
  input.addEventListener("input", fire);
  input.addEventListener("change", fire);
};

export const installAddButton = (cb) => () => {
  const button = document.getElementById("add-btn");
  const input = document.getElementById("element-value");
  if (!button) return;
  button.addEventListener("click", () => {
    const z = input ? parseInt(input.value, 10) : NaN;
    cb(Number.isNaN(z) ? 6 : z)();
  });
};

export const installClearButton = (effect) => () => {
  const button = document.getElementById("clear-btn");
  if (!button) return;
  button.addEventListener("click", () => effect());
};

export const requestAnimationFrame = (effect) => () => {
  window.requestAnimationFrame(() => effect());
};
