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

// Pointer (mouse) down/move/up over the canvas, reported in canvas-LOCAL
// backing-store pixels: subtract the canvas bounding rect (CSS px) and scale by
// the backing width/height vs the CSS size, so the coordinates match the canvas
// the renderer draws into regardless of DPR / CSS scaling. DOM-only: reads
// geometry, never touches WebGL.
export const installCanvasPointer = (down) => (move) => (up) => () => {
  const canvas = document.getElementById("canvas");
  if (!canvas) return;
  const toLocal = (e) => {
    const rect = canvas.getBoundingClientRect();
    const sx = rect.width > 0 ? canvas.width / rect.width : 1;
    const sy = rect.height > 0 ? canvas.height / rect.height : 1;
    return { x: (e.clientX - rect.left) * sx, y: (e.clientY - rect.top) * sy };
  };
  canvas.addEventListener("mousedown", (e) => {
    const p = toLocal(e);
    down(p.x)(p.y)();
  });
  // Listen on window for move/up so a drag that strays off the canvas still
  // tracks (standard drag behaviour).
  window.addEventListener("mousemove", (e) => {
    const p = toLocal(e);
    move(p.x)(p.y)();
  });
  window.addEventListener("mouseup", () => up());
};

// Mouse wheel over the canvas: attach a non-passive 'wheel' listener to
// #canvas, preventDefault so the PAGE never scrolls, then hand the raw deltaY to
// the callback. Canvas-scoped (not window). DOM-only input plumbing.
export const installWheelListener = (cb) => () => {
  const canvas = document.getElementById("canvas");
  if (!canvas) return;
  canvas.addEventListener(
    "wheel",
    (e) => {
      e.preventDefault();
      cb(e.deltaY)();
    },
    { passive: false },
  );
};

export const requestAnimationFrame = (effect) => () => {
  window.requestAnimationFrame(() => effect());
};
