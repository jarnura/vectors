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

export const installValenceOnlyToggle = (effect) => () => {
  const el = document.getElementById("valence-only");
  if (!el) return;
  el.addEventListener("change", () => effect());
};

export const installAntibondingToggle = (effect) => () => {
  const el = document.getElementById("antibonding");
  if (!el) return;
  el.addEventListener("change", () => effect());
};

export const installSubshellViewToggle = (effect) => () => {
  const el = document.getElementById("subshell-view");
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

// Drag-strength slider (#drag-strength): on input/change, parse the slider
// value and hand the live drag strength to the callback. Null-guarded and
// NaN-guarded like the other control wirings.
export const installDragStrengthSlider = (cb) => () => {
  const el = document.getElementById("drag-strength");
  if (!el) return;
  const fire = () => {
    const v = parseFloat(el.value);
    if (!Number.isNaN(v)) cb(v)();
  };
  el.addEventListener("input", fire);
  el.addEventListener("change", fire);
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
    down(p.x)(p.y)(e.detail)();
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

// On-screen zoom buttons: #zoom-in fires cb(-delta) (zoom IN) and #zoom-out
// fires cb(+delta) (zoom OUT), reusing the same zoom channel as the wheel. The
// magnitude is passed from PureScript (Camera.buttonZoomDelta) — no literal
// here. Null-guarded like the other control wirings.
export const installZoomButtons = (delta) => (cb) => () => {
  const zin = document.getElementById("zoom-in");
  const zout = document.getElementById("zoom-out");
  if (zin) zin.addEventListener("click", () => cb(-delta)());
  if (zout) zout.addEventListener("click", () => cb(delta)());
};

// On-screen orbit buttons: #orbit-left / #orbit-right step yaw, #orbit-up /
// #orbit-down step pitch, #orbit-reset returns to {yaw:0,pitch:0}. Each is a
// plain Effect Unit thunk — the PureScript side owns the per-direction {dx,dy}
// decision and the Camera.buttonOrbitDelta magnitude, mirroring how the
// empty-space drag folds a cursor delta. Null-guarded like the other wirings.
export const installOrbitButtons =
  (onLeft) => (onRight) => (onUp) => (onDown) => (onReset) => () => {
    const wire = (id, cb) => {
      const el = document.getElementById(id);
      if (el) el.addEventListener("click", () => cb());
    };
    wire("orbit-left", onLeft);
    wire("orbit-right", onRight);
    wire("orbit-up", onUp);
    wire("orbit-down", onDown);
    wire("orbit-reset", onReset);
  };

// Publish the live eased Builder detail level for deterministic E2E observation.
export const setBuilderDetail = (d) => () => {
  window.__builderDetail = d;
};

export const requestAnimationFrame = (effect) => () => {
  window.requestAnimationFrame(() => effect());
};
