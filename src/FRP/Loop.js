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

export const installFreeElectronsOnlyToggle = (effect) => () => {
  const el = document.getElementById("free-electrons-only");
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

// Layer-space slider (#layer-space): on input/change, parse the slider value
// and hand the live layer-space multiplier to the callback. Mirrors
// installDragStrengthSlider exactly. Null-guarded and NaN-guarded.
export const installLayerSpaceSlider = (cb) => () => {
  const el = document.getElementById("layer-space");
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

// Zoom slider (#zoom-slider): on input/change, parse the slider value and hand
// the absolute zoom to the callback. Mirrors installDragStrengthSlider exactly.
// Null-guarded and NaN-guarded like the other control wirings.
export const installZoomSlider = (cb) => () => {
  const el = document.getElementById("zoom-slider");
  if (!el) return;
  const fire = () => {
    const v = parseFloat(el.value);
    if (!Number.isNaN(v)) cb(v)();
  };
  el.addEventListener("input", fire);
  el.addEventListener("change", fire);
};

// Write the live zoom value back to the #zoom-slider each frame so the thumb
// tracks programmatic zoom changes (wheel, Materials reframe). Setting .value
// programmatically does NOT fire the input/change listener — no feedback loop.
// Skip the write while the user is actively dragging the slider (it holds focus)
// so the per-frame write never fights the held thumb — the standard guard for a
// two-way range-input binding.
export const setZoomSlider = (v) => () => {
  const el = document.getElementById("zoom-slider");
  if (el && document.activeElement !== el) el.value = String(v);
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

// Nuclide scene controls: wire the left-drawer buttons and inputs for the
// transmutation sandbox. Each argument is a curried PureScript Effect Unit
// thunk (the PS caller owns the NuclearApi mutation). Null-guarded like the
// other control wirings.
export const installNuclearControls =
  (onAddProton) =>
  (onRemoveProton) =>
  (onAddNeutron) =>
  (onRemoveNeutron) =>
  (onReset) =>
  (onSetNuclide) =>
  () => {
    const wire = (id, cb) => {
      const el = document.getElementById(id);
      if (el) el.addEventListener("click", () => cb());
    };
    wire("nuc-add-proton", onAddProton);
    wire("nuc-remove-proton", onRemoveProton);
    wire("nuc-add-neutron", onAddNeutron);
    wire("nuc-remove-neutron", onRemoveNeutron);
    wire("nuc-reset", onReset);

    // Z + N number inputs + Set button.
    const setBtn = document.getElementById("nuc-set");
    if (setBtn) {
      setBtn.addEventListener("click", () => {
        const zEl = document.getElementById("nuclide-z");
        const nEl = document.getElementById("nuclide-n");
        const z = zEl ? parseInt(zEl.value, 10) : NaN;
        const n = nEl ? parseInt(nEl.value, 10) : NaN;
        if (!Number.isNaN(z) && !Number.isNaN(n)) onSetNuclide(z)(n)();
      });
    }
  };

// M3 named-reaction buttons: #react-alpha, #react-beta-minus, #react-beta-plus,
// #react-fuse (reads #fuse-z2 / #fuse-n2 inputs), #react-fission (reads
// #fiss-za / #fiss-na / #fiss-zb / #fiss-nb inputs).
// Each argument is a curried PureScript Effect thunk. Null-guarded like the
// other control wirings.
export const installNuclearReactionControls =
  (onAlpha) =>
  (onBetaMinus) =>
  (onBetaPlus) =>
  (onFuseWith) =>
  (onFission) =>
  () => {
    const wire = (id, cb) => {
      const el = document.getElementById(id);
      if (el) el.addEventListener("click", () => cb());
    };
    wire("react-alpha", onAlpha);
    wire("react-beta-minus", onBetaMinus);
    wire("react-beta-plus", onBetaPlus);

    // Fuse button: reads Z2 + N2 inputs.
    const fuseBtn = document.getElementById("react-fuse");
    if (fuseBtn) {
      fuseBtn.addEventListener("click", () => {
        const z2El = document.getElementById("fuse-z2");
        const n2El = document.getElementById("fuse-n2");
        const z2 = z2El ? parseInt(z2El.value, 10) : NaN;
        const n2 = n2El ? parseInt(n2El.value, 10) : NaN;
        if (!Number.isNaN(z2) && !Number.isNaN(n2)) onFuseWith(z2)(n2)();
      });
    }

    // Fission button: reads four fragment inputs.
    const fissBtn = document.getElementById("react-fission");
    if (fissBtn) {
      fissBtn.addEventListener("click", () => {
        const zA = parseInt((document.getElementById("fiss-za") || {}).value, 10);
        const nA = parseInt((document.getElementById("fiss-na") || {}).value, 10);
        const zB = parseInt((document.getElementById("fiss-zb") || {}).value, 10);
        const nB = parseInt((document.getElementById("fiss-nb") || {}).value, 10);
        if (!Number.isNaN(zA) && !Number.isNaN(nA) && !Number.isNaN(zB) && !Number.isNaN(nB)) {
          onFission(zA)(nA)(zB)(nB)();
        }
      });
    }
  };

export const requestAnimationFrame = (effect) => () => {
  window.requestAnimationFrame(() => effect());
};
