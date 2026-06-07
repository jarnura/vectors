// anime.js HTML overlay-controls FFI. DOM only — this module must never import
// or touch WebGL/GL. It imports only from "animejs" and manipulates the DOM.
// Each export returns a thunk to match `Effect`.
import { animate, stagger } from "animejs";

// Render the data-driven properties panel: one `.mol-row` per property entry,
// then animate the rows in (fade + slide up, staggered). Builds the DOM from the
// passed array — never hardcodes molecule text. No-op when the element is absent.
export const renderInfoPanel = (id) => (rows) => () => {
  const el = document.getElementById(id);
  if (!el) return;
  // Build the rows from the data, escaping nothing into innerHTML directly:
  // construct nodes so label/value text is set via textContent (no injection).
  el.innerHTML = "";
  for (const row of rows) {
    const r = document.createElement("div");
    r.className = "mol-row";
    const label = document.createElement("span");
    label.className = "mol-label";
    label.textContent = row.label;
    const value = document.createElement("span");
    value.className = "mol-value";
    value.textContent = row.value;
    r.appendChild(label);
    r.appendChild(value);
    el.appendChild(r);
  }
  // Animate the freshly-built rows in via anime.js.
  const targets = el.querySelectorAll(".mol-row");
  if (targets.length > 0) {
    animate(targets, {
      opacity: [0, 1],
      translateY: [8, 0],
      duration: 500,
      delay: stagger(80),
      ease: "outQuad",
    });
  }
};

// Wire a click on `#bond-btn` to run the given effect. No-op if absent.
export const installBondButton = (effect) => () => {
  const button = document.getElementById("bond-btn");
  if (!button) return;
  button.addEventListener("click", () => effect());
};

// Animate a plain JS object's `p` from the separated resting state (1.0) down to
// a coalesced bonded state and settle there, calling the PureScript callback with
// the current progress each onUpdate. The animation overshoots toward full
// coalescence (0.0) then settles at a bonded resting separation (0.45), so the
// molecule visibly draws together and STAYS bonded (a lasting render change). DOM
// only: this animates a JS value, not WebGL — the callback pushes progress into
// the FRP input ref so the renderer interpolates the atoms together.
export const runBondAnimation = (cb) => () => {
  const obj = { p: 1.0 };
  animate(obj, {
    p: [1.0, 0.0, 0.4],
    duration: 1000,
    ease: "inOutQuad",
    onUpdate: () => cb(obj.p)(),
  });
};
