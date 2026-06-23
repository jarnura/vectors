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

// Entrance animation for the control bar (by id): the panel fades + slides up
// (opacity 0→1, translateY 12→0), then its child buttons stagger in. The panel's
// CSS sets opacity:0 initially, so the very first sampled frame reads < 1
// mid-flight and the animation settles it to 1. A deliberately long duration
// keeps the mid-animation opacity sample robustly below 1 under SwiftShader.
// No-op when the element is absent. DOM only — never WebGL.
export const animateControlBarIn = (id) => () => {
  const el = document.getElementById(id);
  if (!el) return;
  // Animate the panel container in (opacity + slide).
  animate(el, {
    opacity: [0, 1],
    translateY: [12, 0],
    duration: 650,
    ease: "outCubic",
  });
  // Stagger the interactive children in for a polished, intentional reveal.
  const children = el.querySelectorAll("button, label, input, .controls-sep");
  if (children.length > 0) {
    animate(children, {
      opacity: [0, 1],
      translateY: [6, 0],
      duration: 450,
      delay: stagger(35, { start: 120 }),
      ease: "outQuad",
    });
  }
};

// Wire the left-drawer toggle: clicking the icon (by id) slides the panel (by id)
// IN from the left (translateX -120% → 0%, opacity 0 → 1) and OUT again, via
// anime.js. pointer-events is set SYNCHRONOUSLY on every click from the new open
// state (auto when opening, none when closing) — never deferred to onComplete —
// so a closing/closed drawer can NEVER trap canvas events (Builder drag / wheel
// zoom), even under rapid double-clicks that would otherwise desync an
// animation-completion guard. DOM only — never WebGL.
export const installPanelToggle = (iconId) => (panelId) => () => {
  const icon = document.getElementById(iconId);
  const panel = document.getElementById(panelId);
  if (!icon || !panel) return;
  let open = false;
  panel.style.pointerEvents = "none";
  icon.addEventListener("click", () => {
    open = !open;
    // Synchronous + idempotent: the closed/closing drawer is immediately
    // non-interactive (it is sliding away off-screen, so it should not capture
    // clicks/wheel anyway), the open drawer is immediately interactive.
    panel.style.pointerEvents = open ? "auto" : "none";
    animate(panel, {
      translateX: open ? ["-120%", "0%"] : ["0%", "-120%"],
      opacity: open ? [0, 1] : [1, 0],
      duration: 420,
      ease: "outCubic",
    });
  });
};

// Wire a quick "pulse" (scale bounce) onto a button by id, as click feedback for
// the Add/Clear actions. No-op when absent. DOM only — animates the DOM node.
export const installButtonPulse = (id) => () => {
  const button = document.getElementById(id);
  if (!button) return;
  button.addEventListener("click", () => {
    animate(button, {
      scale: [1, 1.14, 1],
      duration: 320,
      ease: "outBack",
    });
  });
};

// Show or hide `#nuclide-info`. Pass `true` in the Nuclide scene, `false`
// elsewhere. Mirrors showMaterialsPanel. No-op if the element is absent.
export const showNuclidePanel = (visible) => () => {
  const el = document.getElementById("nuclide-info");
  if (!el) return;
  el.style.display = visible ? "block" : "none";
};

// Show or hide `#nuclide-controls` (the Nuclide-specific section inside the
// left drawer). Pass `true` in the Nuclide scene, `false` elsewhere.
export const showNuclideSectionInDrawer = (visible) => () => {
  const el = document.getElementById("nuclide-controls");
  if (!el) return;
  el.style.display = visible ? "flex" : "none";
};
