// Builder-scene potential-energy (Morse) curve overlay FFI. DOM only — this
// module must never import or touch WebGL/GL. It draws an inline SVG into the
// `#pe-overlay` container: a polyline of the sampled curve E(r), light axis
// hints, and a small dot per live bond at its current (r, e). Each export
// returns a thunk to match `Effect`. No-ops safely if `#pe-overlay` is absent.

const SVG_NS = "http://www.w3.org/2000/svg";

// Inner drawing box inside the 220x140 panel, leaving a little padding for the
// (pointer-events:none) glassy frame. viewBox coordinates, not CSS pixels.
const W = 220;
const H = 140;
const PAD_L = 14;
const PAD_R = 10;
const PAD_T = 12;
const PAD_B = 16;

const PLOT_X0 = PAD_L;
const PLOT_X1 = W - PAD_R;
const PLOT_Y0 = PAD_T;
const PLOT_Y1 = H - PAD_B;

// Build a child SVG element with the given attributes.
function el(name, attrs) {
  const node = document.createElementNS(SVG_NS, name);
  for (const k in attrs) node.setAttribute(k, String(attrs[k]));
  return node;
}

// Compute [min,max] of a field over an array; returns null on empty.
function extent(arr, key) {
  if (!arr.length) return null;
  let lo = arr[0][key];
  let hi = arr[0][key];
  for (const p of arr) {
    if (p[key] < lo) lo = p[key];
    if (p[key] > hi) hi = p[key];
  }
  return [lo, hi];
}

// Draw (replace) the SVG inside #pe-overlay from {samples, markers}. The energy
// axis is INVERTED (screen y grows down), so the well minimum dips toward the
// bottom of the panel — the visible "well". Auto-scales to the data extents.
export const renderPeCurve = (curve) => () => {
  const container = document.getElementById("pe-overlay");
  if (!container) return;

  const samples = curve.samples || [];
  // Clear previous render.
  while (container.firstChild) container.removeChild(container.firstChild);
  if (!samples.length) return;

  const rExt = extent(samples, "r");
  const eExt = extent(samples, "e");
  if (!rExt || !eExt) return;

  const [rLo, rHi] = rExt;
  const [eLo, eHi] = eExt;
  const rSpan = rHi - rLo || 1;
  const eSpan = eHi - eLo || 1;

  // r → x (left→right), e → y (INVERTED: high energy near top, low/well near
  // bottom). plotY maps energy linearly into [PLOT_Y0, PLOT_Y1] inverted.
  const plotX = (r) => PLOT_X0 + ((r - rLo) / rSpan) * (PLOT_X1 - PLOT_X0);
  const plotY = (e) => PLOT_Y1 - ((e - eLo) / eSpan) * (PLOT_Y1 - PLOT_Y0);

  const svg = el("svg", { viewBox: "0 0 " + W + " " + H });

  // Axis hints: a faint baseline (bottom) and left axis line.
  svg.appendChild(
    el("line", {
      x1: PLOT_X0,
      y1: PLOT_Y1,
      x2: PLOT_X1,
      y2: PLOT_Y1,
      stroke: "rgba(120,160,230,0.35)",
      "stroke-width": 1,
    }),
  );
  svg.appendChild(
    el("line", {
      x1: PLOT_X0,
      y1: PLOT_Y0,
      x2: PLOT_X0,
      y2: PLOT_Y1,
      stroke: "rgba(120,160,230,0.35)",
      "stroke-width": 1,
    }),
  );
  // Axis labels (tiny): "E" on the energy axis, "r" on the distance axis.
  svg.appendChild(
    el2("text", { x: PLOT_X0 - 9, y: PLOT_Y0 + 6, fill: "#7fb0e6", "font-size": 8 }, "E"),
  );
  svg.appendChild(
    el2("text", { x: PLOT_X1 - 4, y: PLOT_Y1 + 12, fill: "#7fb0e6", "font-size": 8 }, "r"),
  );

  // The curve as a polyline (x=r, y=e inverted so the well dips down).
  let pts = "";
  for (const p of samples) {
    pts += plotX(p.r).toFixed(2) + "," + plotY(p.e).toFixed(2) + " ";
  }
  svg.appendChild(
    el("polyline", {
      points: pts.trim(),
      fill: "none",
      stroke: "#8fd0ff",
      "stroke-width": 1.6,
    }),
  );

  // A live dot per bond marker at its current (r, e). Clamp to the plot box so a
  // marker slightly outside the sampled range still shows at the edge.
  const markers = curve.markers || [];
  for (const m of markers) {
    let cx = plotX(m.r);
    let cy = plotY(m.e);
    cx = Math.max(PLOT_X0, Math.min(PLOT_X1, cx));
    cy = Math.max(PLOT_Y0, Math.min(PLOT_Y1, cy));
    svg.appendChild(
      el("circle", {
        cx: cx.toFixed(2),
        cy: cy.toFixed(2),
        r: 3,
        fill: "#ffcf5a",
        stroke: "rgba(0,0,0,0.6)",
        "stroke-width": 0.75,
      }),
    );
  }

  container.appendChild(svg);
};

// Build an SVG text element with a text child (kept separate so attr-only `el`
// stays simple).
function el2(name, attrs, text) {
  const node = el(name, attrs);
  node.textContent = text;
  return node;
}

// Toggle the #pe-overlay panel display. No-op if the container is absent.
export const setPeOverlayVisible = (visible) => () => {
  const container = document.getElementById("pe-overlay");
  if (!container) return;
  container.style.display = visible ? "block" : "none";
};
