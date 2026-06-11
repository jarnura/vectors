// Per-atom atomic-SYMBOL HTML overlay-label FFI. DOM only — this module must
// never import or touch WebGL/GL. It maintains a pool of `<div class="atom-label">`
// nodes inside `#atom-labels`, keyed by atom id, so labels reconcile cheaply each
// frame (no churn). Each export returns a thunk to match `Effect`.

// The live pool: atom id → its <div class="atom-label"> node. Module-scoped so it
// persists across frames; reconciled against the input on every sync.
const pool = new Map();

// Reconcile the label pool against the PS array of records. PureScript records
// compile to plain JS objects, so fields are read as item.id / item.x / item.y /
// item.text / item.opacity. No-op if the #atom-labels container is missing.
export const syncAtomLabels = (items) => () => {
  const container = document.getElementById("atom-labels");
  if (!container) return;

  const seen = new Set();
  for (const item of items) {
    seen.add(item.id);
    let div = pool.get(item.id);
    if (!div) {
      div = document.createElement("div");
      div.className = "atom-label";
      container.appendChild(div);
      pool.set(item.id, div);
    }
    // Set text only when it changes (avoids needless DOM writes / reflows).
    if (div.textContent !== item.text) div.textContent = item.text;
    // Centre the label on the atom: translate to (x,y) then back by half its own
    // size (the -50%,-50% baseline lives in the .atom-label CSS via an additional
    // transform is not available, so bake the centring offset into the translate).
    div.style.transform =
      "translate(" + item.x + "px, " + item.y + "px) translate(-50%, -50%)";
    div.style.opacity = String(item.opacity);
  }

  // Remove any pooled label whose id is no longer in the input (leak-free).
  for (const [id, div] of pool) {
    if (!seen.has(id)) {
      if (div.parentNode) div.parentNode.removeChild(div);
      pool.delete(id);
    }
  }
};

// The canvas CLIENT size (CSS px) — clientWidth/clientHeight — for mapping
// backing-store projection pixels to CSS pixels. Falls back to the backing-store
// size if client size reads as 0 (e.g. detached), so the map never divides by 0.
export const getCanvasClientSize = (canvas) => () => {
  const w = canvas.clientWidth || canvas.width;
  const h = canvas.clientHeight || canvas.height;
  return { w, h };
};

// Remove all `.atom-label` children from `#atom-labels` and empty the pool.
// No-op if the container is absent.
export const clearAtomLabels = () => {
  const container = document.getElementById("atom-labels");
  for (const [, div] of pool) {
    if (div.parentNode) div.parentNode.removeChild(div);
  }
  pool.clear();
  if (container) {
    // Defensive: drop any stray .atom-label not tracked in the pool.
    const strays = container.querySelectorAll(".atom-label");
    strays.forEach((el) => {
      if (el.parentNode) el.parentNode.removeChild(el);
    });
  }
};
