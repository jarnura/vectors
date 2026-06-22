// anime.js data-driven #materials-cards gallery FFI. DOM only — this module
// must never import or touch WebGL/GL. It imports only from "animejs" and
// manipulates the DOM. Each export returns a thunk to match `Effect`.
import { animate, stagger } from "animejs";

// Module-scoped reference to the rendered cards (for highlight/selection state).
// Each entry is { el: HTMLElement, index: number }.
let renderedCards = [];

// Build `#materials-cards` children data-driven from the PS array. One
// `.material-card` div per entry; each card shows name, formula, hybridization.
// Wires each card's click to the `onSelect i` PureScript callback. Animates
// all cards in with a stagger entrance (fade + slide up). No-op when the
// container is absent. DOM only — never WebGL.
export const renderMaterialsCards = (cards) => (onSelect) => () => {
  const container = document.getElementById("materials-cards");
  if (!container) return;

  // Clear any previously-rendered cards.
  container.innerHTML = "";
  renderedCards = [];

  for (let i = 0; i < cards.length; i++) {
    const card = cards[i];
    const div = document.createElement("div");
    div.className = "material-card";
    div.setAttribute("data-index", String(i));

    // Name heading
    const nameEl = document.createElement("div");
    nameEl.className = "material-card-name";
    nameEl.textContent = card.name;

    // Formula
    const formulaEl = document.createElement("div");
    formulaEl.className = "material-card-formula";
    formulaEl.textContent = card.formula;

    // Hybridization badge
    const hybridEl = document.createElement("div");
    hybridEl.className = "material-card-hybrid";
    hybridEl.textContent = card.hybridization;

    div.appendChild(nameEl);
    div.appendChild(formulaEl);
    div.appendChild(hybridEl);
    container.appendChild(div);
    renderedCards.push({ el: div, index: i });

    // Wire the click handler (capture index in closure).
    const capturedIndex = i;
    div.addEventListener("click", () => {
      // Highlight this card.
      highlightCardInternal(capturedIndex);
      // Fire the PureScript onSelect callback with this index.
      onSelect(capturedIndex)();
    });
  }

  // Animate all cards in with a stagger entrance (opacity + translateY).
  if (renderedCards.length > 0) {
    const els = renderedCards.map((c) => c.el);
    animate(els, {
      opacity: [0, 1],
      translateY: [16, 0],
      duration: 480,
      delay: stagger(90),
      ease: "outQuad",
    });
  }
};

// Internal: highlight a single card by index (removes old highlight first).
function highlightCardInternal(index) {
  for (const { el } of renderedCards) {
    el.classList.remove("material-card-selected");
  }
  const target = renderedCards.find((c) => c.index === index);
  if (!target) return;
  target.el.classList.add("material-card-selected");
  // Pulse animation on the selected card.
  animate(target.el, {
    scale: [1, 1.06, 1],
    duration: 340,
    ease: "outBack",
  });
}

// Exported PureScript-facing version of highlightCardInternal (thunk for Effect).
export const highlightCard = (index) => () => {
  highlightCardInternal(index);
};

// Show or hide `#materials-cards`. Pass `true` in the Materials scene, `false`
// elsewhere. Uses display/opacity so the hidden gallery never traps pointer
// events on the canvas. No-op if the container is absent. DOM only.
export const showMaterialsCards = (visible) => () => {
  const container = document.getElementById("materials-cards");
  if (!container) return;
  if (visible) {
    container.style.display = "flex";
    container.style.pointerEvents = "auto";
  } else {
    container.style.display = "none";
    container.style.pointerEvents = "none";
  }
};

// Show or hide `#materials-info`. Pass `true` in the Materials scene, `false`
// elsewhere. Mirrors showMaterialsCards for the sibling info panel. No-op if
// the container is absent. DOM only.
export const showMaterialsPanel = (visible) => () => {
  const el = document.getElementById("materials-info");
  if (!el) return;
  el.style.display = visible ? "block" : "none";
};
