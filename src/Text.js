// anime.js HTML overlay-text FFI. DOM only — this module must never import or
// touch WebGL/GL. Each export returns a thunk to match `Effect`.
import { animate, scrambleText } from "animejs";

export const scrambleInto = (id) => (text) => () => {
  const el = document.getElementById(id);
  if (!el) return;
  // anime.js v4 treats `textContent` as a direct element property (it exists on
  // the node), so the scrambled string is written to el.textContent each frame.
  animate(el, {
    textContent: scrambleText({ text }),
    duration: 700,
    ease: "outQuad",
  });
};

export const setVisible = (id) => (visible) => () => {
  const el = document.getElementById(id);
  if (!el) return;
  el.style.display = visible ? "block" : "none";
};
