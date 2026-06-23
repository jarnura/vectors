module Test.SceneStarfieldSpec where

import Prelude

import Data.Array (all, length)
import Data.Number (sqrt)
import Effect (Effect)
import Effect.Console (log)
import Palette (shellColor, subshellColor)
import Scene (Scene(..), nextScene, sceneTitle)
import Starfield (starPositions)
import Test.Util (check)

sceneStarfieldSpec :: Effect Unit
sceneStarfieldSpec = do
  -- ───── Scene switch + starfield (atomos M2) ─────────────────────────
  log "scene + starfield properties:"

  -- The on-screen switch cycles CubePoc → Atomos → Molecule → Builder → Materials → Nuclide → CubePoc.
  check "nextScene cycles 6-scene CubePoc -> Atomos -> Molecule -> Builder -> Materials -> Nuclide -> CubePoc" $
    nextScene CubePoc == Atomos
      && nextScene Atomos == Molecule
      && nextScene Molecule == Builder
      && nextScene Builder == Materials
      && nextScene Materials == Nuclide
      && nextScene Nuclide == CubePoc

  -- The starfield is a non-empty, deterministic set of points.
  check "starfield has stars" $ length starPositions > 0

  -- Stars sit far out (a backdrop), beyond the atom's region.
  check "stars are far out (>800)" $
    all (\p -> sqrt (p.x * p.x + p.y * p.y + p.z * p.z) > 800.0) starPositions

  log "all scene + starfield properties hold."

  -- ───── Shell / sub-shell colour palette (colours M1) ────────────────
  log "shell/sub-shell palette properties:"

  let
    csum c = c.r + c.g + c.b
    -- Which RGB channel dominates (0=r, 1=g, 2=b) — the colour's "hue family".
    dominant c
      | c.r >= c.g && c.r >= c.b = 0
      | c.g >= c.b = 1
      | otherwise = 2

  -- Each principal shell gets a distinct colour.
  check "shell colours are distinct (1≠2≠3≠4)" $
    shellColor 1 /= shellColor 2
      && shellColor 2 /= shellColor 3
      && shellColor 3 /= shellColor 4
      && shellColor 1 /= shellColor 3

  -- A sub-shell's ℓ=0 is exactly the shell base colour.
  check "subshellColor n 0 == shellColor n (base)" $
    subshellColor 2 0 == shellColor 2 && subshellColor 4 0 == shellColor 4

  -- Higher ℓ is lighter (blended toward white → larger channel sum).
  check "sub-shells get lighter with ℓ" $
    csum (subshellColor 3 0) < csum (subshellColor 3 1)
      && csum (subshellColor 3 1) < csum (subshellColor 3 2)

  -- Same shell colour, just lighter: the dominant channel (hue) is preserved.
  check "sub-shell keeps the shell hue" $
    dominant (subshellColor 1 1) == dominant (shellColor 1)
      && dominant (subshellColor 4 2) == dominant (shellColor 4)

  -- Channels stay within [0,1] even at high ℓ.
  check "sub-shell colour channels clamp to [0,1]" $
    let c = subshellColor 1 3 in c.r <= 1.0 && c.g <= 1.0 && c.b <= 1.0 && c.r >= 0.0

  log "all shell/sub-shell palette properties hold."

  -- ───── Scene titles for the banner (overlay-text M2) ────────────────
  log "scene title properties:"

  check "sceneTitle CubePoc = Cube POC" $ sceneTitle CubePoc == "Cube POC"
  check "sceneTitle Atomos = atomos" $ sceneTitle Atomos == "atomos"

  log "all scene title properties hold."

  -- ───── Molecule scene wiring (molecule-platform M2) ─────────────────
  -- The molecule scene joins the on-screen switch, so the toggle cycles
  -- CubePoc → Atomos → Molecule → Builder → CubePoc.
  log "molecule scene wiring properties:"

  -- The switch passes through every scene.
  check "nextScene CubePoc = Atomos" $ nextScene CubePoc == Atomos
  check "nextScene Atomos = Molecule" $ nextScene Atomos == Molecule

  -- The molecule scene has a non-empty banner title (the implementer matches
  -- this exact string in Scene.sceneTitle).
  check "sceneTitle Molecule = molecule" $ sceneTitle Molecule == "molecule"

  log "all molecule scene wiring properties hold."

  -- ───── Builder scene wiring (molecule-builder M2) ───────────────────
  -- The builder scene joins the on-screen switch as a fourth scene, so the
  -- toggle now cycles CubePoc → Atomos → Molecule → Builder → CubePoc.
  log "builder scene wiring properties:"

  -- The switch passes Molecule → Builder, Builder → Materials, Materials → Nuclide,
  -- and Nuclide closes the 6-cycle back to CubePoc.
  check "nextScene Molecule = Builder" $ nextScene Molecule == Builder
  check "nextScene Builder = Materials" $ nextScene Builder == Materials
  check "nextScene Materials = Nuclide" $ nextScene Materials == Nuclide
  check "nextScene Nuclide = CubePoc (closes 6-cycle)" $ nextScene Nuclide == CubePoc

  -- The builder scene has a non-empty banner title (the implementer matches
  -- this exact string in Scene.sceneTitle); lowercase like 'atomos'/'molecule'.
  check "sceneTitle Builder is non-empty" $ sceneTitle Builder /= ""
  check "sceneTitle Builder = builder" $ sceneTitle Builder == "builder"

  -- The materials scene has a non-empty banner title (lowercase like sibling scenes).
  check "sceneTitle Materials is non-empty" $ sceneTitle Materials /= ""
  check "sceneTitle Materials = materials" $ sceneTitle Materials == "materials"

  -- The nuclide scene has the title "nuclide" (M2-nuclide).
  check "sceneTitle Nuclide is non-empty" $ sceneTitle Nuclide /= ""
  check "sceneTitle Nuclide = nuclide" $ sceneTitle Nuclide == "nuclide"

  log "all builder + materials + nuclide scene wiring properties hold."
