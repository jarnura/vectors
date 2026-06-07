-- The app has two scenes: the original cube POC and the atomos atom
-- visualizer. An on-screen switch toggles between them; each scene supplies
-- its own backdrop (clear color) and entities.
module Scene
  ( Scene(..)
  , nextScene
  , sceneTitle
  , spaceColor
  ) where

import Prelude

import Graphics.GL (Color)

data Scene
  = CubePoc
  | Atomos
  | Molecule

derive instance eqScene :: Eq Scene

-- Cycle to the next scene: CubePoc → Atomos → Molecule → CubePoc.
nextScene :: Scene -> Scene
nextScene CubePoc = Atomos
nextScene Atomos = Molecule
nextScene Molecule = CubePoc

-- Human-readable scene name, used by the overlay title banner.
sceneTitle :: Scene -> String
sceneTitle CubePoc = "Cube POC"
sceneTitle Atomos = "atomos"
sceneTitle Molecule = "molecule"

-- Near-black "deep space" backdrop for the atomos scene.
spaceColor :: Color
spaceColor = { r: 0.02, g: 0.02, b: 0.06, a: 1.0 }
