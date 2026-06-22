-- The app has five scenes: the original cube POC, the atomos atom visualizer,
-- the molecule scene, the builder sandbox, and the materials scene. An
-- on-screen switch cycles between them; each scene supplies its own backdrop
-- (clear color) and entities.
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
  | Builder
  | Materials

derive instance eqScene :: Eq Scene

-- Cycle to the next scene: CubePoc → Atomos → Molecule → Builder → Materials → CubePoc.
nextScene :: Scene -> Scene
nextScene CubePoc = Atomos
nextScene Atomos = Molecule
nextScene Molecule = Builder
nextScene Builder = Materials
nextScene Materials = CubePoc

-- Human-readable scene name, used by the overlay title banner.
sceneTitle :: Scene -> String
sceneTitle CubePoc = "Cube POC"
sceneTitle Atomos = "atomos"
sceneTitle Molecule = "molecule"
sceneTitle Builder = "builder"
sceneTitle Materials = "materials"

-- Near-black "deep space" backdrop for the atomos scene.
spaceColor :: Color
spaceColor = { r: 0.02, g: 0.02, b: 0.06, a: 1.0 }
