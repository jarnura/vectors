-- The app has two scenes: the original cube POC and the atomos atom
-- visualizer. An on-screen switch toggles between them; each scene supplies
-- its own backdrop (clear color) and entities.
module Scene
  ( Scene(..)
  , nextScene
  , spaceColor
  ) where

import Prelude

import Graphics.GL (Color)

data Scene
  = CubePoc
  | Atomos

derive instance eqScene :: Eq Scene

-- Toggle to the other scene.
nextScene :: Scene -> Scene
nextScene CubePoc = Atomos
nextScene Atomos = CubePoc

-- Near-black "deep space" backdrop for the atomos scene.
spaceColor :: Color
spaceColor = { r: 0.02, g: 0.02, b: 0.06, a: 1.0 }
