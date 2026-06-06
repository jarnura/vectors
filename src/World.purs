-- Static world-backdrop placement constants and transforms. Pure and
-- side-effect free so both Main (rendering) and the test suite can share them.
module World
  ( groundY
  , groundExtent
  , gridDivisions
  , groundTransform
  , gridTransform
  ) where

import Prelude

import Math.Matrix (Matrix)
import Math.Matrix as M

-- The ground sits at the main cube's base. The main cube has half-extent 100,
-- so its bottom face is at Y = -100; placing the plane there grounds the cube.
groundY :: Number
groundY = -100.0

-- Half-width of the ground/grid in X and Z. Kept ≤ ~900 so the plane stays
-- inside the perspective far plane (camera 1000 back, far 2000).
groundExtent :: Number
groundExtent = 800.0

-- Number of grid divisions per axis.
gridDivisions :: Int
gridDivisions = 16

-- The ground's model matrix: a constant translation to groundY. It takes no
-- State, so the ground never moves when the cube rotates.
groundTransform :: Matrix Number
groundTransform = M.translate 0.0 groundY 0.0

-- The grid sits a hair above the ground plane so the coplanar lines don't
-- z-fight with the solid quad.
gridTransform :: Matrix Number
gridTransform = M.translate 0.0 (groundY + 0.5) 0.0
