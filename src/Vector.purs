module Vector
  ( projectOn2d
  , rotateX
  , rotateY
  , rotateZ
  ) where

import Prelude hiding (add)

import Cube.Types (Cube)
import Data.Maybe (fromMaybe)
import Data.Number (cos, pi, sin)
import Math.Matrix (Matrix, add, fromArray, multiply, zeros)

-- 3D → 2D projection: drop the z-axis by multiplying by a 2×4 matrix
-- that selects the x and y components.

projectOn2d :: Number -> Number -> Cube -> Cube
projectOn2d w h =
  map (multiply transform3dMatrixTo2dMatrix)
    >>> map (add (changeOrigin w h))

transform3dMatrixTo2dMatrix :: Matrix Number
transform3dMatrixTo2dMatrix = projection2x4 2.0

-- Origin shift: the 3D-plane origin (0,0,0) projects to (0,0) on the 2D plane.
-- Centering the cube on the canvas requires shifting by half its dimensions.

changeOrigin :: Number -> Number -> Matrix Number
changeOrigin w h =
  fromMaybe (zeros 2 1) $ fromArray 2 1 [ w / 2.0, h / 2.0 ]

projection2x4 :: Number -> Matrix Number
projection2x4 v =
  fromMaybe (zeros 2 4) $ fromArray 2 4
    [ v   , 0.0 , 0.0 , 0.0
    , 0.0 , v   , 0.0 , 0.0
    ]

rotateX :: Number -> Matrix Number
rotateX deg = fromMaybe (zeros 4 4) $ fromArray 4 4
  [ 1.0 , 0.0      , 0.0       , 0.0
  , 0.0 , costeta  , -sinteta  , 0.0
  , 0.0 , sinteta  , costeta   , 0.0
  , 0.0 , 0.0      , 0.0       , 1.0
  ]
  where
    costeta = cos (deg * pi / 180.0)
    sinteta = sin (deg * pi / 180.0)

rotateY :: Number -> Matrix Number
rotateY deg = fromMaybe (zeros 4 4) $ fromArray 4 4
  [ costeta  , 0.0 , sinteta , 0.0
  , 0.0      , 1.0 , 0.0     , 0.0
  , -sinteta , 0.0 , costeta , 0.0
  , 0.0      , 0.0 , 0.0     , 1.0
  ]
  where
    costeta = cos (deg * pi / 180.0)
    sinteta = sin (deg * pi / 180.0)

rotateZ :: Number -> Matrix Number
rotateZ deg = fromMaybe (zeros 4 4) $ fromArray 4 4
  [ costeta , -sinteta , 0.0 , 0.0
  , sinteta , costeta  , 0.0 , 0.0
  , 0.0     , 0.0      , 1.0 , 0.0
  , 0.0     , 0.0      , 0.0 , 1.0
  ]
  where
    costeta = cos (deg * pi / 180.0)
    sinteta = sin (deg * pi / 180.0)
