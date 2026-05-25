module Vector where

import VectorPrelude

import Cube.Types (Cube)

import LinearAlgebra.Matrix as M
import Utils                as Helper

-- TransForm 3d to 2d

-- 1. To Convert a 3d point [ x ]
--                          [ y ]
--                          [ z ]
-- 2. To a 2d point [ x ]
--                  [ y ]

-- 3. Then multiply the 3d point matrix with [ 1 0 0 ] gives [ x ]
--                                           [ 0 1 0 ]       [ y ]

transform3dMatrixTo2dMatrix :: Matrix Number
transform3dMatrixTo2dMatrix = identityMatrix_2_4 2.0

projectOn2d :: Number → Number → Cube → Cube
projectOn2d w h =
  (<$>) (M.multiply transform3dMatrixTo2dMatrix)
    >>> (<$>) (M.add (changeOrigin w h))

-- Origin shift: 3d-plane origin (0,0,0) projects to (0,0) on the 2d plane.
-- Centering on the canvas requires shifting by half its dimensions.

changeOrigin :: Number → Number → Matrix Number
changeOrigin w h =
  M.fromArray 2 1 [ w / 2.0, h / 2.0 ]
    # fromMaybe (M.zeros 2 1)


-- Value Determines size of the Cube

identityMatrix_2_4 :: Number → Matrix Number
identityMatrix_2_4 v =
  M.fromArray 2 4 [ v, 0.0, 0.0, 0.0,
                  0.0, v, 0.0, 0.0 ]
  # maybe
    (M.zeros 2 4)
    identity


-- *** Rotation Matrix for each plane *** --

rotateX :: Number -> Matrix Number
rotateX deg = M.fromArray 4 4
  [ one  , zero    , zero     , zero,
    zero , costeta , -sinteta , zero,
    zero , sinteta , costeta  , zero,
    zero , zero    , zero     , one
  ]
  # maybe (M.zeros 4 4) identity
  where
        costeta = cos (deg * pi / 180.0)
        sinteta = sin (deg * pi / 180.0)

rotateY :: Number -> Matrix Number
rotateY deg = M.fromArray 4 4
  [ costeta  , zero , sinteta , zero,
    zero     , one  , zero    , zero,
    -sinteta , zero , costeta , zero,
    zero     , zero , zero    , one
  ]
  # maybe (M.zeros 4 4) identity
  where
        costeta = cos (deg * pi / 180.0)
        sinteta = sin (deg * pi / 180.0)

rotateZ :: Number -> Matrix Number
rotateZ deg = M.fromArray 4 4
  [ costeta , -sinteta , zero , zero,
    sinteta , costeta  , zero , zero,
    zero    , zero     , one  , zero,
    zero    , zero     , zero , one
  ]
  # maybe (M.zeros 4 4) identity
  where
        costeta = cos (deg * pi / 180.0)
        sinteta = sin (deg * pi / 180.0)

transformation :: String -> Matrix Number
transformation key = case key of
  "ArrowLeft"  → rotateY        (unsafePerformEffect Helper.incSpeed)
  "ArrowRight" → rotateY (negate (unsafePerformEffect Helper.incSpeed))
  "ArrowUp"    → rotateX        (unsafePerformEffect Helper.incSpeed)
  "ArrowDown"  → rotateX (negate (unsafePerformEffect Helper.incSpeed))
  _            → rotateZ 0.0
