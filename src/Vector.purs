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

projectOn2d :: Cube -> Cube
projectOn2d =
  (<$>) (M.multiply transform3dMatrixTo2dMatrix)
    >>> (<$>) (M.add changeOrigin)

-- The origin of a 3d plane is 0,0,0 as projection on 2d plane its origin 0,0
--   If the origin is 0,0 we cant see the negative side projection in negative axis
--   To overcome this move origin to point (more than the length of object) in positive axis

changeOrigin :: Matrix Number
changeOrigin =
  M.fromArray 2 1 [500.0,500.0]
    # maybe (M.zeros 2 1) identity


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
  "ArrowLeft"  → rotateY $ Helper.incSpeed unit
  "ArrowRight" → rotateY $ (-1.0) * Helper.incSpeed unit
  "ArrowUp"    → rotateX $ Helper.incSpeed unit
  "ArrowDown"  → rotateX $ (-1.0) * Helper.incSpeed unit
  _            → rotateZ $ 0.0
