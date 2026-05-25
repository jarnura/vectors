module Cube
  ( cubeVertices
  , cubeEdgeIndices
  , cubeColor
  ) where

import Prelude

import Graphics.GL (Color)

-- Half-edge length of the cube. The cube spans [-h, +h] on each axis.
-- Picked to match the original Canvas2D-era visual size (~200px wide on a
-- 1920×1080 canvas after orthographic projection).
halfExtent :: Number
halfExtent = 100.0

-- 8 corner positions, flattened as [x0,y0,z0, x1,y1,z1, ...].
cubeVertices :: Array Number
cubeVertices =
  let h = halfExtent
  in
  [ -h, -h,  h   -- 0: front bottom-left
  ,  h, -h,  h   -- 1: front bottom-right
  ,  h,  h,  h   -- 2: front top-right
  , -h,  h,  h   -- 3: front top-left
  , -h, -h, -h   -- 4: back bottom-left
  , -h,  h, -h   -- 5: back top-left
  ,  h,  h, -h   -- 6: back top-right
  ,  h, -h, -h   -- 7: back bottom-right
  ]

-- 12 edges as pairs of vertex indices, flattened for gl.drawElements(LINES).
cubeEdgeIndices :: Array Int
cubeEdgeIndices =
  [ 0,1, 1,2, 2,3, 3,0   -- front face
  , 4,5, 5,6, 6,7, 7,4   -- back face
  , 0,4, 1,7, 2,6, 3,5   -- connecting edges
  ]

-- Black, fully opaque.
cubeColor :: Color
cubeColor = { r: 0.0, g: 0.0, b: 0.0, a: 1.0 }
