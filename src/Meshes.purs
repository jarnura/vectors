module Meshes
  ( MeshSpec
  , mainCube
  , satelliteCube
  ) where

import Prelude

import Graphics.GL (Color)

type MeshSpec =
  { vertices :: Array Number
  , indices  :: Array Int
  , color    :: Color
  }

-- The main user-controlled cube. Sized to match the original visual
-- (~200 px wide at 1280×720 with the current perspective).
mainCube :: MeshSpec
mainCube = cubeAt 100.0 black

-- A smaller satellite cube that orbits the main one.
satelliteCube :: MeshSpec
satelliteCube = cubeAt 25.0 red

cubeAt :: Number -> Color -> MeshSpec
cubeAt halfExtent color =
  { vertices: cubeVerticesAt halfExtent
  , indices:  cubeEdgeIndices
  , color
  }

cubeVerticesAt :: Number -> Array Number
cubeVerticesAt h =
  [ -h, -h,  h   -- 0: front bottom-left
  ,  h, -h,  h   -- 1: front bottom-right
  ,  h,  h,  h   -- 2: front top-right
  , -h,  h,  h   -- 3: front top-left
  , -h, -h, -h   -- 4: back bottom-left
  , -h,  h, -h   -- 5: back top-left
  ,  h,  h, -h   -- 6: back top-right
  ,  h, -h, -h   -- 7: back bottom-right
  ]

cubeEdgeIndices :: Array Int
cubeEdgeIndices =
  [ 0,1, 1,2, 2,3, 3,0   -- front face
  , 4,5, 5,6, 6,7, 7,4   -- back face
  , 0,4, 1,7, 2,6, 3,5   -- connecting edges
  ]

black :: Color
black = { r: 0.0, g: 0.0, b: 0.0, a: 1.0 }

red :: Color
red = { r: 0.85, g: 0.20, b: 0.20, a: 1.0 }
