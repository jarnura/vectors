module Meshes
  ( MeshSpec
  , SolidSpec
  , mainCube
  , satelliteCube
  , solidMainCube
  , solidSatelliteCube
  ) where

import Prelude

import Graphics.GL (Color)

type MeshSpec =
  { vertices :: Array Number
  , indices  :: Array Int
  , color    :: Color
  }

type SolidSpec =
  { vertices :: Array Number
  , normals  :: Array Number
  , indices  :: Array Int
  , color    :: Color
  }

-- ───── Wireframe variants ─────────────────────────────────────────────

mainCube :: MeshSpec
mainCube = wireCubeAt 100.0 black

satelliteCube :: MeshSpec
satelliteCube = wireCubeAt 25.0 red

wireCubeAt :: Number -> Color -> MeshSpec
wireCubeAt h color =
  { vertices: wireCubeVertices h
  , indices:  wireCubeIndices
  , color
  }

wireCubeVertices :: Number -> Array Number
wireCubeVertices h =
  [ -h, -h,  h   -- 0: front bottom-left
  ,  h, -h,  h   -- 1: front bottom-right
  ,  h,  h,  h   -- 2: front top-right
  , -h,  h,  h   -- 3: front top-left
  , -h, -h, -h   -- 4: back bottom-left
  , -h,  h, -h   -- 5: back top-left
  ,  h,  h, -h   -- 6: back top-right
  ,  h, -h, -h   -- 7: back bottom-right
  ]

wireCubeIndices :: Array Int
wireCubeIndices =
  [ 0,1, 1,2, 2,3, 3,0   -- front face
  , 4,5, 5,6, 6,7, 7,4   -- back face
  , 0,4, 1,7, 2,6, 3,5   -- connecting edges
  ]

-- ───── Solid lit variants ─────────────────────────────────────────────

solidMainCube :: SolidSpec
solidMainCube = solidCubeAt 100.0 indigo

solidSatelliteCube :: SolidSpec
solidSatelliteCube = solidCubeAt 25.0 red

solidCubeAt :: Number -> Color -> SolidSpec
solidCubeAt h color =
  { vertices: solidCubeVertices h
  , normals:  solidCubeNormals
  , indices:  solidCubeIndices
  , color
  }

-- 24 vertices: 4 per face × 6 faces. Each face's vertices are listed CCW
-- when viewed from outside, so triangles are wound counter-clockwise for
-- the GPU's CCW front-face convention.
solidCubeVertices :: Number -> Array Number
solidCubeVertices h =
  [ -- Front (+Z): A,B,C,D
    -h,-h, h,    h,-h, h,    h, h, h,   -h, h, h
    -- Back (-Z): F,E,H,G
  ,  h,-h,-h,   -h,-h,-h,   -h, h,-h,    h, h,-h
    -- Top (+Y): D,C,G,H
  , -h, h, h,    h, h, h,    h, h,-h,   -h, h,-h
    -- Bottom (-Y): E,F,B,A
  , -h,-h,-h,    h,-h,-h,    h,-h, h,   -h,-h, h
    -- Right (+X): B,F,G,C
  ,  h,-h, h,    h,-h,-h,    h, h,-h,    h, h, h
    -- Left (-X): E,A,D,H
  , -h,-h,-h,   -h,-h, h,   -h, h, h,   -h, h,-h
  ]

solidCubeNormals :: Array Number
solidCubeNormals =
  [  0.0, 0.0, 1.0,    0.0, 0.0, 1.0,    0.0, 0.0, 1.0,    0.0, 0.0, 1.0
  ,  0.0, 0.0,-1.0,    0.0, 0.0,-1.0,    0.0, 0.0,-1.0,    0.0, 0.0,-1.0
  ,  0.0, 1.0, 0.0,    0.0, 1.0, 0.0,    0.0, 1.0, 0.0,    0.0, 1.0, 0.0
  ,  0.0,-1.0, 0.0,    0.0,-1.0, 0.0,    0.0,-1.0, 0.0,    0.0,-1.0, 0.0
  ,  1.0, 0.0, 0.0,    1.0, 0.0, 0.0,    1.0, 0.0, 0.0,    1.0, 0.0, 0.0
  , -1.0, 0.0, 0.0,   -1.0, 0.0, 0.0,   -1.0, 0.0, 0.0,   -1.0, 0.0, 0.0
  ]

-- 36 indices = 6 faces × 2 triangles × 3 vertices.
-- For face starting at vertex i (i, i+1, i+2, i+3), the two triangles are
-- (i, i+1, i+2) and (i, i+2, i+3).
solidCubeIndices :: Array Int
solidCubeIndices =
  [  0, 1, 2,    0, 2, 3   -- front
  ,  4, 5, 6,    4, 6, 7   -- back
  ,  8, 9,10,    8,10,11   -- top
  , 12,13,14,   12,14,15   -- bottom
  , 16,17,18,   16,18,19   -- right
  , 20,21,22,   20,22,23   -- left
  ]

-- ───── Colors ─────────────────────────────────────────────────────────

black :: Color
black = { r: 0.0, g: 0.0, b: 0.0, a: 1.0 }

red :: Color
red = { r: 0.85, g: 0.20, b: 0.20, a: 1.0 }

-- Deeper blue-purple — looks more vibrant under directional light than
-- pure black.
indigo :: Color
indigo = { r: 0.20, g: 0.30, b: 0.85, a: 1.0 }
