module Graphics.GL
  ( Renderer
  , Mesh
  , SolidMesh
  , Color
  , initRenderer
  , createWireframeMesh
  , createSolidMesh
  , drawMesh
  , drawSolidMesh
  , beginFrame
  , setProjection
  , resizeRenderer
  ) where

import Prelude

import Effect (Effect)
import Graphics.Canvas (CanvasElement)

foreign import data Renderer :: Type
foreign import data Mesh :: Type
foreign import data SolidMesh :: Type

type Color = { r :: Number, g :: Number, b :: Number, a :: Number }

-- Compile shaders, allocate programs, set up GL state. Throws if WebGL2 is
-- unavailable or shader compilation fails.
foreign import initRenderer :: CanvasElement -> Effect Renderer

-- Upload a wireframe mesh: vertex positions [x0,y0,z0, ...] and pairwise
-- line indices [i0,j0, i1,j1, ...].
foreign import createWireframeMesh
  :: Renderer
  -> { vertices :: Array Number
     , indices :: Array Int
     , color :: Color
     }
  -> Effect Mesh

-- Upload a solid lit mesh: positions, per-vertex normals, and triangle
-- indices [i0,j0,k0, i1,j1,k1, ...] wound CCW when viewed from outside.
foreign import createSolidMesh
  :: Renderer
  -> { vertices :: Array Number
     , normals :: Array Number
     , indices :: Array Int
     , color :: Color
     }
  -> Effect SolidMesh

-- Clear the framebuffer at the start of a frame.
foreign import beginFrame :: Renderer -> Effect Unit

-- Upload the projection matrix uniform (row-major 16 floats) to both
-- shader programs.
foreign import setProjection :: Renderer -> Array Number -> Effect Unit

-- Draw a wireframe mesh with the given model matrix (row-major 16 floats).
foreign import drawMesh :: Renderer -> Mesh -> Array Number -> Effect Unit

-- Draw a solid mesh with directional lighting.
foreign import drawSolidMesh
  :: Renderer -> SolidMesh -> Array Number -> Effect Unit

-- Resize the GL viewport. Pass the canvas's current pixel dimensions.
foreign import resizeRenderer
  :: Renderer -> { width :: Number, height :: Number } -> Effect Unit
