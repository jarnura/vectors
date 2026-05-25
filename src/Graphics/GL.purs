module Graphics.GL
  ( Renderer
  , Mesh
  , Color
  , initRenderer
  , createWireframeMesh
  , drawMesh
  , beginFrame
  , setProjection
  , resizeRenderer
  ) where

import Prelude

import Effect (Effect)
import Graphics.Canvas (CanvasElement)

foreign import data Renderer :: Type
foreign import data Mesh     :: Type

type Color = { r :: Number, g :: Number, b :: Number, a :: Number }

-- Compile shaders, allocate program, set up GL state. Throws if WebGL2 is
-- unavailable or shader compilation fails.
foreign import initRenderer :: CanvasElement -> Effect Renderer

-- Upload a wireframe mesh: vertex positions [x0,y0,z0, x1,y1,z1, ...] and
-- line indices [i0,j0, i1,j1, ...]. Indices are pairs because we draw GL_LINES.
foreign import createWireframeMesh
  :: Renderer
  -> { vertices :: Array Number
     , indices  :: Array Int
     , color    :: Color
     }
  -> Effect Mesh

-- Clear the framebuffer at the start of a frame.
foreign import beginFrame :: Renderer -> Effect Unit

-- Upload the projection matrix uniform (row-major 16 floats).
foreign import setProjection :: Renderer -> Array Number -> Effect Unit

-- Draw a mesh with the given model matrix (row-major 16 floats).
foreign import drawMesh :: Renderer -> Mesh -> Array Number -> Effect Unit

-- Resize the GL viewport. Pass the canvas's current pixel dimensions.
foreign import resizeRenderer
  :: Renderer -> { width :: Int, height :: Int } -> Effect Unit
