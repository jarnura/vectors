module Main where

import Prelude

import Cube as C
import Data.Int (toNumber)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Number (pi, tan)
import Effect (Effect)
import Effect.Console (log)
import Effect.Ref as Ref
import FRP.Loop (Input, runLoop)
import Graphics.Canvas
  ( CanvasElement
  , getCanvasElementById
  , getCanvasHeight
  , getCanvasWidth
  )
import Graphics.GL (Renderer)
import Graphics.GL as GL
import Math.Matrix (Matrix)
import Math.Matrix as M
import Vector as V

type State =
  { transform :: Matrix Number
  , speed     :: Number
  , mouseLast :: Maybe { x :: Int, y :: Int }
  }

-- Maximum rotation per keypress, in degrees.
maxSpeed :: Number
maxSpeed = 10.0

-- Additional rotation each keypress accrues on top of the previous one.
speedStep :: Number
speedStep = 0.2

-- Degrees of rotation per pixel of mouse movement.
mouseSensitivity :: Number
mouseSensitivity = 0.1

-- Vertical field of view, in radians.
fov :: Number
fov = pi / 3.0

-- Distance from the camera to the cube center along -Z.
cameraDistance :: Number
cameraDistance = 1000.0

-- Near/far clip planes (positive distances; the projection matrix negates them).
clipNear :: Number
clipNear = 1.0

clipFar :: Number
clipFar = 2000.0

identityMatrix :: Matrix Number
identityMatrix = fromMaybe (M.zeros 4 4) $ M.fromArray 4 4
  [ 1.0, 0.0, 0.0, 0.0
  , 0.0, 1.0, 0.0, 0.0
  , 0.0, 0.0, 1.0, 0.0
  , 0.0, 0.0, 0.0, 1.0
  ]

initialState :: State
initialState =
  { transform: identityMatrix
  , speed: 0.0
  , mouseLast: Nothing
  }

-- Perspective projection composed with a camera-distance translation.
-- The cube sits at the world origin; the camera looks down -Z from
-- (0, 0, +cameraDistance).
perspectiveProjection :: Number -> Number -> Matrix Number
perspectiveProjection w h =
  let
    aspect = w / h
    f      = 1.0 / tan (fov / 2.0)
    p      = fromMaybe (M.zeros 4 4) $ M.fromArray 4 4
      [ f / aspect , 0.0 , 0.0                                    , 0.0
      , 0.0        , f   , 0.0                                    , 0.0
      , 0.0        , 0.0 , (clipFar + clipNear) / (clipNear - clipFar)
                         , (2.0 * clipFar * clipNear) / (clipNear - clipFar)
      , 0.0        , 0.0 , -1.0                                   , 0.0
      ]
    v = fromMaybe (M.zeros 4 4) $ M.fromArray 4 4
      [ 1.0 , 0.0 , 0.0 , 0.0
      , 0.0 , 1.0 , 0.0 , 0.0
      , 0.0 , 0.0 , 1.0 , -cameraDistance
      , 0.0 , 0.0 , 0.0 , 1.0
      ]
  in M.multiply p v

step :: Input -> State -> State
step input = applyKey input.lastKey >>> applyMouse input.mouse

applyKey :: Maybe String -> State -> State
applyKey Nothing s = s
applyKey (Just key) s =
  let nextSpeed = if s.speed < maxSpeed then s.speed + speedStep else s.speed
      rotation  = case key of
        "ArrowLeft"  -> Just (V.rotateY nextSpeed)
        "ArrowRight" -> Just (V.rotateY (negate nextSpeed))
        "ArrowUp"    -> Just (V.rotateX nextSpeed)
        "ArrowDown"  -> Just (V.rotateX (negate nextSpeed))
        _            -> Nothing
  in case rotation of
       Nothing -> s
       Just r  -> s { transform = M.multiply r s.transform
                    , speed     = nextSpeed
                    }

applyMouse :: Maybe { x :: Int, y :: Int } -> State -> State
applyMouse Nothing s = s
applyMouse (Just pos) s = case s.mouseLast of
  Nothing   -> s { mouseLast = Just pos }
  Just last ->
    let dx = toNumber pos.x - toNumber last.x
        dy = toNumber pos.y - toNumber last.y
        r  = M.multiply
               (V.rotateX (negate (dy * mouseSensitivity)))
               (V.rotateY        (dx * mouseSensitivity))
    in s { transform = M.multiply r s.transform
         , mouseLast = Just pos
         }

-- Push the current canvas dimensions into the GL viewport + projection
-- uniform. Called once at startup and on every frame where the canvas
-- size has changed (handles window resize).
updateViewport :: Renderer -> CanvasElement -> Effect Unit
updateViewport renderer canvas = do
  w <- getCanvasWidth  canvas
  h <- getCanvasHeight canvas
  GL.resizeRenderer renderer { width: w, height: h }
  GL.setProjection  renderer (M.toVector (perspectiveProjection w h))

main :: Effect Unit
main = do
  mcanvas <- getCanvasElementById "canvas"
  case mcanvas of
    Nothing -> log "Main: canvas element with id 'canvas' not found; aborting init"
    Just canvas -> do
      renderer <- GL.initRenderer canvas
      mesh     <- GL.createWireframeMesh renderer
                    { vertices: C.cubeVertices
                    , indices:  C.cubeEdgeIndices
                    , color:    C.cubeColor
                    }
      updateViewport renderer canvas
      w0 <- getCanvasWidth  canvas
      h0 <- getCanvasHeight canvas
      sizeRef <- Ref.new { w: w0, h: h0 }
      runLoop
        { initial: initialState
        , step
        , draw: \s -> do
            w <- getCanvasWidth  canvas
            h <- getCanvasHeight canvas
            prev <- Ref.read sizeRef
            when (prev.w /= w || prev.h /= h) do
              Ref.write { w, h } sizeRef
              updateViewport renderer canvas
            GL.beginFrame renderer
            GL.drawMesh renderer mesh (M.toVector s.transform)
        }
