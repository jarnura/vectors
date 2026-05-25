module Main where

import Prelude

import Cube as C
import Data.Int (toNumber)
import Data.Maybe (Maybe(..), fromMaybe)
import Effect (Effect)
import Effect.Console (log)
import FRP.Loop (Input, runLoop)
import Graphics.Canvas
  ( getCanvasElementById
  , getCanvasHeight
  , getCanvasWidth
  )
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

-- Orthographic projection centered on origin: [-w/2, w/2] x [-h/2, h/2].
-- Depth range [-far, far] keeps the cube visible regardless of orientation.
orthoProjection :: Number -> Number -> Matrix Number
orthoProjection w h =
  let far = 1000.0
  in fromMaybe (M.zeros 4 4) $ M.fromArray 4 4
       [ 2.0 / w , 0.0     , 0.0          , 0.0
       , 0.0     , 2.0 / h , 0.0          , 0.0
       , 0.0     , 0.0     , -1.0 / far   , 0.0
       , 0.0     , 0.0     , 0.0          , 1.0
       ]

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

main :: Effect Unit
main = do
  mcanvas <- getCanvasElementById "canvas"
  case mcanvas of
    Nothing -> log "Main: canvas element with id 'canvas' not found; aborting init"
    Just canvas -> do
      w <- getCanvasWidth  canvas
      h <- getCanvasHeight canvas
      renderer <- GL.initRenderer canvas
      mesh     <- GL.createWireframeMesh renderer
                    { vertices: C.cubeVertices
                    , indices:  C.cubeEdgeIndices
                    , color:    C.cubeColor
                    }
      GL.setProjection renderer (M.toVector (orthoProjection w h))
      runLoop
        { initial: initialState
        , step
        , draw: \s -> do
            GL.beginFrame renderer
            GL.drawMesh renderer mesh (M.toVector s.transform)
        }
