module Main where

import Prelude

import Data.Foldable (for_)
import Data.Int (toNumber)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Number (cos, pi, sin, tan)
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
import Graphics.GL (Mesh, Renderer, SolidMesh)
import Graphics.GL as GL
import Math.Matrix (Matrix)
import Math.Matrix as M
import Meshes as Meshes
import Vector as V
import World (groundTransform, gridTransform, groundExtent, gridDivisions, skyColor)

type State =
  { transform :: Matrix Number
  , speed :: Number
  , mouseLast :: Maybe { x :: Int, y :: Int }
  , frame :: Number
  }

-- A renderable mesh is either a solid lit mesh or a wireframe mesh; the draw
-- loop dispatches on the constructor (exhaustively).
data EntityMesh
  = Solid SolidMesh
  | Wire Mesh

type Entity =
  { mesh :: EntityMesh
  , modelMatrix :: State -> Matrix Number
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

-- Distance from the camera to the world origin along -Z.
cameraDistance :: Number
cameraDistance = 1000.0

clipNear :: Number
clipNear = 1.0

clipFar :: Number
clipFar = 2000.0

-- Satellite orbits the world origin on the XZ plane.
satelliteOrbitRadius :: Number
satelliteOrbitRadius = 200.0

-- One full orbit in 5 seconds at 60 fps = 1.2 deg/frame.
satelliteDegreesPerFrame :: Number
satelliteDegreesPerFrame = 1.2

initialState :: State
initialState =
  { transform: M.identity
  , speed: 0.0
  , mouseLast: Nothing
  , frame: 0.0
  }

-- Perspective projection composed with a camera-distance translation.
perspectiveProjection :: Number -> Number -> Matrix Number
perspectiveProjection w h =
  let
    aspect = w / h
    f = 1.0 / tan (fov / 2.0)
    p = fromMaybe (M.zeros 4 4) $ M.fromArray 4 4
      [ f / aspect
      , 0.0
      , 0.0
      , 0.0
      , 0.0
      , f
      , 0.0
      , 0.0
      , 0.0
      , 0.0
      , (clipFar + clipNear) / (clipNear - clipFar)
      , (2.0 * clipFar * clipNear) / (clipNear - clipFar)
      , 0.0
      , 0.0
      , -1.0
      , 0.0
      ]
  in
    M.multiply p (M.translate 0.0 0.0 (-cameraDistance))

step :: Input -> State -> State
step input =
  applyKey input.lastKey
    >>> applyMouse input.mouse
    >>> tickFrame
  where
  tickFrame s = s { frame = s.frame + 1.0 }

applyKey :: Maybe String -> State -> State
applyKey Nothing s = s
applyKey (Just key) s =
  let
    nextSpeed = if s.speed < maxSpeed then s.speed + speedStep else s.speed
    rotation = case key of
      "ArrowLeft" -> Just (V.rotateY nextSpeed)
      "ArrowRight" -> Just (V.rotateY (negate nextSpeed))
      "ArrowUp" -> Just (V.rotateX nextSpeed)
      "ArrowDown" -> Just (V.rotateX (negate nextSpeed))
      _ -> Nothing
  in
    case rotation of
      Nothing -> s
      Just r -> s
        { transform = M.multiply r s.transform
        , speed = nextSpeed
        }

applyMouse :: Maybe { x :: Int, y :: Int } -> State -> State
applyMouse Nothing s = s
applyMouse (Just pos) s = case s.mouseLast of
  Nothing -> s { mouseLast = Just pos }
  Just last ->
    let
      dx = toNumber pos.x - toNumber last.x
      dy = toNumber pos.y - toNumber last.y
      r = M.multiply
        (V.rotateX (negate (dy * mouseSensitivity)))
        (V.rotateY (dx * mouseSensitivity))
    in
      s
        { transform = M.multiply r s.transform
        , mouseLast = Just pos
        }

-- Satellite's world-space transform: a circular orbit at constant Y=0.
satelliteTransform :: State -> Matrix Number
satelliteTransform s =
  let
    angleRad = s.frame * satelliteDegreesPerFrame * pi / 180.0
  in
    M.translate
      (satelliteOrbitRadius * cos angleRad)
      0.0
      (satelliteOrbitRadius * sin angleRad)

updateViewport :: Renderer -> CanvasElement -> Effect Unit
updateViewport renderer canvas = do
  w <- getCanvasWidth canvas
  h <- getCanvasHeight canvas
  GL.resizeRenderer renderer { width: w, height: h }
  GL.setProjection renderer (M.toVector (perspectiveProjection w h))

main :: Effect Unit
main = do
  mcanvas <- getCanvasElementById "canvas"
  case mcanvas of
    Nothing -> log "Main: canvas element with id 'canvas' not found; aborting init"
    Just canvas -> do
      renderer <- GL.initRenderer canvas
      GL.setClearColor renderer skyColor
      groundMesh <- GL.createSolidMesh renderer (Meshes.groundPlane groundExtent)
      gridMesh <- GL.createWireframeMesh renderer (Meshes.gridFloor groundExtent gridDivisions)
      mainMesh <- GL.createSolidMesh renderer Meshes.solidMainCube
      satMesh <- GL.createSolidMesh renderer Meshes.solidSatelliteCube
      let
        entities :: Array Entity
        entities =
          -- Ground + grid are static Entities: their model matrices ignore State.
          [ { mesh: Solid groundMesh, modelMatrix: \_ -> groundTransform }
          , { mesh: Wire gridMesh, modelMatrix: \_ -> gridTransform }
          , { mesh: Solid mainMesh, modelMatrix: _.transform }
          , { mesh: Solid satMesh, modelMatrix: satelliteTransform }
          ]
      updateViewport renderer canvas
      w0 <- getCanvasWidth canvas
      h0 <- getCanvasHeight canvas
      sizeRef <- Ref.new { w: w0, h: h0 }
      runLoop
        { initial: initialState
        , step
        , draw: \s -> do
            w <- getCanvasWidth canvas
            h <- getCanvasHeight canvas
            prev <- Ref.read sizeRef
            when (prev.w /= w || prev.h /= h) do
              Ref.write { w, h } sizeRef
              updateViewport renderer canvas
            GL.beginFrame renderer
            for_ entities \e ->
              case e.mesh of
                Solid m -> GL.drawSolidMesh renderer m (M.toVector (e.modelMatrix s))
                Wire m -> GL.drawMesh renderer m (M.toVector (e.modelMatrix s))
        }
