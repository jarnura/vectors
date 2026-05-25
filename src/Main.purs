module Main where

import Prelude

import Color (white)
import Cube as C
import Cube.Types (Cube)
import Data.Int (toNumber)
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Console (log)
import FRP.Loop (Input, runLoop)
import Graphics.Canvas
  ( getCanvasElementById
  , getCanvasHeight
  , getCanvasWidth
  , getContext2D
  , setCanvasHeight
  , setCanvasWidth
  )
import Graphics.Drawing2D (Drawing, fillColor, filled, rectangle, render)
import Math.Matrix as M
import Vector as V

type State =
  { cube      :: Cube
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

initialState :: State
initialState =
  { cube: C.initcube
  , speed: 0.0
  , mouseLast: Nothing
  }

step :: Input -> State -> State
step input = applyKey input.lastKey >>> applyMouse input.mouse

applyKey :: Maybe String -> State -> State
applyKey Nothing s = s
applyKey (Just key) s =
  let nextSpeed = if s.speed < maxSpeed then s.speed + speedStep else s.speed
      transform = case key of
        "ArrowLeft"  -> Just (V.rotateY nextSpeed)
        "ArrowRight" -> Just (V.rotateY (negate nextSpeed))
        "ArrowUp"    -> Just (V.rotateX nextSpeed)
        "ArrowDown"  -> Just (V.rotateX (negate nextSpeed))
        _            -> Nothing
  in case transform of
       Nothing -> s
       Just t  -> s { cube = map (M.multiply t) s.cube
                    , speed = nextSpeed
                    }

applyMouse :: Maybe { x :: Int, y :: Int } -> State -> State
applyMouse Nothing s = s
applyMouse (Just pos) s = case s.mouseLast of
  Nothing   -> s { mouseLast = Just pos }
  Just last ->
    let dx = toNumber pos.x - toNumber last.x
        dy = toNumber pos.y - toNumber last.y
        t  = M.multiply
               (V.rotateX (negate (dy * mouseSensitivity)))
               (V.rotateY        (dx * mouseSensitivity))
    in s { cube      = map (M.multiply t) s.cube
         , mouseLast = Just pos
         }

draw :: { w :: Number, h :: Number } -> State -> Drawing
draw { w, h } s =
  filled (fillColor white) (rectangle 0.0 0.0 w h)
    <> C.renderCube w h s.cube

main :: Effect Unit
main = do
  mcanvas <- getCanvasElementById "canvas"
  case mcanvas of
    Nothing -> log "Main: canvas element with id 'canvas' not found; aborting init"
    Just canvas -> do
      ctx <- getContext2D canvas
      w   <- getCanvasWidth  canvas
      h   <- getCanvasHeight canvas
      _   <- setCanvasWidth  canvas w
      _   <- setCanvasHeight canvas h
      runLoop
        { initial: initialState
        , step
        , draw: \s -> render ctx (draw { w, h } s)
        }
