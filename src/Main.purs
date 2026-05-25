module Main where

import VectorPrelude

import Cube                 as C
import FRP.Behavior.Mouse   as Mouse
import FRP.Event.Keyboard   as Key
import LinearAlgebra.Matrix as M
import Utils                as Helper
import Vector               as V

-- Degrees of rotation applied per pixel of mouse movement.
mouseSensitivity :: Number
mouseSensitivity = 0.1

scene :: Keyboard → Mouse → { w :: Number, h :: Number } → Behavior Drawing
scene keyboard mouse {w,h} =
  (unfold keyboardEvent Key.up init) <> (mouseEvent <$> (Mouse.position mouse))
  where

        init = unsafePerformEffect do
          cube <- Helper.getCachedMatrix
          pure (V.projectOn2d w h cube # C.drawEdges # foldMap C.renderSide)

        background = filled (fillColor white) (rectangle 0.0 0.0 w h)

        keyboardEvent value _ = unsafePerformEffect do
          cube <- Helper.getCachedMatrix
          pure (background <> C.renderCube w h (V.transformation value) cube)

        mouseEvent :: Maybe {x::Int,y::Int} -> Drawing
        mouseEvent (Just {x,y}) = unsafePerformEffect do
          oldX <- Helper.getX
          oldY <- Helper.getY
          Helper.storeX (toNumber x)
          Helper.storeY (toNumber y)
          cube <- Helper.getCachedMatrix
          let xdeg  = toNumber x - oldX
              ydeg  = toNumber y - oldY
              trans = M.multiply
                       (V.rotateX (negate (ydeg * mouseSensitivity)))
                       (V.rotateY        (xdeg * mouseSensitivity))
          pure (background <> C.renderCube w h trans cube)

        mouseEvent Nothing = background

main :: Effect Unit
main = do
  mcanvas <- getCanvasElementById "canvas"
  case mcanvas of
    Nothing -> log "Main: canvas element with id 'canvas' not found; aborting init"
    Just canvas -> do
      Helper.storeMatrix C.initcube
      ctx      <- getContext2D    canvas
      w        <- getCanvasWidth  canvas
      h        <- getCanvasHeight canvas
      _        <- setCanvasWidth  canvas w
      _        <- setCanvasHeight canvas h
      keyboard <- getKeyboard
      mouse    <- getMouse
      _        <- animate (scene keyboard mouse {w,h}) (render ctx)
      pure unit
