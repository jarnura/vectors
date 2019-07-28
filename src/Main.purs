module Main where

import VectorPrelude

import Cube                 as C
import FRP.Behavior.Mouse   as Mouse
import FRP.Event.Keyboard   as Key
import LinearAlgebra.Matrix as M
import Utils                as Helper
import Vector               as V

scene :: Keyboard → Mouse → { w :: Number, h :: Number } → Behavior Drawing
scene keyboard mouse {w,h} =
  (unfold keyboardEvent Key.up init) <> (mouseEvent <$> (Mouse.position mouse))
  where

        getcube    = Helper.getCachedMatrix

        init       = Helper.getCachedMatrix unit # V.projectOn2d >>> C.drawEdges >>> foldMap C.renderSide

        background = filled (fillColor white) (rectangle 0.0 0.0 w h)

        keyboardEvent value _ = background <> (C.renderCube (V.transformation value) $ getcube unit)

        mouseEvent :: Maybe {x::Int,y::Int} -> Drawing
        mouseEvent (Just {x,y}) = do
           let _      = Helper.storeX (toNumber x)
               _      = Helper.storeY (toNumber y)
               trans = M.multiply (V.rotateX (-(ydeg * 0.1))) (V.rotateY (xdeg * 0.1))

           background <> (C.renderCube trans $ getcube unit)

           where
                 xdeg   = (toNumber x) - (Helper.getX unit)
                 ydeg   = (toNumber y) - (Helper.getY unit)

        mouseEvent Nothing = background

main :: Effect Unit
main = do
  mcanvas    ← getCanvasElementById "canvas"
  let canvas = unsafePartial   (fromJust mcanvas)
      _      = Helper.storeMatrix C.initcube
  ctx        ← getContext2D    canvas
  w          ← getCanvasWidth  canvas
  h          ← getCanvasHeight canvas
  _          ← setCanvasWidth  canvas w
  _          ← setCanvasHeight canvas h
  keyboard   ← getKeyboard
  mouse      ← getMouse
  _          ← animate (scene keyboard mouse {w,h}) (render ctx)
  pure unit

{--(unfold keyboardEvent (withKeys keyboard Key.down) init)--}
