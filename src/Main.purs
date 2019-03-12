module Main where

import Prelude

import Color (black, white)
import Control.Monad.List.Trans (take)
import Data.Array ((!!))
import Data.Array (foldl, snoc, take, takeEnd) as Arr
import Data.Foldable (foldMap)
import Data.Int (toNumber)
import Data.Maybe (Maybe(..), fromJust, fromMaybe, maybe)
import Data.Tuple (Tuple(..), snd)
import Debug.Trace (spy)
import Effect (Effect)
import Effect.Console (log)
import FRP.Behavior (Behavior, animate, unfold)
import FRP.Behavior.Mouse (position) as Mou
import FRP.Event.Keyboard (down)
import FRP.Event.Mouse (Mouse)
import FRP.Event.Mouse (down, getMouse, move) as Mouse
import Graphics.Canvas (getCanvasElementById, getCanvasHeight, getCanvasWidth, getContext2D, setCanvasHeight, setCanvasWidth)
import Graphics.Drawing (Drawing, Point, closed, fillColor, filled, lineWidth, outlineColor, outlined, path, rectangle, render)
import LinearAlgebra.Matrix (Matrix, fromArray, fromColumn, multiply, zeros)
import LinearAlgebra.Matrix (add, fromColumn, multiply, toVector, zeros) as M
import LinearAlgebra.Vector (Vector, mulScalar)
import Math (cos, pi, sin)
import Partial.Unsafe (unsafePartial)

foreign import logAny :: forall a. a -> a
foreign import storeMatrix :: Array (Matrix Number) -> Array (Matrix Number)
foreign import getCachedMatrix :: Unit -> Array (Matrix Number)
foreign import getX :: Unit -> Number
foreign import getY :: Unit -> Number
foreign import storeX :: Number -> Number
foreign import storeY :: Number -> Number

foreign import getrotX :: Unit -> Number
foreign import getrotY :: Unit -> Number
foreign import storerotX :: Number -> Number
foreign import storerotY :: Number -> Number

point1 :: Vector Number
point1 = [-100.0, -100.0, 100.0,one]

point2 :: Vector Number
point2 = [100.0, -100.0, 100.0,one]

point3 :: Vector Number
point3 = [100.0, 100.0, 100.0,one]

point4 :: Vector Number
point4 = [-100.0, 100.0, 100.0,one]

point5 :: Vector Number
point5 = [-100.0, -100.0, -100.0,one]

point6 :: Vector Number
point6 = [-100.0, 100.0, -100.0,one]

point7 :: Vector Number
point7 = [100.0, 100.0, -100.0,one]

point8 :: Vector Number
point8 = [100.0, -100.0, -100.0,one]

edges :: Array (Array Int)
edges = [
  [0,1],
  [1,2],
  [2,3],
  [3,0],
  [4,5],
  [5,6],
  [6,7],
  [7,4],
  [0,4],
  [1,7],
  [2,6],
  [3,5]
  ]

scalar :: Vector Number
scalar = [2.0]

cubePoints :: Array (Vector Number)
cubePoints = map (mulScalar 1.0) [point1,point2,point3,point4,point5,point6,point7,point8]

transform3dto2d :: Matrix Number
transform3dto2d = fromArray 2 4
  [ 1.0 + one, zero, zero, zero,
    zero, 1.0 + one, zero, zero ] # maybe (M.zeros 2 4) identity

projection2d :: Array (Matrix Number) -> Array (Matrix Number)
projection2d =
  map (M.multiply transform3dto2d)
  >>> map (M.add
            ((fromArray 2 1 [500.0,500.0])
              # maybe (M.zeros 2 1) identity)
          )

rotateX :: Number -> Matrix Number
rotateX deg = fromArray 4 4
  [ one, zero,        zero, zero,
    zero, (costeta), -(sinteta), zero,
    zero, (sinteta), (costeta), zero,
    zero, zero, zero, one
  ]
  # maybe (M.zeros 4 4) identity
  where
        costeta = cos (deg * pi / 180.0)
        sinteta = sin (deg * pi / 180.0)

rotateY :: Number -> Matrix Number
rotateY deg = fromArray 4 4
  [ (costeta), zero, (sinteta), zero,
    zero,        one, zero, zero,
    -(sinteta),zero, (costeta), zero,
    zero, zero, zero, one
  ]
  # maybe (M.zeros 4 4) identity
  where
        costeta = cos (deg * pi / 180.0)
        sinteta = sin (deg * pi / 180.0)

rotateZ :: Number -> Matrix Number
rotateZ deg = fromArray 4 4
  [ (costeta), -(sinteta), zero, zero,
    (sinteta), (costeta),  zero, zero,
    zero, zero, one, zero,
    zero, zero, zero, one
  ]
  # maybe (M.zeros 4 4) identity
  where
        costeta = cos (deg * pi / 180.0)
        sinteta = sin (deg * pi / 180.0)

scene :: Mouse -> { w :: Number, h :: Number } -> Behavior Drawing
scene mouse {w,h} = (unfold loop2 down init) <> (loop <$> (Mou.position mouse))
  where
        loop2 :: String -> Drawing -> Drawing
        loop2 key _ = do
           let trans = transformation key
           (background <> (renderCube trans $ getcube unit))

        loop :: Maybe {x::Int,y::Int} -> Drawing
        loop (Just {x,y}) = do
           let trans = transformation ""
               xdeg = (toNumber x) - (getX unit)
               ydeg = (toNumber y) - (getY unit)
               _ = storeX (toNumber x)
               _ = storeY (toNumber y)
               trans2 = M.multiply (rotateX (-(ydeg * 0.1))) (rotateY (xdeg * 0.1))
           (background <>
           (renderCube trans2 $ getcube unit))
        loop Nothing = background

        transformation :: String -> Matrix Number
        transformation "ArrowLeft" = rotateY 1.0
        transformation "ArrowRight" = rotateY (-1.0)
        transformation "ArrowUp" = rotateX 1.0
        transformation "ArrowDown" = rotateX (-1.0)
        transformation _ = rotateZ 1.0

        getcube :: Unit -> Array (Matrix Number)
        getcube = getCachedMatrix

        init :: Drawing
        init = getCachedMatrix unit # projection2d >>> logAny >>> drawEdges >>> foldMap renderSide

        background :: Drawing
        background = filled (fillColor white) (rectangle 0.0 0.0 w h)

        renderSide :: Array Point -> Drawing
        renderSide = closed >>> outlined ((outlineColor black) <> (lineWidth 2.0))

        vectorToPoint :: Matrix Number -> Point
        vectorToPoint = M.toVector >>> (\arr -> { x: fromMaybe zero (arr !! 0), y: fromMaybe zero (arr !! 1)})

        renderCube :: Matrix Number -> Array (Matrix Number) -> Drawing
        renderCube trans =
          map (M.multiply (logAny $ trans))
          {-- >>> map (M.multiply (rotateX 0.5)) --}
          {-- >>> map (M.multiply (rotateZ 0.5)) --}
          >>> storeMatrix
          >>> projection2d
          >>> drawEdges
          >>> foldMap renderSide

        drawEdges :: Array (Matrix Number) -> Array (Array Point)
        drawEdges points = Arr.foldl (\acc x -> Arr.snoc acc [(from x),(to x)]) [] edges
          where
                from x = vectorToPoint $ fromMaybe (M.zeros 2 1) $ points !! (fromMaybe 0 (x !! 0))
                to x = vectorToPoint $ fromMaybe (M.zeros 2 1) $ points !! (fromMaybe 0 (x !! 1))


initcube :: Array (Matrix Number)
initcube = cubePoints # map M.fromColumn


main :: Effect Unit
main = do
  mcanvas <- getCanvasElementById "canvas"
  let canvas = unsafePartial (fromJust mcanvas)
  ctx <- getContext2D canvas
  w <- getCanvasWidth canvas
  h <- getCanvasHeight canvas
  _ <- setCanvasWidth canvas w
  _ <- setCanvasHeight canvas h
  _ <- pure (storeMatrix initcube)
  mouse <- Mouse.getMouse
  _ <- animate (scene mouse {w,h}) (render ctx)
  log $ show (M.fromColumn point1)
  {-- log $ show (projection2d cubePoints) --}
  pure unit
