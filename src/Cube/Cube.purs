module Cube
  ( initcube
  , renderCube
  ) where

import Prelude

import Color (black)
import Cube.Types (Cube, CubePoint)
import Data.Array ((!!))
import Data.Foldable (foldMap)
import Data.Maybe (fromMaybe)
import Graphics.Drawing2D (Drawing, Point, closed, lineWidth, outlineColor, outlined)
import Math.Matrix (Matrix, fromColumn, mulScalar, toVector, zeros)
import Vector as V

-- Half-extent of the unit cube before scaling.
cubeHalfExtent :: Number
cubeHalfExtent = 100.0

-- Uniform scale applied to all cube vertices at initialization.
cubeScale :: Number
cubeScale = 0.5

-- Stroke width for cube edges.
edgeLineWidth :: Number
edgeLineWidth = 0.5

point1 :: CubePoint
point1 = [ -cubeHalfExtent, -cubeHalfExtent,  cubeHalfExtent, 1.0 ]

point2 :: CubePoint
point2 = [  cubeHalfExtent, -cubeHalfExtent,  cubeHalfExtent, 1.0 ]

point3 :: CubePoint
point3 = [  cubeHalfExtent,  cubeHalfExtent,  cubeHalfExtent, 1.0 ]

point4 :: CubePoint
point4 = [ -cubeHalfExtent,  cubeHalfExtent,  cubeHalfExtent, 1.0 ]

point5 :: CubePoint
point5 = [ -cubeHalfExtent, -cubeHalfExtent, -cubeHalfExtent, 1.0 ]

point6 :: CubePoint
point6 = [ -cubeHalfExtent,  cubeHalfExtent, -cubeHalfExtent, 1.0 ]

point7 :: CubePoint
point7 = [  cubeHalfExtent,  cubeHalfExtent, -cubeHalfExtent, 1.0 ]

point8 :: CubePoint
point8 = [  cubeHalfExtent, -cubeHalfExtent, -cubeHalfExtent, 1.0 ]

edges :: Array (Array Int)
edges =
  [ [0,1], [1,2], [2,3], [3,0]
  , [4,5], [5,6], [6,7], [7,4]
  , [0,4], [1,7], [2,6], [3,5]
  ]

cubePoints :: Array CubePoint
cubePoints = map (mulScalar cubeScale)
  [ point1, point2, point3, point4, point5, point6, point7, point8 ]

initcube :: Cube
initcube = map fromColumn cubePoints

vectorToPoint :: Matrix Number -> Point
vectorToPoint m =
  let v = toVector m
  in { x: fromMaybe 0.0 (v !! 0), y: fromMaybe 0.0 (v !! 1) }

drawEdges :: Cube -> Array (Array Point)
drawEdges points = map edgePoints edges
  where
    edgePoints edge =
      [ vectorAt edge 0
      , vectorAt edge 1
      ]
    vectorAt edge idx =
      vectorToPoint $ fromMaybe (zeros 2 1) $
        points !! fromMaybe 0 (edge !! idx)

renderSide :: Array Point -> Drawing
renderSide =
  closed >>> outlined (outlineColor black <> lineWidth edgeLineWidth)

renderCube :: Number -> Number -> Cube -> Drawing
renderCube w h cube =
  V.projectOn2d w h cube # drawEdges # foldMap renderSide
