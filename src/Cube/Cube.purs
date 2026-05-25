module Cube where

import VectorPrelude

import Cube.Types (CubePoint, Cube)

import Graphics.Drawing (closed)

import LinearAlgebra.Matrix as M
import Utils                as Helper
import Vector               as V

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
point1 = [ negate cubeHalfExtent, negate cubeHalfExtent,        cubeHalfExtent, one ]

point2 :: CubePoint
point2 = [        cubeHalfExtent, negate cubeHalfExtent,        cubeHalfExtent, one ]

point3 :: CubePoint
point3 = [        cubeHalfExtent,        cubeHalfExtent,        cubeHalfExtent, one ]

point4 :: CubePoint
point4 = [ negate cubeHalfExtent,        cubeHalfExtent,        cubeHalfExtent, one ]

point5 :: CubePoint
point5 = [ negate cubeHalfExtent, negate cubeHalfExtent, negate cubeHalfExtent, one ]

point6 :: CubePoint
point6 = [ negate cubeHalfExtent,        cubeHalfExtent, negate cubeHalfExtent, one ]

point7 :: CubePoint
point7 = [        cubeHalfExtent,        cubeHalfExtent, negate cubeHalfExtent, one ]

point8 :: CubePoint
point8 = [        cubeHalfExtent, negate cubeHalfExtent, negate cubeHalfExtent, one ]

edges :: Array (Array Int)
edges = [ [0,1], [1,2], [2,3], [3,0],
          [4,5], [5,6], [6,7], [7,4],
          [0,4], [1,7], [2,6], [3,5]
        ]

renderCube :: Number → Number → Matrix Number → Cube → Drawing
renderCube w h trans cube = unsafePerformEffect do
  let updated = map (M.multiply trans) cube
  Helper.storeMatrix updated
  pure (V.projectOn2d w h updated # drawEdges # foldMap renderSide)

cubePoints :: Array CubePoint
cubePoints = map (mulScalar cubeScale) [point1,point2,point3,point4,point5,point6,point7,point8]

initcube :: Cube
initcube = cubePoints # map M.fromColumn

drawEdges :: Cube → Array (Array Point)
drawEdges points = map (\edge -> [from edge, to edge]) edges
  where
        from edge = Helper.vectorToPoint $ fromMaybe (M.zeros 2 1) $ points !! (fromMaybe 0 (edge !! 0))
        to   edge = Helper.vectorToPoint $ fromMaybe (M.zeros 2 1) $ points !! (fromMaybe 0 (edge !! 1))

renderSide :: Array Point → Drawing
renderSide = closed >>> outlined ((outlineColor black) <> (lineWidth edgeLineWidth))
