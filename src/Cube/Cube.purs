module Cube where

import VectorPrelude

import Cube.Types (CubePoint, Cube)

import Graphics.Drawing (closed)

import LinearAlgebra.Matrix as M
import Utils                as Helper
import Vector               as V

point1 :: CubePoint
point1 = [-100.0, -100.0,  100.0,one]

point2 :: CubePoint
point2 = [ 100.0, -100.0,  100.0,one]

point3 :: CubePoint
point3 = [ 100.0,  100.0,  100.0,one]

point4 :: CubePoint
point4 = [-100.0,  100.0,  100.0,one]

point5 :: CubePoint
point5 = [-100.0, -100.0, -100.0,one]

point6 :: CubePoint
point6 = [-100.0,  100.0, -100.0,one]

point7 :: CubePoint
point7 = [ 100.0,  100.0, -100.0,one]

point8 :: CubePoint
point8 = [ 100.0, -100.0, -100.0,one]

edges :: Array (Array Int)
edges = [ [0,1], [1,2], [2,3], [3,0],
          [4,5], [5,6], [6,7], [7,4],
          [0,4], [1,7], [2,6], [3,5]
        ]

renderCube :: Matrix Number → Cube → Drawing
renderCube trans =
  (<$>) (M.multiply trans)
    >>> Helper.storeMatrix
    >>> V.projectOn2d
    >>> drawEdges
    >>> foldMap renderSide

cubePoints :: Array CubePoint
cubePoints = map (mulScalar 0.5) [point1,point2,point3,point4,point5,point6,point7,point8]

initcube :: Cube
initcube = cubePoints # map M.fromColumn

drawEdges :: Cube → Array (Array Point)
drawEdges points = foldl (\acc x -> snoc acc [(from x),(to x)]) [] edges
  where
        from x = Helper.vectorToPoint $ fromMaybe (M.zeros 2 1) $ points !! (fromMaybe 0 (x !! 0))
        to   x = Helper.vectorToPoint $ fromMaybe (M.zeros 2 1) $ points !! (fromMaybe 0 (x !! 1))

renderSide :: Array Point → Drawing
renderSide = closed >>> outlined ((outlineColor black) <> (lineWidth 0.5))

