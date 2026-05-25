module Vector
  ( rotateX
  , rotateY
  , rotateZ
  ) where

import Prelude

import Data.Maybe (fromMaybe)
import Data.Number (cos, pi, sin)
import Math.Matrix (Matrix, fromArray, zeros)

rotateX :: Number -> Matrix Number
rotateX deg = fromMaybe (zeros 4 4) $ fromArray 4 4
  [ 1.0 , 0.0      , 0.0       , 0.0
  , 0.0 , costeta  , -sinteta  , 0.0
  , 0.0 , sinteta  , costeta   , 0.0
  , 0.0 , 0.0      , 0.0       , 1.0
  ]
  where
    costeta = cos (deg * pi / 180.0)
    sinteta = sin (deg * pi / 180.0)

rotateY :: Number -> Matrix Number
rotateY deg = fromMaybe (zeros 4 4) $ fromArray 4 4
  [ costeta  , 0.0 , sinteta , 0.0
  , 0.0      , 1.0 , 0.0     , 0.0
  , -sinteta , 0.0 , costeta , 0.0
  , 0.0      , 0.0 , 0.0     , 1.0
  ]
  where
    costeta = cos (deg * pi / 180.0)
    sinteta = sin (deg * pi / 180.0)

rotateZ :: Number -> Matrix Number
rotateZ deg = fromMaybe (zeros 4 4) $ fromArray 4 4
  [ costeta , -sinteta , 0.0 , 0.0
  , sinteta , costeta  , 0.0 , 0.0
  , 0.0     , 0.0      , 1.0 , 0.0
  , 0.0     , 0.0      , 0.0 , 1.0
  ]
  where
    costeta = cos (deg * pi / 180.0)
    sinteta = sin (deg * pi / 180.0)
