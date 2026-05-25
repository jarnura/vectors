module Utils where

import VectorPrelude

import LinearAlgebra.Matrix (toVector) as M

foreign import storeMatrix     :: Array (Matrix Number) → Effect Unit
foreign import getCachedMatrix :: Effect (Array (Matrix Number))
foreign import getX            :: Effect Number
foreign import getY            :: Effect Number
foreign import storeX          :: Number → Effect Unit
foreign import storeY          :: Number → Effect Unit
foreign import incSpeed        :: Effect Number

vectorToPoint :: Matrix Number -> Point
vectorToPoint = M.toVector >>> (\arr -> { x: fromMaybe zero (arr !! 0), y: fromMaybe zero (arr !! 1)})
