module Utils where

import VectorPrelude

import LinearAlgebra.Matrix (toVector) as M

foreign import logAny          :: ∀ a. a → a
foreign import storeMatrix     :: Array (Matrix Number) → (Array (Matrix Number))
foreign import getCachedMatrix :: Unit → (Array (Matrix Number))
foreign import getX            :: Unit → Number
foreign import getY            :: Unit → Number
foreign import storeX          :: Number → Number
foreign import storeY          :: Number → Number
foreign import getrotX         :: Unit → Number
foreign import getrotY         :: Unit → Number
foreign import storerotX       :: Number → Number
foreign import storerotY       :: Number → Number
foreign import incSpeed        :: Unit → Number
foreign import decSpeed        :: Unit → Number
foreign import getSpeed        :: Unit → Number
foreign import off             :: Unit → Unit

vectorToPoint :: Matrix Number -> Point
vectorToPoint = M.toVector >>> (\arr -> { x: fromMaybe zero (arr !! 0), y: fromMaybe zero (arr !! 1)})

