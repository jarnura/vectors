module Math.Matrix
  ( Matrix
  , Vector
  , fromArray
  , fromColumn
  , toVector
  , zeros
  , identity
  , translate
  , scale
  , shear
  , add
  , multiply
  , mulScalar
  , transpose
  ) where

import Prelude

import Data.Array ((!!), length, replicate, range, zipWith)
import Data.Foldable (sum)
import Data.Maybe (Maybe(..), fromMaybe)

type Vector a = Array a

newtype Matrix a = Matrix { rows :: Int, cols :: Int, entries :: Array a }

mkMatrix :: forall a. Int -> Int -> Array a -> Matrix a
mkMatrix r c xs = Matrix { rows: r, cols: c, entries: xs }

fromArray :: forall a. Int -> Int -> Array a -> Maybe (Matrix a)
fromArray r c xs = if length xs == r * c then Just (mkMatrix r c xs) else Nothing

fromColumn :: forall a. Vector a -> Matrix a
fromColumn xs = mkMatrix (length xs) 1 xs

toVector :: forall a. Matrix a -> Array a
toVector (Matrix m) = m.entries

zeros :: Int -> Int -> Matrix Number
zeros r c = mkMatrix r c (replicate (r * c) 0.0)

-- 4x4 identity matrix.
identity :: Matrix Number
identity = mkMatrix 4 4
  [ 1.0
  , 0.0
  , 0.0
  , 0.0
  , 0.0
  , 1.0
  , 0.0
  , 0.0
  , 0.0
  , 0.0
  , 1.0
  , 0.0
  , 0.0
  , 0.0
  , 0.0
  , 1.0
  ]

-- 4x4 translation matrix: shifts a point by (tx, ty, tz).
translate :: Number -> Number -> Number -> Matrix Number
translate tx ty tz = mkMatrix 4 4
  [ 1.0
  , 0.0
  , 0.0
  , tx
  , 0.0
  , 1.0
  , 0.0
  , ty
  , 0.0
  , 0.0
  , 1.0
  , tz
  , 0.0
  , 0.0
  , 0.0
  , 1.0
  ]

-- 4x4 non-uniform scale matrix.
scale :: Number -> Number -> Number -> Matrix Number
scale sx sy sz = mkMatrix 4 4
  [ sx
  , 0.0
  , 0.0
  , 0.0
  , 0.0
  , sy
  , 0.0
  , 0.0
  , 0.0
  , 0.0
  , sz
  , 0.0
  , 0.0
  , 0.0
  , 0.0
  , 1.0
  ]

-- 4x4 shear matrix: x' = x + k*y (shears the Y axis into X). shear 0 = identity.
shear :: Number -> Matrix Number
shear k = mkMatrix 4 4
  [ 1.0
  , k
  , 0.0
  , 0.0
  , 0.0
  , 1.0
  , 0.0
  , 0.0
  , 0.0
  , 0.0
  , 1.0
  , 0.0
  , 0.0
  , 0.0
  , 0.0
  , 1.0
  ]

at :: Matrix Number -> Int -> Int -> Number
at (Matrix m) i j = fromMaybe 0.0 (m.entries !! (i * m.cols + j))

add :: Matrix Number -> Matrix Number -> Matrix Number
add (Matrix a) (Matrix b) =
  Matrix { rows: a.rows, cols: a.cols, entries: zipWith (+) a.entries b.entries }

multiply :: Matrix Number -> Matrix Number -> Matrix Number
multiply ma@(Matrix a) mb@(Matrix b) =
  Matrix
    { rows: a.rows
    , cols: b.cols
    , entries: do
        i <- range 0 (a.rows - 1)
        j <- range 0 (b.cols - 1)
        pure (sum (map (\k -> at ma i k * at mb k j) (range 0 (a.cols - 1))))
    }

mulScalar :: Number -> Vector Number -> Vector Number
mulScalar s = map (s * _)

-- | Transpose a 4x4 row-major matrix: entry (i,j) of the output equals entry
-- | (j,i) of the input. Built via toVector/fromArray; for an orthogonal
-- | rotation matrix this is its inverse.
transpose :: Matrix Number -> Matrix Number
transpose m =
  let
    v = toVector m
    el i j = fromMaybe 0.0 (v !! (i * 4 + j))
  in
    fromMaybe (zeros 4 4) $ fromArray 4 4 do
      i <- range 0 3
      j <- range 0 3
      pure (el j i)
