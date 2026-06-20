module Test.Util where

import Prelude

import Data.Array (all, filter, index, mapWithIndex, zipWith)
import Data.Foldable (foldl)
import Data.Maybe (fromMaybe)
import Data.Number (abs, pi, tan)
import Effect (Effect)
import Effect.Console (log)
import Effect.Exception (throw)
import Math.Matrix as M
import Camera as Cam

-- Tolerance for floating-point matrix equality.
-- sin/cos roundtrip at 360° produces error on the order of 1e-15;
-- 1e-10 leaves comfortable headroom for chained multiplications.
epsilon :: Number
epsilon = 1.0e-10

approxEq :: Number -> Number -> Boolean
approxEq a b = abs (a - b) < epsilon

approxEqMatrix :: M.Matrix Number -> M.Matrix Number -> Boolean
approxEqMatrix m1 m2 =
  let
    v1 = M.toVector m1
    v2 = M.toVector m2
  in
    all identity (zipWith approxEq v1 v2)

identity4 :: M.Matrix Number
identity4 = fromMaybe (M.zeros 4 4) $ M.fromArray 4 4
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

check :: String -> Boolean -> Effect Unit
check name cond =
  if cond then log $ "  ok  " <> name
  else throw $ "FAIL " <> name

-- Fold applyZoomStep repeatedly with a fixed wheel delta, starting from `start`.
-- Used to assert repeated stepping stays clamped within [minZoom, maxZoom].
foldZoom :: Number -> Number -> Array Int -> Number
foldZoom delta start = foldl (\z _ -> Cam.applyZoomStep z delta) start

-- A pure perspective×camera projection for the pick/unproject round-trip test.
-- Mirrors the shape of Main.perspectiveProjection (perspective matrix composed
-- with a translation by -cameraDistance along -Z), built from Math.Matrix so the
-- test stays free of WebGL/Effect dependencies. fov = pi/3, near 1, far 2000,
-- camera 1000 back — the same constants the app uses.
testProjection :: Number -> Number -> M.Matrix Number
testProjection w h =
  let
    fov = pi / 3.0
    near = 1.0
    far = 2000.0
    cameraDistance = 1000.0
    aspect = w / h
    f = 1.0 / tan (fov / 2.0)
    p = fromMaybe (M.zeros 4 4) $ M.fromArray 4 4
      [ f / aspect
      , 0.0
      , 0.0
      , 0.0
      , 0.0
      , f
      , 0.0
      , 0.0
      , 0.0
      , 0.0
      , (far + near) / (near - far)
      , (2.0 * far * near) / (near - far)
      , 0.0
      , 0.0
      , -1.0
      , 0.0
      ]
  in
    M.multiply p (M.translate 0.0 0.0 (-cameraDistance))

-- Extract every Nth element starting at `start` (used to pluck x/y/z columns).
everyNth :: Int -> Int -> Array Number -> Array Number
everyNth step start xs =
  map (\t -> t.v)
    $ filter (\t -> (t.i - start) `mod` step == 0 && t.i >= start)
    $ mapWithIndex (\i v -> { i, v }) xs

-- Read a vec3 at vertex index `vi` (positions are packed [x,y,z, x,y,z, ...]).
nth :: Array Number -> Int -> { x :: Number, y :: Number, z :: Number }
nth xs vi =
  { x: at (vi * 3)
  , y: at (vi * 3 + 1)
  , z: at (vi * 3 + 2)
  }
  where
  at i = fromMaybe 0.0 (index xs i)
