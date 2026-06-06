-- A deterministic starfield for the atomos backdrop: points spread evenly on a
-- large shell via a Fibonacci-sphere distribution (no Math.random, so it is
-- stable across frames and runs).
module Starfield
  ( Star
  , starPositions
  ) where

import Prelude

import Data.Array (range)
import Data.Int (toNumber)
import Data.Number (cos, pi, sin, sqrt)

type Star = { x :: Number, y :: Number, z :: Number }

-- Number of stars in the backdrop.
starCount :: Int
starCount = 140

-- Shell radius — far out but inside the perspective far plane (2000).
starRadius :: Number
starRadius = 1500.0

starPositions :: Array Star
starPositions = map place (range 0 (starCount - 1))
  where
  -- Golden angle for the Fibonacci-sphere distribution.
  goldenAngle = pi * (3.0 - sqrt 5.0)
  place i =
    let
      fi = toNumber i
      y = 1.0 - 2.0 * (fi + 0.5) / toNumber starCount -- evenly in (-1, 1)
      r = sqrt (1.0 - y * y)
      theta = goldenAngle * fi
    in
      { x: starRadius * r * cos theta
      , y: starRadius * y
      , z: starRadius * r * sin theta
      }
