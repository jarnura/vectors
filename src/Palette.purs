-- Shell / sub-shell colour palette for the atomos orbital rings and electrons.
-- Each principal shell n has a distinct base colour; within a shell, successive
-- sub-shells (s < p < d < f, by azimuthal ℓ) share that colour but are lighter
-- (blended toward white). Pure — only depends on the quantum numbers.
module Palette
  ( shellColor
  , subshellColor
  ) where

import Prelude

import Data.Int (toNumber)
import Graphics.GL (Color)

-- Distinct base colour per principal shell n (K, L, M, N, O, …).
shellColor :: Int -> Color
shellColor n = case n of
  1 -> { r: 0.92, g: 0.30, b: 0.30, a: 1.0 } -- K: red
  2 -> { r: 0.95, g: 0.62, b: 0.25, a: 1.0 } -- L: amber
  3 -> { r: 0.40, g: 0.80, b: 0.45, a: 1.0 } -- M: green
  4 -> { r: 0.40, g: 0.62, b: 1.00, a: 1.0 } -- N: blue
  5 -> { r: 0.75, g: 0.50, b: 0.95, a: 1.0 } -- O: purple
  _ -> { r: 0.70, g: 0.78, b: 0.85, a: 1.0 } -- fallback: pale steel

-- How much lighter each successive sub-shell is (blend fraction toward white per ℓ).
lightStep :: Number
lightStep = 0.22

-- The colour of sub-shell (n, ℓ): the shell base blended toward white by
-- ℓ·lightStep. ℓ=0 is exactly the shell colour; higher ℓ is the same hue, lighter.
subshellColor :: Int -> Int -> Color
subshellColor n l =
  let
    base = shellColor n
    t = min 1.0 (toNumber l * lightStep)
    lighten c = c + (1.0 - c) * t
  in
    { r: lighten base.r, g: lighten base.g, b: lighten base.b, a: base.a }
