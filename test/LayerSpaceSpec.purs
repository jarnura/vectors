-- Unit tests for the layer-space feature (M1-layer-space):
--   1. State.layerSpace field + initialState default (1.6)
--   2. applyLayerSpace fold: Just sets/clamps, Nothing is identity
--   3. Scene.Entities scale parameterization:
--        builderWorldPos with layerSpace=1.0 equals the historic builderScale result;
--        builderWorldPos scales linearly with layerSpace.
module Test.LayerSpaceSpec where

import Prelude

import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Console (log)
import Main (applyLayerSpace, initialState)
import Scene.Entities (builderScale, builderWorldPosWith)
import Test.Util (approxEq, check)

layerSpaceSpec :: Effect Unit
layerSpaceSpec = do
  -- ───── State.layerSpace field + initialState default ─────────────────
  log "layer-space state field + initialState properties:"

  -- Default: layerSpace starts at 1.6 (visibly more spacious than the unit baseline).
  check "initialState.layerSpace == 1.6 (default)" $
    initialState.layerSpace == 1.6

  -- ───── applyLayerSpace fold ──────────────────────────────────────────
  log "applyLayerSpace fold properties:"

  -- Nothing is identity on the layerSpace field.
  check "applyLayerSpace Nothing leaves layerSpace unchanged" $
    (applyLayerSpace Nothing initialState).layerSpace == initialState.layerSpace

  -- Nothing does not mutate other fields (e.g. zoom).
  check "applyLayerSpace Nothing leaves zoom unchanged" $
    (applyLayerSpace Nothing initialState).zoom == initialState.zoom

  -- Just v sets layerSpace = v (within range).
  check "applyLayerSpace (Just 2.0) sets layerSpace = 2.0" $
    (applyLayerSpace (Just 2.0) initialState).layerSpace == 2.0

  check "applyLayerSpace (Just 1.0) sets layerSpace = 1.0 (min)" $
    (applyLayerSpace (Just 1.0) initialState).layerSpace == 1.0

  check "applyLayerSpace (Just 4.0) sets layerSpace = 4.0 (max)" $
    (applyLayerSpace (Just 4.0) initialState).layerSpace == 4.0

  -- Clamp: below 1.0 → clamped to 1.0.
  check "applyLayerSpace (Just 0.5) clamps to 1.0" $
    (applyLayerSpace (Just 0.5) initialState).layerSpace == 1.0

  -- Clamp: above 4.0 → clamped to 4.0.
  check "applyLayerSpace (Just 5.0) clamps to 4.0" $
    (applyLayerSpace (Just 5.0) initialState).layerSpace == 4.0

  -- Just d does not mutate other fields (e.g. dragStrength, valenceOnly).
  check "applyLayerSpace (Just 2.5) leaves dragStrength unchanged" $
    (applyLayerSpace (Just 2.5) initialState).dragStrength == initialState.dragStrength

  check "applyLayerSpace (Just 2.5) leaves valenceOnly unchanged" $
    (applyLayerSpace (Just 2.5) initialState).valenceOnly == initialState.valenceOnly

  check "applyLayerSpace (Just 2.5) leaves zoom unchanged" $
    (applyLayerSpace (Just 2.5) initialState).zoom == initialState.zoom

  -- ───── Scene.Entities scale parameterization ─────────────────────────
  log "builderWorldPosWith scale parameterization properties:"

  let
    testPos = { x: 100.0, y: 50.0, z: -30.0 }

  -- At layerSpace=1.0 the effective scale == builderScale, so
  -- builderWorldPosWith 1.0 p == the old builderWorldPos result.
  check "builderWorldPosWith 1.0 x matches builderScale*x" $
    approxEq (builderWorldPosWith 1.0 testPos).x (testPos.x * builderScale)

  check "builderWorldPosWith 1.0 y matches builderScale*y" $
    approxEq (builderWorldPosWith 1.0 testPos).y (testPos.y * builderScale)

  check "builderWorldPosWith 1.0 z matches builderScale*z" $
    approxEq (builderWorldPosWith 1.0 testPos).z (testPos.z * builderScale)

  -- Scale linearity: doubling layerSpace doubles the world position.
  check "builderWorldPosWith 2.0 x == 2 * builderWorldPosWith 1.0 x" $
    approxEq
      (builderWorldPosWith 2.0 testPos).x
      (2.0 * (builderWorldPosWith 1.0 testPos).x)

  check "builderWorldPosWith 4.0 z == 4 * builderWorldPosWith 1.0 z" $
    approxEq
      (builderWorldPosWith 4.0 testPos).z
      (4.0 * (builderWorldPosWith 1.0 testPos).z)

  -- builderWorldPosWith layerSpace grows with layerSpace (non-trivial
  -- position: x=100 so the product is non-zero).
  check "builderWorldPosWith 1.6 > builderWorldPosWith 1.0 for positive x" $
    (builderWorldPosWith 1.6 testPos).x > (builderWorldPosWith 1.0 testPos).x

  check "builderWorldPosWith 3.0 > builderWorldPosWith 1.6 for positive x" $
    (builderWorldPosWith 3.0 testPos).x > (builderWorldPosWith 1.6 testPos).x

  log "all layer-space properties hold."
