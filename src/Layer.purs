-- | Pure "scale ladder" for the zoom-to-traverse-scales mechanic. The viewer
-- | sits on one `ScaleLayer` (sub-atomic → atomic, micro → macro); zooming all
-- | the way OUT steps UP a layer (toward macro), zooming all the way IN steps
-- | DOWN a layer (toward micro). Each crossing resets the zoom to a neutral
-- | mid value so the next layer starts framed.
-- |
-- | Pure / total / deterministic — imports only Prelude + Data.Maybe (no
-- | Effect / WebGL / Math.Matrix).
module Layer
  ( ScaleLayer(..)
  , allLayers
  , layerUp
  , layerDown
  , layerName
  , zoomOutThreshold
  , zoomInThreshold
  , layerResetZoom
  , applyLayerZoom
  ) where

import Prelude

import Data.Maybe (Maybe(..))

-- The scale ladder, ordered micro → macro: SubAtomic is the BOTTOM rung,
-- Atomic the TOP rung.
data ScaleLayer
  = SubAtomic
  | Atomic

derive instance eqScaleLayer :: Eq ScaleLayer

instance showScaleLayer :: Show ScaleLayer where
  show SubAtomic = "SubAtomic"
  show Atomic = "Atomic"

-- All layers, ordered micro → macro.
allLayers :: Array ScaleLayer
allLayers = [ SubAtomic, Atomic ]

-- Step toward macro (zooming out). The top rung has nowhere further to go.
layerUp :: ScaleLayer -> Maybe ScaleLayer
layerUp SubAtomic = Just Atomic
layerUp Atomic = Nothing

-- Step toward micro (zooming in). The bottom rung has nowhere further to go.
layerDown :: ScaleLayer -> Maybe ScaleLayer
layerDown Atomic = Just SubAtomic
layerDown SubAtomic = Nothing

-- Human-readable layer name for the overlay banner.
layerName :: ScaleLayer -> String
layerName SubAtomic = "Sub-atomic layer"
layerName Atomic = "Atomic layer"

-- Zoom thresholds and the neutral reset zoom. These MUST bracket inside the
-- Camera zoom bounds and order as:
--   Camera.minZoom (0.2) < zoomOutThreshold < layerResetZoom < zoomInThreshold < Camera.maxZoom (5.0)
zoomOutThreshold :: Number
zoomOutThreshold = 0.3

zoomInThreshold :: Number
zoomInThreshold = 3.0

layerResetZoom :: Number
layerResetZoom = 1.0

-- | Apply a layer crossing for the current zoom. Zooming OUT past
-- | `zoomOutThreshold` steps UP a layer (resetting zoom); zooming IN past
-- | `zoomInThreshold` steps DOWN a layer (also resetting zoom). At a ladder
-- | edge (no further layer) both the layer and the zoom are left untouched.
-- | Mid-range zoom passes through unchanged.
applyLayerZoom :: ScaleLayer -> Number -> { layer :: ScaleLayer, zoom :: Number }
applyLayerZoom layer zoom
  | zoom <= zoomOutThreshold = case layerUp layer of
      Just up -> { layer: up, zoom: layerResetZoom }
      Nothing -> { layer, zoom }
  | zoom >= zoomInThreshold = case layerDown layer of
      Just dn -> { layer: dn, zoom: layerResetZoom }
      Nothing -> { layer, zoom }
  | otherwise = { layer, zoom }
