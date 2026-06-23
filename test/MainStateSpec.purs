module Test.MainStateSpec where

import Prelude

import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Console (log)
import Camera as Camera
import Main (applyAntibonding, applyDragStrength, applySubshellView, applyValenceOnly, applyZoomSet, initialState)
import Test.Util (approxEq, check)

mainStateSpec :: Effect Unit
mainStateSpec = do
  -- ───── Builder valence-only toggle (valence-only M2) ────────────────
  log "builder valence-only toggle properties:"

  -- true flips the field from its false init.
  check "applyValenceOnly true flips false→true" $
    (applyValenceOnly true initialState).valenceOnly == true
  -- A double-toggle returns to the original value.
  check "applyValenceOnly true twice returns to false" $
    (applyValenceOnly true (applyValenceOnly true initialState)).valenceOnly == false
  -- false is identity on the field.
  check "applyValenceOnly false leaves valenceOnly = false" $
    (applyValenceOnly false initialState).valenceOnly == false
  -- false is a no-op: other fields (e.g. view2D) are untouched.
  check "applyValenceOnly false leaves view2D unchanged" $
    (applyValenceOnly false initialState).view2D == initialState.view2D

  log "all builder valence-only toggle properties hold."

  -- ───── Atomos sub-shell view toggle (atomos shell/sub-shell toggle M2) ─────
  log "atomos sub-shell view toggle properties:"

  -- Default: subshellView starts as true (sub-shell view is on by default).
  check "initialState.subshellView == true (default is sub-shell view)" $
    initialState.subshellView == true

  -- true flips the field from its true init (true → false).
  check "applySubshellView true flips true→false" $
    (applySubshellView true initialState).subshellView == false

  -- A double-toggle returns to the original value (false → true via not).
  check "applySubshellView true twice returns to true" $
    (applySubshellView true (applySubshellView true initialState)).subshellView == true

  -- false is identity on the field (unchanged from the default true).
  check "applySubshellView false leaves subshellView = true" $
    (applySubshellView false initialState).subshellView == true

  -- false is a no-op: other fields (e.g. view2D) are untouched.
  check "applySubshellView false leaves view2D unchanged" $
    (applySubshellView false initialState).view2D == initialState.view2D

  log "all atomos sub-shell view toggle properties hold."

  -- ───── M2: applyDragStrength + dragStrength state field ──────────────
  log "M2 drag-strength wiring (applyDragStrength / State.dragStrength) properties:"

  -- Default: dragStrength starts at 3.0.
  check "initialState.dragStrength == 3.0" $
    initialState.dragStrength == 3.0

  -- Nothing is identity on all fields.
  check "applyDragStrength Nothing leaves dragStrength unchanged" $
    (applyDragStrength Nothing initialState).dragStrength == initialState.dragStrength

  -- Just d updates dragStrength to d.
  check "applyDragStrength (Just 5.0) sets dragStrength = 5.0" $
    (applyDragStrength (Just 5.0) initialState).dragStrength == 5.0
  check "applyDragStrength (Just 0.0) sets dragStrength = 0.0" $
    (applyDragStrength (Just 0.0) initialState).dragStrength == 0.0

  -- Nothing does not mutate other fields (e.g. view2D).
  check "applyDragStrength Nothing leaves view2D unchanged" $
    (applyDragStrength Nothing initialState).view2D == initialState.view2D

  -- Just d does not mutate other fields (e.g. zoom).
  check "applyDragStrength (Just 7.0) leaves zoom unchanged" $
    (applyDragStrength (Just 7.0) initialState).zoom == initialState.zoom

  log "all M2 drag-strength wiring properties hold."

  -- ───── M3-S2: antibonding toggle (Builder-only render flag) ────────────
  log "M3-S2 antibonding toggle properties:"

  -- Default: antibonding starts as false.
  check "initialState.antibonding == false" $
    initialState.antibonding == false

  -- true flips the field from its false init.
  check "applyAntibonding true flips false→true" $
    (applyAntibonding true initialState).antibonding == true

  -- A double-toggle returns to the original value.
  check "applyAntibonding true twice returns to false" $
    (applyAntibonding true (applyAntibonding true initialState)).antibonding == false

  -- false is identity on the field.
  check "applyAntibonding false leaves antibonding = false" $
    (applyAntibonding false initialState).antibonding == false

  -- false is a no-op: other fields (e.g. valenceOnly) are untouched.
  check "applyAntibonding false leaves valenceOnly unchanged" $
    (applyAntibonding false initialState).valenceOnly == initialState.valenceOnly

  -- true does not mutate other fields (e.g. zoom).
  check "applyAntibonding true leaves zoom unchanged" $
    (applyAntibonding true initialState).zoom == initialState.zoom

  -- true does not mutate scene field.
  check "applyAntibonding true leaves scene unchanged" $
    (applyAntibonding true initialState).scene == initialState.scene

  -- true does not mutate valenceOnly.
  check "applyAntibonding true leaves valenceOnly unchanged" $
    (applyAntibonding true initialState).valenceOnly == initialState.valenceOnly

  log "all M3-S2 antibonding toggle properties hold."

  -- ───── M2-zoom-slider: applyZoomSet fold ─────────────────────────────
  log "M2-zoom-slider applyZoomSet fold properties:"

  -- Default: zoom starts at 1.0.
  check "initialState.zoom == 1.0 (default)" $
    initialState.zoom == 1.0

  -- Nothing is identity on the zoom field.
  check "applyZoomSet Nothing leaves zoom unchanged" $
    (applyZoomSet Nothing initialState).zoom == initialState.zoom

  -- Nothing does not mutate other fields (e.g. view2D).
  check "applyZoomSet Nothing leaves view2D unchanged" $
    (applyZoomSet Nothing initialState).view2D == initialState.view2D

  -- Just v with a mid-range value sets zoom to that value.
  check "applyZoomSet (Just 2.0) sets zoom = 2.0" $
    approxEq (applyZoomSet (Just 2.0) initialState).zoom 2.0

  check "applyZoomSet (Just 0.5) sets zoom = 0.5" $
    approxEq (applyZoomSet (Just 0.5) initialState).zoom 0.5

  -- Clamp: below minZoom clamps to minZoom.
  check "applyZoomSet (Just 0.0) clamps to Camera.minZoom" $
    approxEq (applyZoomSet (Just 0.0) initialState).zoom Camera.minZoom

  check "applyZoomSet (Just -1.0) clamps to Camera.minZoom" $
    approxEq (applyZoomSet (Just (-1.0)) initialState).zoom Camera.minZoom

  -- Clamp: above maxZoom clamps to maxZoom.
  check "applyZoomSet (Just 999.0) clamps to Camera.maxZoom" $
    approxEq (applyZoomSet (Just 999.0) initialState).zoom Camera.maxZoom

  check "applyZoomSet (Just 5.1) clamps to Camera.maxZoom (5.0)" $
    approxEq (applyZoomSet (Just 5.1) initialState).zoom Camera.maxZoom

  -- Boundary: minZoom and maxZoom pass through unchanged.
  check "applyZoomSet (Just minZoom) sets zoom = minZoom exactly" $
    approxEq (applyZoomSet (Just Camera.minZoom) initialState).zoom Camera.minZoom

  check "applyZoomSet (Just maxZoom) sets zoom = maxZoom exactly" $
    approxEq (applyZoomSet (Just Camera.maxZoom) initialState).zoom Camera.maxZoom

  -- Just v does not mutate other fields (e.g. dragStrength, valenceOnly).
  check "applyZoomSet (Just 3.0) leaves dragStrength unchanged" $
    (applyZoomSet (Just 3.0) initialState).dragStrength == initialState.dragStrength

  check "applyZoomSet (Just 3.0) leaves valenceOnly unchanged" $
    (applyZoomSet (Just 3.0) initialState).valenceOnly == initialState.valenceOnly

  -- applyZoomSet is independent of applyZoom: setting then delta-zooming compounds.
  -- (Not a fold test — just verifying the set and the wheel delta work independently.)
  check "applyZoomSet sets zoom absolutely, not delta-relative" $
    approxEq (applyZoomSet (Just 1.0) (applyZoomSet (Just 3.0) initialState)).zoom 1.0

  log "all M2-zoom-slider applyZoomSet fold properties hold."
