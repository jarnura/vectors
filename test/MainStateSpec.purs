module Test.MainStateSpec where

import Prelude

import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Console (log)
import Main (applyDragStrength, applySubshellView, applyValenceOnly, initialState)
import Test.Util (check)

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
