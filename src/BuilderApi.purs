-- The `window.__builder` test/automation API for the Builder scene. It owns a
-- single mutable `Ref BuilderState` that BOTH this JS-facing API and the render
-- loop read, so a mutation through the API (addAtom/moveAtom/clear) is reflected
-- immediately and synchronously — getBonds()/getMolecules() read back the live
-- state right after a mutation, and the renderer picks up the same snapshot on
-- the next frame.
--
-- This module is the only seam between the pure `Builder` model and the DOM
-- global `window.__builder`. The FFI (`BuilderApi.js`) is pure DOM/JS — it never
-- touches WebGL.
module BuilderApi
  ( installBuilderApi
  , installBuilderControls
  ) where

import Prelude

import Builder (BuilderState, addAtom, bondThreshold, clear, emptyBuilder, formulaOf, molecules, moveAtom, moveMolecule)
import Data.Array (length)
import Data.Int (toNumber)
import Effect (Effect)
import Effect.Ref as Ref
import FRP.Loop (installAddButton, installClearButton)

-- One bond rendered as a plain JS object the test reads as an array element.
type JsBond = { a :: Int, b :: Int }

-- One molecule (connected component) rendered as a JS object carrying both the
-- atom ids and the Unicode formula, so the E2E can adapt to either shape.
type JsMolecule = { ids :: Array Int, formula :: String }

-- One placed atom rendered as a plain JS object the test reads as an array
-- element: its id, atomic number, and {x,y,z} model position.
type JsAtom = { id :: Int, z :: Int, pos :: { x :: Number, y :: Number, z :: Number } }

-- The bridge handed to the FFI: synchronous mutators + readers over a shared
-- builder Ref. addAtom/moveAtom/clear mutate the Ref in place (immutably
-- replacing the held record); getBonds/getMolecules read the current snapshot.
type Bridge =
  { addAtom :: Int -> Number -> Number -> Number -> Effect Unit
  , moveAtom :: Int -> Number -> Number -> Number -> Effect Unit
  , moveMolecule :: Int -> Number -> Number -> Number -> Effect Unit
  , clear :: Effect Unit
  , getBonds :: Effect (Array JsBond)
  , getMolecules :: Effect (Array JsMolecule)
  , getAtoms :: Effect (Array JsAtom)
  }

-- Installs `window.__builder` and returns the shared Ref so the render loop can
-- read the live builder state each frame. The same Ref backs both the API
-- mutations and the renderer. After every mutation the `onChange` callback fires
-- with the new state, letting the caller eagerly re-render the scene so the
-- change is on-screen immediately (deterministic for pixel-read E2E).
installBuilderApi :: (BuilderState -> Effect Unit) -> Effect (Ref.Ref BuilderState)
installBuilderApi onChange = do
  ref <- Ref.new emptyBuilder
  let
    -- Apply a pure update to the shared Ref, then notify the renderer.
    mutate :: (BuilderState -> BuilderState) -> Effect Unit
    mutate f = do
      Ref.modify_ f ref
      st <- Ref.read ref
      onChange st

    bridge :: Bridge
    bridge =
      { addAtom: \z x y z3 ->
          mutate (addAtom z { x, y, z: z3 })
      , moveAtom: \aid x y z3 ->
          mutate (moveAtom aid { x, y, z: z3 })
      , moveMolecule: \aid x y z3 ->
          mutate (moveMolecule aid { x, y, z: z3 })
      , clear:
          mutate clear
      , getBonds: do
          st <- Ref.read ref
          pure (map (\bd -> { a: bd.a, b: bd.b }) st.bonds)
      , getMolecules: do
          st <- Ref.read ref
          pure
            ( map
                (\comp -> { ids: comp, formula: formulaOf st comp })
                (molecules st)
            )
      , getAtoms: do
          st <- Ref.read ref
          pure (map (\a -> { id: a.id, z: a.z, pos: a.pos }) st.atoms)
      }
  installWindowBuilder bridge
  pure ref

-- Wire the in-app Add/Clear buttons (#add-btn / #clear-btn) to the SAME builder
-- Ref the API and renderer share. Add spawns the selected element Z at a spread
-- position so consecutive adds land within bondThreshold of one another and
-- auto-bond (demonstrating the model through the UI, not just the test API).
-- Clear empties the world. After each mutation the `onChange` callback fires with
-- the new state, so the in-app buttons eagerly re-render the scene exactly like
-- the window.__builder API path (one consistent mutate→render seam). Pure DOM
-- wiring (the buttons live in the controls panel) — no WebGL.
installBuilderControls :: (BuilderState -> Effect Unit) -> Ref.Ref BuilderState -> Effect Unit
installBuilderControls onChange ref = do
  let
    mutate f = do
      Ref.modify_ f ref
      st <- Ref.read ref
      onChange st
  installAddButton \z ->
    mutate (\st -> addAtom z (spawnPos (length st.atoms)) st)
  installClearButton
    (mutate clear)
  where
  -- Lay successive atoms along x near the origin, stepped by ~0.45·bondThreshold
  -- so each new atom sits within bonding range of the previous one (a chain that
  -- auto-bonds as it grows), centred about the origin so it stays on-screen.
  spawnPos i =
    { x: (toNumber i - 1.0) * (bondThreshold * 0.45), y: 0.0, z: 0.0 }

-- DOM-only FFI: stash the bridge's effectful closures on `window.__builder` as
-- plain JS functions (each PureScript `Effect` is a thunk the FFI runs).
foreign import installWindowBuilder :: Bridge -> Effect Unit
