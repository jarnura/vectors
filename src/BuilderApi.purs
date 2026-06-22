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

import Builder (BuilderState, addAtom, clear, emptyBuilder, formulaOf, molecules, moveAtom, moveAtomWith, moveMolecule, spawnPos)
import Data.Array (length)
import Effect (Effect)
import Effect.Ref as Ref
import FRP.Loop (installAddButton, installClearButton)
import Lattice as Lattice

-- One bond rendered as a plain JS object the test reads as an array element.
-- order is exposed so E2E consumers can read the bond multiplicity; existing
-- readers that only inspect a/b are unaffected (extra field is ignored in JS).
type JsBond = { a :: Int, b :: Int, order :: Int }

-- One molecule (connected component) rendered as a JS object carrying both the
-- atom ids and the Unicode formula, so the E2E can adapt to either shape.
type JsMolecule = { ids :: Array Int, formula :: String }

-- One placed atom rendered as a plain JS object the test reads as an array
-- element: its id, atomic number, and {x,y,z} model position.
type JsAtom = { id :: Int, z :: Int, pos :: { x :: Number, y :: Number, z :: Number } }

-- The bridge handed to the FFI: synchronous mutators + readers over a shared
-- builder Ref. addAtom/moveAtom/clear/loadStructure mutate the Ref in place
-- (immutably replacing the held record); getBonds/getMolecules/getAtoms read
-- the current snapshot. loadStructure(i) replaces the entire BuilderState with
-- (Lattice.structureOf i).build and fires onChange, so the Materials scene
-- default-loads Diamond (index 0) on entry.
type Bridge =
  { addAtom :: Int -> Number -> Number -> Number -> Effect Unit
  , moveAtom :: Int -> Number -> Number -> Number -> Effect Unit
  , moveAtomWith :: Number -> Int -> Number -> Number -> Number -> Effect Unit
  , moveMolecule :: Int -> Number -> Number -> Number -> Effect Unit
  , clear :: Effect Unit
  , loadStructure :: Int -> Effect Unit
  , getBonds :: Effect (Array JsBond)
  , getMolecules :: Effect (Array JsMolecule)
  , getAtoms :: Effect (Array JsAtom)
  }

-- Installs `window.__builder` and returns the shared Ref so the render loop can
-- read the live builder state each frame. The same Ref backs both the API
-- mutations and the renderer. After every mutation the `onChange` callback fires
-- with the new state, letting the caller eagerly re-render the scene so the
-- change is on-screen immediately (deterministic for pixel-read E2E).
-- The `onSelectStructure` callback is invoked whenever `loadStructure(i)` is
-- called (both through the card click path and through the seam), so the card
-- highlight and the #materials-info panel stay in sync regardless of which path
-- triggered the load. Passing `mempty` disables the extra notification.
installBuilderApi :: (BuilderState -> Effect Unit) -> (Int -> Effect Unit) -> Effect (Ref.Ref BuilderState)
installBuilderApi onChange onSelectStructure = do
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
      , moveAtomWith: \strength aid x y z3 ->
          mutate (moveAtomWith strength aid { x, y, z: z3 })
      , moveMolecule: \aid x y z3 ->
          mutate (moveMolecule aid { x, y, z: z3 })
      , clear:
          mutate clear
      -- Replace the entire BuilderState with a curated crystal structure
      -- (Lattice.structureOf i).build. Fires onChange (eager re-render) and
      -- onSelectStructure (card highlight + #materials-info panel update) so
      -- both the rendering and the UI overlays stay in sync regardless of
      -- whether the call came from the card click path or the seam. Clamp-safe:
      -- out-of-range indices are clamped by Lattice.structureOf. DOM only.
      , loadStructure: \i -> do
          let newState = (Lattice.structureOf i).build
          Ref.write newState ref
          onChange newState
          onSelectStructure i
      , getBonds: do
          st <- Ref.read ref
          pure (map (\bd -> { a: bd.a, b: bd.b, order: bd.order }) st.bonds)
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

-- DOM-only FFI: stash the bridge's effectful closures on `window.__builder` as
-- plain JS functions (each PureScript `Effect` is a thunk the FFI runs).
foreign import installWindowBuilder :: Bridge -> Effect Unit
