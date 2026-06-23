-- The `window.__nuclear` test/automation API for the Nuclide scene. It owns a
-- single mutable `Ref NuclearState` that BOTH this JS-facing API and the render
-- loop read, so a mutation through the API (setNuclide/addProton/etc.) is
-- reflected immediately and synchronously.
--
-- `NuclearState` holds the current `Nuclide` (Z + N), the Q-value of the last
-- reaction, and the last fission result (fragments + neutrons). The `nuclear`
-- field in `State` is mirrored from this Ref each frame (like `State.builder`).
--
-- M3 adds named reaction ops: decayAlpha, decayBetaMinus, decayBetaPlus, fuseWith,
-- fission, lastQ. Each updates the current nuclide and stores lastQ.
--
-- This module is the only seam between the pure `Nuclear` model and the DOM
-- global `window.__nuclear`. The FFI (`NuclearApi.js`) is pure DOM/JS — it
-- never touches WebGL.
module NuclearApi
  ( installNuclearApi
  ) where

import Prelude

import Effect (Effect)
import Effect.Ref as Ref
import Data.Int (toNumber) as I
import Data.Maybe (Maybe(..))
import Nuclear
  ( NuclearState
  , Mode(..)
  , addNeutron
  , addProton
  , massNumber
  , measuredBinding
  , decayMode
  , defaultNeutrons
  , nuclearSymbol
  , removeNeutron
  , removeProton
  , applyAlphaDecay
  , applyBetaMinus
  , applyBetaPlusEC
  , applyFuse
  , applyFission
  )

-- Default seed: Carbon-12 (Z=6, N=6). Well-known stable nuclide.
defaultNuclearState :: NuclearState
defaultNuclearState =
  { nuclide: { z: 6, n: defaultNeutrons 6 }
  , lastQ: 0.0
  , lastFission: Nothing
  }

-- JS-facing shape for getNuclide().
type JsNuclide = { z :: Int, n :: Int, a :: Int, symbol :: String }

-- JS-facing shape for getBinding().
type JsBinding = { total :: Number, perNucleon :: Number }

-- JS-facing shape for getStability().
type JsStability = { mode :: String, stable :: Boolean }

-- JS-facing shape for fission result.
type JsFissionResult =
  { aZ :: Int, aA :: Int, aSymbol :: String
  , bZ :: Int, bA :: Int, bSymbol :: String
  , neutrons :: Int
  }

-- The bridge handed to the FFI.
type Bridge =
  { setNuclide :: Int -> Int -> Effect Unit
  , addProton :: Effect Unit
  , removeProton :: Effect Unit
  , addNeutron :: Effect Unit
  , removeNeutron :: Effect Unit
  , getNuclide :: Effect JsNuclide
  , getBinding :: Effect JsBinding
  , getStability :: Effect JsStability
  -- M3 reaction ops.
  , decayAlpha :: Effect Unit
  , decayBetaMinus :: Effect Unit
  , decayBetaPlus :: Effect Unit
  , fuseWith :: Int -> Int -> Effect Unit
  , fissionReact :: Int -> Int -> Int -> Int -> Effect Unit
  , lastQ :: Effect Number
  , lastFission :: Effect (Maybe JsFissionResult)
  }

-- Render the Mode as a plain String for JS consumers.
modeString :: Mode -> String
modeString Stable     = "Stable"
modeString BetaMinus  = "BetaMinus"
modeString BetaPlusEC = "BetaPlusEC"
modeString Alpha      = "Alpha"
modeString Unbound    = "Unbound"

-- Install `window.__nuclear` and return the shared Ref so the render loop can
-- read the live nuclear state each frame. After every mutation the `onChange`
-- callback fires with the new state, letting the caller eagerly re-render.
installNuclearApi
  :: (NuclearState -> Effect Unit)
  -> Effect (Ref.Ref NuclearState)
installNuclearApi onChange = do
  ref <- Ref.new defaultNuclearState
  let
    -- Apply a pure update to the shared Ref, then notify the renderer.
    mutate :: (NuclearState -> NuclearState) -> Effect Unit
    mutate f = do
      Ref.modify_ f ref
      st <- Ref.read ref
      onChange st

    bridge :: Bridge
    bridge =
      { setNuclide: \z n ->
          mutate (\_ -> defaultNuclearState { nuclide = { z, n } })
      , addProton:
          mutate (\st -> st { nuclide = addProton st.nuclide })
      , removeProton:
          mutate (\st -> st { nuclide = removeProton st.nuclide })
      , addNeutron:
          mutate (\st -> st { nuclide = addNeutron st.nuclide })
      , removeNeutron:
          mutate (\st -> st { nuclide = removeNeutron st.nuclide })
      , getNuclide: do
          st <- Ref.read ref
          let nuc = st.nuclide
          pure
            { z: nuc.z
            , n: nuc.n
            , a: massNumber nuc
            , symbol: nuclearSymbol nuc.z
            }
      , getBinding: do
          st <- Ref.read ref
          let nuc = st.nuclide
          let a = massNumber nuc
          let b = measuredBinding nuc
          pure
            { total: b
            , perNucleon: if a < 1 then 0.0 else b / I.toNumber a
            }
      , getStability: do
          st <- Ref.read ref
          let nuc = st.nuclide
          let mode = decayMode nuc
          pure
            { mode: modeString mode
            , stable: mode == Stable
            }
      -- ── M3 reaction ops — delegate to shared pure helpers in Nuclear ─────
      , decayAlpha:    mutate applyAlphaDecay
      , decayBetaMinus: mutate applyBetaMinus
      , decayBetaPlus:  mutate applyBetaPlusEC
      , fuseWith: \z2 n2 -> mutate (applyFuse z2 n2)
      , fissionReact: \zA nA zB nB -> mutate (applyFission zA nA zB nB)
      , lastQ: do
          st <- Ref.read ref
          pure st.lastQ
      , lastFission: do
          st <- Ref.read ref
          case st.lastFission of
            Nothing -> pure Nothing
            Just f ->
              pure $ Just
                { aZ: f.a.z
                , aA: massNumber f.a
                , aSymbol: nuclearSymbol f.a.z
                , bZ: f.b.z
                , bA: massNumber f.b
                , bSymbol: nuclearSymbol f.b.z
                , neutrons: f.neutrons
                }
      }
  installWindowNuclear bridge
  pure ref

-- DOM-only FFI: stash the bridge's effectful closures on `window.__nuclear`.
foreign import installWindowNuclear :: Bridge -> Effect Unit
