module FRP.Loop
  ( Input
  , emptyInput
  , runLoop
  , installAddButton
  , installClearButton
  , installCanvasPointer
  , installWheelListener
  , installOrbitButtons
  , installValenceOnlyToggle
  , installAntibondingToggle
  , installSubshellViewToggle
  , installDragStrengthSlider
  , installLayerSpaceSlider
  , installZoomSlider
  , installNuclearControls
  , installNuclearReactionControls
  , setBuilderDetail
  , setZoomSlider
  ) where

import Prelude

import Controls (installBondButton, runBondAnimation)
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Ref as Ref

type Input =
  { lastKey :: Maybe String
  , mouse :: Maybe { x :: Int, y :: Int }
  , shear :: Maybe Number
  , toggleScene :: Boolean
  , toggle2D :: Boolean
  , toggleValenceOnly :: Boolean
  , toggleAntibonding :: Boolean
  , toggleSubshellView :: Boolean
  , element :: Maybe Int
  , dragStrength :: Maybe Number
  , layerSpace :: Maybe Number
  , bondProgress :: Maybe Number
  , zoomDelta :: Maybe Number
  -- Absolute zoom set by the #zoom-slider (Nothing = no slider event this frame).
  , zoomSet :: Maybe Number
  }

emptyInput :: Input
emptyInput =
  { lastKey: Nothing
  , mouse: Nothing
  , shear: Nothing
  , toggleScene: false
  , toggle2D: false
  , toggleValenceOnly: false
  , toggleAntibonding: false
  , toggleSubshellView: false
  , element: Nothing
  , dragStrength: Nothing
  , layerSpace: Nothing
  , bondProgress: Nothing
  , zoomDelta: Nothing
  , zoomSet: Nothing
  }

foreign import installKeyUpListener
  :: (String -> Effect Unit) -> Effect Unit

foreign import installMouseMoveListener
  :: (Int -> Int -> Effect Unit) -> Effect Unit

-- Wires the shear button: on click, reads the shear-value input and invokes
-- the callback with the parsed number.
foreign import installShearButton
  :: (Number -> Effect Unit) -> Effect Unit

-- Wires the scene-switch button: runs the given effect on each click.
foreign import installSceneToggle
  :: Effect Unit -> Effect Unit

-- Wires the 2D-view checkbox: runs the given effect on each change.
foreign import installView2DToggle
  :: Effect Unit -> Effect Unit

-- Wires the "valence only" checkbox: runs the given effect on each change.
foreign import installValenceOnlyToggle
  :: Effect Unit -> Effect Unit

-- Wires the "antibonding" checkbox: runs the given effect on each change.
-- Mirrors installValenceOnlyToggle. Builder/Materials render flag.
foreign import installAntibondingToggle
  :: Effect Unit -> Effect Unit

-- Wires the "sub-shells" view checkbox: runs the given effect on each change.
foreign import installSubshellViewToggle
  :: Effect Unit -> Effect Unit

-- Wires the element selector: on change, invokes the callback with the chosen
-- atomic number.
foreign import installElementInput
  :: (Int -> Effect Unit) -> Effect Unit

-- Wires the drag-strength slider (#drag-strength): on input/change, invokes
-- the callback with the parsed slider value (the live drag strength).
foreign import installDragStrengthSlider
  :: (Number -> Effect Unit) -> Effect Unit

-- Wires the layer-space slider (#layer-space): on input/change, invokes the
-- callback with the parsed slider value (the live layer-space multiplier).
-- Mirrors installDragStrengthSlider in shape.
foreign import installLayerSpaceSlider
  :: (Number -> Effect Unit) -> Effect Unit

-- Wires the builder Add button (#add-btn): on click, reads the element selector
-- (#element-value) and invokes the callback with the chosen atomic number.
foreign import installAddButton
  :: (Int -> Effect Unit) -> Effect Unit

-- Wires the builder Clear button (#clear-btn): runs the given effect on click.
foreign import installClearButton
  :: Effect Unit -> Effect Unit

-- Wires pointer (mouse) down/move/up listeners over the canvas element
-- (#canvas). Each callback receives canvas-LOCAL coordinates in backing-store
-- pixels (the event clientX/Y minus the canvas bounding rect, scaled by the
-- canvas backing width/height vs its CSS size). The down callback also receives
-- the native mouse click count (event.detail) — 1 for a single click, ≥2 for the
-- 2nd mousedown of a double-click — so callers can distinguish single- vs
-- double-click drags. DOM-only input plumbing — reads canvas geometry but never
-- touches WebGL.
foreign import installCanvasPointer
  :: (Number -> Number -> Int -> Effect Unit)
  -> (Number -> Number -> Effect Unit)
  -> Effect Unit
  -> Effect Unit

-- Wires a 'wheel' listener on the canvas (#canvas), passive:false so it can
-- preventDefault (the PAGE never scrolls), and invokes the callback with the
-- raw wheel deltaY. Canvas-scoped DOM input plumbing — never touches WebGL.
foreign import installWheelListener
  :: (Number -> Effect Unit) -> Effect Unit

-- Wires the zoom range slider (#zoom-slider): on input/change, invokes the
-- callback with the parsed slider value (an absolute zoom in [minZoom,maxZoom]).
-- Mirrors installDragStrengthSlider in shape. DOM-only — never touches WebGL.
foreign import installZoomSlider
  :: (Number -> Effect Unit) -> Effect Unit

-- Write the live State.zoom back to the #zoom-slider value every frame so that
-- programmatic zoom changes (mouse wheel, Materials reframe) move the thumb.
-- Setting .value programmatically does NOT fire the input/change listener, so
-- there is no feedback loop. DOM-only — never touches WebGL.
foreign import setZoomSlider :: Number -> Effect Unit

-- Wires the on-screen orbit buttons (#orbit-left / #orbit-right step yaw,
-- #orbit-up / #orbit-down step pitch, #orbit-reset returns to zero). Each
-- argument is a plain `Effect Unit` click handler — the caller (Main) owns the
-- per-direction {dx,dy} decision (Camera.buttonOrbitDelta pushed through
-- Main.applyOrbit) and the scene gate, mirroring the empty-space orbit-drag.
-- DOM-only input plumbing — never touches WebGL.
foreign import installOrbitButtons
  :: Effect Unit
  -> Effect Unit
  -> Effect Unit
  -> Effect Unit
  -> Effect Unit
  -> Effect Unit

-- Publish the live eased Builder detail level to the `window.__builderDetail`
-- debug global (a number in [0,1]) for deterministic E2E observation. Called
-- every frame from the draw loop. DOM-only — never touches WebGL.
foreign import setBuilderDetail :: Number -> Effect Unit

-- Wire the Nuclide scene control buttons and inputs in the left drawer.
-- Each callback is a plain `Effect Unit` thunk (the PureScript caller owns the
-- per-button mutation — NuclearApi.addProton etc.). The reset callback is
-- invoked when #nuc-reset is clicked. The setNuclide callback receives (z, n)
-- from the #nuclide-z / #nuclide-n inputs + "Set" button. DOM-only.
foreign import installNuclearControls
  :: Effect Unit  -- addProton
  -> Effect Unit  -- removeProton
  -> Effect Unit  -- addNeutron
  -> Effect Unit  -- removeNeutron
  -> Effect Unit  -- reset
  -> (Int -> Int -> Effect Unit)  -- setNuclide z n
  -> Effect Unit

-- Wire the M3 named-reaction buttons (#react-alpha, #react-beta-minus,
-- #react-beta-plus, #react-fuse, #react-fission) in the Nuclide drawer.
-- fuseWith receives (z2, n2) from the #fuse-z2 / #fuse-n2 inputs.
-- fissionReact receives (zA, nA, zB, nB) from the fission fragment inputs.
foreign import installNuclearReactionControls
  :: Effect Unit                          -- decayAlpha
  -> Effect Unit                          -- decayBetaMinus
  -> Effect Unit                          -- decayBetaPlus
  -> (Int -> Int -> Effect Unit)          -- fuseWith z2 n2
  -> (Int -> Int -> Int -> Int -> Effect Unit)  -- fission zA nA zB nB
  -> Effect Unit

foreign import requestAnimationFrame
  :: Effect Unit -> Effect Unit

runLoop
  :: forall s
   . { initial :: s
     , step :: Input -> s -> s
     , draw :: s -> Effect Unit
     -- Optional one-shot state override: on each tick, if this Ref holds Just f,
     -- f is applied to the state (after `step`) and the Ref is cleared to Nothing.
     -- Use to inject a precise state change (e.g. zoom reset on scene entry) that
     -- persists naturally in subsequent frames (step preserves zoom when no
     -- zoomDelta arrives, so the zoom stays at the overridden value until the user
     -- zooms manually). The override fires at most once per write.
     , stateOverride :: Ref.Ref (Maybe (s -> s))
     }
  -> Effect Unit
runLoop spec = do
  stateRef <- Ref.new spec.initial
  inputRef <- Ref.new emptyInput
  installKeyUpListener \k ->
    Ref.modify_ (_ { lastKey = Just k }) inputRef
  installMouseMoveListener \x y ->
    Ref.modify_ (_ { mouse = Just { x, y } }) inputRef
  installShearButton \k ->
    Ref.modify_ (_ { shear = Just k }) inputRef
  installSceneToggle
    (Ref.modify_ (_ { toggleScene = true }) inputRef)
  installView2DToggle
    (Ref.modify_ (_ { toggle2D = true }) inputRef)
  installValenceOnlyToggle
    (Ref.modify_ (_ { toggleValenceOnly = true }) inputRef)
  installAntibondingToggle
    (Ref.modify_ (_ { toggleAntibonding = true }) inputRef)
  installSubshellViewToggle
    (Ref.modify_ (_ { toggleSubshellView = true }) inputRef)
  installElementInput \z ->
    Ref.modify_ (_ { element = Just z }) inputRef
  installDragStrengthSlider \d ->
    Ref.modify_ (_ { dragStrength = Just d }) inputRef
  installLayerSpaceSlider \v ->
    Ref.modify_ (_ { layerSpace = Just v }) inputRef
  -- Mouse wheel over the canvas: push the raw deltaY as a zoom step. The FFI
  -- preventDefault's the wheel so the page never scrolls.
  installWheelListener \d ->
    Ref.modify_ (_ { zoomDelta = Just d }) inputRef
  -- Zoom slider (#zoom-slider): on input/change, push an absolute zoom value
  -- into Input.zoomSet. applyZoomSet clamps to [minZoom,maxZoom] in Update.
  installZoomSlider \v ->
    Ref.modify_ (_ { zoomSet = Just v }) inputRef
  -- Bond control: clicking #bond-btn runs an anime.js value animation whose
  -- onUpdate pushes the current bond progress into the input ref (DOM-driven).
  installBondButton
    ( runBondAnimation \p ->
        Ref.modify_ (_ { bondProgress = Just p }) inputRef
    )
  -- NOTE: the canvas pointer pick+drag is wired ONCE in Main.installBuilderPick
  -- (the production Effect-routed path that consumes the DOM events directly).
  -- The loop intentionally does NOT register a second set of pointer listeners.
  let
    tick = do
      i <- Ref.read inputRef
      Ref.write emptyInput inputRef
      Ref.modify_ (spec.step i) stateRef
      -- Consume the one-shot state override (if any) AFTER step so the override
      -- takes effect on this frame AND persists into subsequent frames (step
      -- preserves the overridden field when no input drives it, e.g. zoom stays
      -- at the new value until the user scrolls).
      mOverride <- Ref.read spec.stateOverride
      case mOverride of
        Just f -> do
          Ref.write Nothing spec.stateOverride
          Ref.modify_ f stateRef
        Nothing -> pure unit
      s <- Ref.read stateRef
      spec.draw s
      requestAnimationFrame tick
  requestAnimationFrame tick
