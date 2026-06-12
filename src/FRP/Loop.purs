module FRP.Loop
  ( Input
  , emptyInput
  , runLoop
  , installAddButton
  , installClearButton
  , installCanvasPointer
  , installWheelListener
  , installZoomButtons
  , installValenceOnlyToggle
  , installSubshellViewToggle
  , installDragStrengthSlider
  , setBuilderDetail
  ) where

import Prelude

import Camera as Camera
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
  , toggleSubshellView :: Boolean
  , element :: Maybe Int
  , dragStrength :: Maybe Number
  , bondProgress :: Maybe Number
  , zoomDelta :: Maybe Number
  }

emptyInput :: Input
emptyInput =
  { lastKey: Nothing
  , mouse: Nothing
  , shear: Nothing
  , toggleScene: false
  , toggle2D: false
  , toggleValenceOnly: false
  , toggleSubshellView: false
  , element: Nothing
  , dragStrength: Nothing
  , bondProgress: Nothing
  , zoomDelta: Nothing
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

-- Wires the on-screen zoom buttons (#zoom-in / #zoom-out): the first argument is
-- the per-click delta magnitude. #zoom-in fires cb(−mag) for zoom IN, #zoom-out
-- fires cb(+mag) for zoom OUT — synthetic wheel deltas that reuse the same zoom
-- channel as the mouse wheel. DOM-only input plumbing — never touches WebGL.
foreign import installZoomButtons
  :: Number -> (Number -> Effect Unit) -> Effect Unit

-- Publish the live eased Builder detail level to the `window.__builderDetail`
-- debug global (a number in [0,1]) for deterministic E2E observation. Called
-- every frame from the draw loop. DOM-only — never touches WebGL.
foreign import setBuilderDetail :: Number -> Effect Unit

foreign import requestAnimationFrame
  :: Effect Unit -> Effect Unit

runLoop
  :: forall s
   . { initial :: s
     , step :: Input -> s -> s
     , draw :: s -> Effect Unit
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
  installSubshellViewToggle
    (Ref.modify_ (_ { toggleSubshellView = true }) inputRef)
  installElementInput \z ->
    Ref.modify_ (_ { element = Just z }) inputRef
  installDragStrengthSlider \d ->
    Ref.modify_ (_ { dragStrength = Just d }) inputRef
  -- Mouse wheel over the canvas: push the raw deltaY as a zoom step. The FFI
  -- preventDefault's the wheel so the page never scrolls.
  installWheelListener \d ->
    Ref.modify_ (_ { zoomDelta = Just d }) inputRef
  -- On-screen zoom buttons push a synthetic wheel delta into the SAME channel:
  -- #zoom-in → −buttonZoomDelta (zoom in), #zoom-out → +buttonZoomDelta (out).
  installZoomButtons Camera.buttonZoomDelta \d ->
    Ref.modify_ (_ { zoomDelta = Just d }) inputRef
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
      s <- Ref.read stateRef
      spec.draw s
      requestAnimationFrame tick
  requestAnimationFrame tick
