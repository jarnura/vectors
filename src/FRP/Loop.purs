module FRP.Loop
  ( Input
  , PointerPos
  , emptyInput
  , runLoop
  , installAddButton
  , installClearButton
  , installCanvasPointer
  ) where

import Prelude

import Controls (installBondButton, runBondAnimation)
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Ref as Ref

-- A pointer position in canvas-local backing-store pixels (origin top-left).
type PointerPos = { x :: Number, y :: Number }

type Input =
  { lastKey :: Maybe String
  , mouse :: Maybe { x :: Int, y :: Int }
  , shear :: Maybe Number
  , toggleScene :: Boolean
  , toggle2D :: Boolean
  , element :: Maybe Int
  , bondProgress :: Maybe Number
  , pointerDown :: Maybe PointerPos
  , pointerMove :: Maybe PointerPos
  , pointerUp :: Boolean
  }

emptyInput :: Input
emptyInput =
  { lastKey: Nothing
  , mouse: Nothing
  , shear: Nothing
  , toggleScene: false
  , toggle2D: false
  , element: Nothing
  , bondProgress: Nothing
  , pointerDown: Nothing
  , pointerMove: Nothing
  , pointerUp: false
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

-- Wires the element selector: on change, invokes the callback with the chosen
-- atomic number.
foreign import installElementInput
  :: (Int -> Effect Unit) -> Effect Unit

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
-- canvas backing width/height vs its CSS size). DOM-only input plumbing — reads
-- canvas geometry but never touches WebGL.
foreign import installCanvasPointer
  :: (Number -> Number -> Effect Unit)
  -> (Number -> Number -> Effect Unit)
  -> Effect Unit
  -> Effect Unit

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
  installElementInput \z ->
    Ref.modify_ (_ { element = Just z }) inputRef
  -- Bond control: clicking #bond-btn runs an anime.js value animation whose
  -- onUpdate pushes the current bond progress into the input ref (DOM-driven).
  installBondButton
    ( runBondAnimation \p ->
        Ref.modify_ (_ { bondProgress = Just p }) inputRef
    )
  -- Pointer (mouse) down/move/up over the canvas, in canvas-local backing-store
  -- pixels. These feed the FRP input so a future pure `step` could observe them;
  -- the production Builder pick+drag consumes the same DOM events directly via a
  -- separate Effect-routed installer (see Main.installBuilderPick).
  installCanvasPointer
    (\x y -> Ref.modify_ (_ { pointerDown = Just { x, y } }) inputRef)
    (\x y -> Ref.modify_ (_ { pointerMove = Just { x, y } }) inputRef)
    (Ref.modify_ (_ { pointerUp = true }) inputRef)
  let
    tick = do
      i <- Ref.read inputRef
      Ref.write emptyInput inputRef
      Ref.modify_ (spec.step i) stateRef
      s <- Ref.read stateRef
      spec.draw s
      requestAnimationFrame tick
  requestAnimationFrame tick
