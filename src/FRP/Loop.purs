module FRP.Loop
  ( Input
  , emptyInput
  , runLoop
  ) where

import Prelude

import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Ref as Ref

type Input =
  { lastKey :: Maybe String
  , mouse :: Maybe { x :: Int, y :: Int }
  , shear :: Maybe Number
  , toggleScene :: Boolean
  , element :: Maybe Int
  }

emptyInput :: Input
emptyInput =
  { lastKey: Nothing
  , mouse: Nothing
  , shear: Nothing
  , toggleScene: false
  , element: Nothing
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

-- Wires the element selector: on change, invokes the callback with the chosen
-- atomic number.
foreign import installElementInput
  :: (Int -> Effect Unit) -> Effect Unit

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
  installElementInput \z ->
    Ref.modify_ (_ { element = Just z }) inputRef
  let
    tick = do
      i <- Ref.read inputRef
      Ref.write emptyInput inputRef
      Ref.modify_ (spec.step i) stateRef
      s <- Ref.read stateRef
      spec.draw s
      requestAnimationFrame tick
  requestAnimationFrame tick
