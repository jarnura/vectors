module Update
  ( State
  , initialState
  , step
  , applyDetail
  , applyZoom
  , applyOrbit
  , applyToggle
  , applyToggle2D
  , applyValenceOnly
  , applySubshellView
  , applyDragStrength
  , applyElement
  , applyBondProgress
  , applyShear
  , applyKey
  , applyMouse
  ) where

import Prelude

import Camera as Camera
import Data.Int (toNumber)
import Data.Maybe (Maybe(..))
import Layer as Layer
import Math.Matrix (Matrix)
import Math.Matrix as M
import Builder as Builder
import FRP.Loop (Input)
import Scene (Scene(..), nextScene)
import Vector as V

type State =
  { transform :: Matrix Number
  , speed :: Number
  , mouseLast :: Maybe { x :: Int, y :: Int }
  , frame :: Number
  , scene :: Scene
  , element :: Int
  , view2D :: Boolean
  , valenceOnly :: Boolean
  , subshellView :: Boolean
  , dragStrength :: Number
  , bondProgress :: Number
  , zoom :: Number
  , detail :: Number
  -- Builder orbit (view) angles in radians, fed to Camera.viewProjection in the
  -- Builder scene only. Both 0.0 ⇒ viewProjection == projection ⇒ byte-identical
  -- render (M2 plumbing; M3 wires the drag→yaw/pitch).
  , builderYaw :: Number
  , builderPitch :: Number
  , builder :: Builder.BuilderState
  }

-- Maximum rotation per keypress, in degrees.
maxSpeed :: Number
maxSpeed = 10.0

-- Additional rotation each keypress accrues on top of the previous one.
speedStep :: Number
speedStep = 0.2

-- Degrees of rotation per pixel of mouse movement.
mouseSensitivity :: Number
mouseSensitivity = 0.1

initialState :: State
initialState =
  { transform: M.identity
  , speed: 0.0
  , mouseLast: Nothing
  , frame: 0.0
  , scene: CubePoc
  , element: 6 -- Carbon by default
  , view2D: false
  , valenceOnly: false
  -- Sub-shell view is the default (each filled sub-shell its own ring); the
  -- #subshell-view checkbox unchecks to the SHELL-only (Bohr) ring view.
  , subshellView: true
  -- How hard a single-atom drag pulls against its bonds (the #drag-strength
  -- slider). Bonds with bondEnergy >= dragStrength HOLD (tugging the partner
  -- along); weaker ones snap. 0.0 = every bond holds (nothing breaks); the
  -- slider default 3.0 snaps weak bonds (O-O 1.46) but not strong ones (O-H 4.63).
  , dragStrength: 3.0
  -- 1.0 = bonded resting state (nuclei at full separation, in the outer thirds).
  -- The bond animation sweeps this 1→0→1, drawing the atoms together and back.
  , bondProgress: 1.0
  -- 1.0 = the unchanged default camera distance (no zoom). Mouse-wheel events
  -- multiply this in/out, clamped to Camera.min/maxZoom.
  , zoom: 1.0
  -- 1.0 = full sub-atomic detail (Builder default zoom 1.0 → smoothstep == 1.0).
  -- Eased toward Layer.layerBlend s.zoom each frame so zoom changes cross-fade
  -- the Builder atoms smoothly between balls (0) and full detail (1).
  , detail: 1.0
  -- Builder orbit angles start flat (yaw = pitch = 0) so the Builder view is
  -- byte-identical to the plain projection until M3 wires the drag.
  , builderYaw: 0.0
  , builderPitch: 0.0
  , builder: Builder.emptyBuilder
  }

step :: Input -> State -> State
step input =
  applyToggle input.toggleScene
    >>> applyToggle2D input.toggle2D
    >>> applyValenceOnly input.toggleValenceOnly
    >>> applySubshellView input.toggleSubshellView
    >>> applyDragStrength input.dragStrength
    >>> applyElement input.element
    >>> applyBondProgress input.bondProgress
    >>> applyShear input.shear
    >>> applyKey input.lastKey
    >>> applyMouse input.mouse
    >>> applyZoom input.zoomDelta
    >>> applyDetail
    >>> tickFrame
  where
  tickFrame s = s { frame = s.frame + 1.0 }

-- Ease the Builder level-of-detail one frame toward the smoothstep of the live
-- zoom (Layer.layerBlend). Run EVERY frame (right after applyZoom) so even
-- discrete #zoom-in/#zoom-out steps cross-fade smoothly rather than snapping.
-- Other scenes ignore s.detail when rendering, so easing always is harmless.
applyDetail :: State -> State
applyDetail s = s { detail = Layer.easeDetail s.detail (Layer.layerBlend s.zoom) }

-- Apply one mouse-wheel step to the camera zoom (clamped inside applyZoomStep).
applyZoom :: Maybe Number -> State -> State
applyZoom Nothing s = s
applyZoom (Just d) s = s { zoom = Camera.applyZoomStep s.zoom d }

-- Radians of Builder orbit per pixel of empty-space drag. Small so a full screen
-- drag (~hundreds of px) sweeps a comfortable arc; pitch is clamped at ±maxPitch.
orbitSens :: Number
orbitSens = 0.01

-- Pure orbit update: fold one drag delta (cursor dx/dy in pixels) into an orbit
-- record. Yaw accumulates freely about Y; pitch about X is clamped to ±maxPitch
-- (Camera.clampPitch) so the camera never flips over the poles. Immutable —
-- returns a NEW orbit record. Shared by the empty-space orbit-drag and reusable
-- for tests; the orbit Ref is the single source of truth this writes into.
applyOrbit
  :: { dx :: Number, dy :: Number }
  -> { yaw :: Number, pitch :: Number }
  -> { yaw :: Number, pitch :: Number }
applyOrbit d o =
  { yaw: o.yaw + d.dx * orbitSens
  , pitch: Camera.clampPitch (o.pitch + d.dy * orbitSens)
  }

-- Flip between the cube POC and atomos when the scene switch is clicked.
applyToggle :: Boolean -> State -> State
applyToggle false s = s
applyToggle true s = s { scene = nextScene s.scene }

-- Flip the 2D/3D view of the atom when the "2D" checkbox changes.
applyToggle2D :: Boolean -> State -> State
applyToggle2D false s = s
applyToggle2D true s = s { view2D = not s.view2D }

-- Flip "valence only" (hide core electrons in the Builder) when the checkbox
-- changes. Mirrors applyToggle2D.
applyValenceOnly :: Boolean -> State -> State
applyValenceOnly false s = s
applyValenceOnly true s = s { valenceOnly = not s.valenceOnly }

-- Flip the atomos sub-shell/shell view when the "#subshell-view" checkbox
-- changes. true (sub-shells) draws one ring per filled sub-shell; false
-- (shell-only/Bohr) collapses each shell onto a single ring. Mirrors
-- applyToggle2D.
applySubshellView :: Boolean -> State -> State
applySubshellView false s = s
applySubshellView true s = s { subshellView = not s.subshellView }

-- Receive the live drag strength from the #drag-strength slider (DOM-driven).
-- Drives the Builder single-atom drag: bonds with bondEnergy >= dragStrength
-- hold and tug their partner along; weaker bonds snap. Mirrors applyElement.
applyDragStrength :: Maybe Number -> State -> State
applyDragStrength Nothing s = s
applyDragStrength (Just d) s = s { dragStrength = d }

-- Select the rendered element (atomic number) from the selector. Out-of-range
-- values are clamped downstream by Atom.elementOf.
applyElement :: Maybe Int -> State -> State
applyElement Nothing s = s
applyElement (Just z) s = s { element = z }

-- Receive bond progress from the anime.js bond animation (DOM-driven). 1.0 =
-- atoms at full separation (resting); 0.0 = coalesced toward the bond midpoint.
applyBondProgress :: Maybe Number -> State -> State
applyBondProgress Nothing s = s
applyBondProgress (Just p) s = s { bondProgress = p }

-- Apply a shear (by the button's input value) to the main cube's transform.
applyShear :: Maybe Number -> State -> State
applyShear Nothing s = s
applyShear (Just k) s = s { transform = M.multiply (M.shear k) s.transform }

applyKey :: Maybe String -> State -> State
applyKey Nothing s = s
applyKey (Just key) s =
  let
    nextSpeed = if s.speed < maxSpeed then s.speed + speedStep else s.speed
    rotation = case key of
      "ArrowLeft" -> Just (V.rotateY nextSpeed)
      "ArrowRight" -> Just (V.rotateY (negate nextSpeed))
      "ArrowUp" -> Just (V.rotateX nextSpeed)
      "ArrowDown" -> Just (V.rotateX (negate nextSpeed))
      _ -> Nothing
  in
    case rotation of
      Nothing -> s
      Just r -> s
        { transform = M.multiply r s.transform
        , speed = nextSpeed
        }

applyMouse :: Maybe { x :: Int, y :: Int } -> State -> State
applyMouse Nothing s = s
applyMouse (Just pos) s = case s.mouseLast of
  Nothing -> s { mouseLast = Just pos }
  Just last ->
    let
      dx = toNumber pos.x - toNumber last.x
      dy = toNumber pos.y - toNumber last.y
      r = M.multiply
        (V.rotateX (negate (dy * mouseSensitivity)))
        (V.rotateY (dx * mouseSensitivity))
    in
      s
        { transform = M.multiply r s.transform
        , mouseLast = Just pos
        }
