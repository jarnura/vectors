module Update
  ( State
  , initialState
  , step
  , applyDetail
  , applyZoom
  , applyZoomSet
  , applyOrbit
  , applyToggle
  , applyToggle2D
  , applyValenceOnly
  , applyFreeElectronsOnly
  , applyAntibonding
  , applySubshellView
  , applyDragStrength
  , applyLayerSpace
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
import Nuclear (NuclearState)
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
  -- Builder/Materials render flag: when true ONLY free/lone electrons are shown;
  -- the shared covalent bonding-pair electrons are hidden. Composes independently
  -- with valenceOnly. Initialises to false (bonding pair visible by default).
  , freeElectronsOnly :: Boolean
  -- Builder/Materials render flag (M3-S2): when true the shared bonding-electron
  -- positions switch from the Bonding to the Antibonding placement
  -- (bondElectronPositionsPhased Antibonding). Render-only; never mutates
  -- BuilderState. Initialises to false (Bonding is the default view).
  , antibonding :: Boolean
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
  -- Layer-space multiplier (M1-layer-space): effective Builder world scale =
  -- builderScale × layerSpace. Range 1.0–4.0, step 0.1, default 1.6 (visibly
  -- more spacious than the cramped unit baseline of 1.0). Driven by the
  -- #layer-space range slider; Builder and Materials share ONE render path
  -- so both scenes benefit. Other scenes (CubePoc/Atomos/Molecule) are unaffected.
  , layerSpace :: Number
  , builder :: Builder.BuilderState
  -- Nuclide scene: live snapshot of the shared NuclearApi Ref (mirrored each
  -- frame, like State.builder). Kept here so render code reads a consistent
  -- immutable snapshot within one frame without re-reading the Ref.
  , nuclear :: NuclearState
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
  -- Free-electrons-only starts false: bonding pair is visible by default.
  , freeElectronsOnly: false
  -- Antibonding starts false: Bonding placement is the default render view.
  , antibonding: false
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
  -- Layer-space default 1.6: effective scale = builderScale (2.2) × 1.6 = 3.52,
  -- which is visibly more spacious than the unit baseline (2.2). Slider range
  -- 1.0–4.0, step 0.1. Clamped by applyLayerSpace to that range.
  , layerSpace: 1.6
  , builder: Builder.emptyBuilder
  -- Nuclide scene seed: Carbon-12 (Z=6, N=6), the same default as NuclearApi.
  -- Overwritten each frame from the shared NuclearApi Ref (like State.builder).
  , nuclear: { nuclide: { z: 6, n: 6 }, lastQ: 0.0, lastFission: Nothing }
  }

step :: Input -> State -> State
step input =
  applyToggle input.toggleScene
    >>> applyToggle2D input.toggle2D
    >>> applyValenceOnly input.toggleValenceOnly
    >>> applyFreeElectronsOnly input.toggleFreeElectronsOnly
    >>> applyAntibonding input.toggleAntibonding
    >>> applySubshellView input.toggleSubshellView
    >>> applyDragStrength input.dragStrength
    >>> applyLayerSpace input.layerSpace
    >>> applyElement input.element
    >>> applyBondProgress input.bondProgress
    >>> applyShear input.shear
    >>> applyKey input.lastKey
    >>> applyMouse input.mouse
    -- Slider set (absolute) comes BEFORE wheel delta so both feed applyDetail.
    >>> applyZoomSet input.zoomSet
    >>> applyZoom input.zoomDelta
    >>> applyDetail
    >>> tickFrame
  where
  tickFrame s = s { frame = s.frame + 1.0 }

-- Ease the Builder level-of-detail one frame toward the smoothstep of the live
-- zoom (Layer.layerBlend). Run EVERY frame (right after applyZoom) so even
-- discrete zoom changes (slider set or wheel notch) cross-fade smoothly rather
-- than snapping. Other scenes ignore s.detail when rendering, so easing always
-- is harmless.
applyDetail :: State -> State
applyDetail s = s { detail = Layer.easeDetail s.detail (Layer.layerBlend s.zoom) }

-- Apply an absolute zoom value from the #zoom-slider (clamped to [minZoom,maxZoom]).
-- Nothing is identity; Just v sets zoom = Camera.clampZoom v. Composed in step
-- BEFORE applyZoom (wheel delta) so the slider and the wheel both feed applyDetail.
applyZoomSet :: Maybe Number -> State -> State
applyZoomSet Nothing s = s
applyZoomSet (Just v) s = s { zoom = Camera.clampZoom v }

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

-- Flip "free electrons only" (hide bonding-pair electrons in the Builder) when
-- the #free-electrons-only checkbox changes. Mirrors applyValenceOnly exactly.
applyFreeElectronsOnly :: Boolean -> State -> State
applyFreeElectronsOnly false s = s
applyFreeElectronsOnly true s = s { freeElectronsOnly = not s.freeElectronsOnly }

-- Flip the Builder antibonding render mode when the "#antibonding" checkbox
-- changes. When true the shared bonding-electron positions switch from the
-- Bonding placement (standard in-phase pair) to the Antibonding placement
-- (node at midpoint, electrons pushed outward past each nucleus). Render-only —
-- never mutates BuilderState. Mirrors applyValenceOnly exactly.
applyAntibonding :: Boolean -> State -> State
applyAntibonding false s = s
applyAntibonding true s = s { antibonding = not s.antibonding }

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

-- Layer-space range: 1.0 (unit, historic builderScale spread) .. 4.0 (4× wider).
-- Default 1.6 (slider init). Effective Builder world scale = builderScale × layerSpace.
layerSpaceMin :: Number
layerSpaceMin = 1.0

layerSpaceMax :: Number
layerSpaceMax = 4.0

clampLayerSpace :: Number -> Number
clampLayerSpace v
  | v < layerSpaceMin = layerSpaceMin
  | v > layerSpaceMax = layerSpaceMax
  | otherwise = v

-- Receive the live layer-space from the #layer-space slider (DOM-driven). The
-- value is clamped to [1.0, 4.0] (the slider range) before storing — Nothing
-- is identity, Just v sets layerSpace = clamp v. Mirrors applyDragStrength.
applyLayerSpace :: Maybe Number -> State -> State
applyLayerSpace Nothing s = s
applyLayerSpace (Just v) s = s { layerSpace = clampLayerSpace v }

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
