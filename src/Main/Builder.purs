module Main.Builder
  ( clearColorFor
  , updateOverlay
  , satelliteTransform
  , ringSegments
  , projectionFor
  , updateViewport
  , clamp01
  , syncAtomLabels
  , pickRadius
  , installBuilderPick
  ) where

import Prelude

import Atom as Atom
import Builder as Builder
import Camera as Camera
import Controls as Controls
import Data.Either (Either(..))
import Data.Foldable (minimumBy)
import Data.Maybe (Maybe(..))
import Data.Number (cos, pi, sin, sqrt)
import Effect (Effect)
import Effect.Ref as Ref
import FRP.Loop (installCanvasPointer)
import Graphics.Canvas (CanvasElement, getCanvasHeight, getCanvasWidth)
import Graphics.GL (Renderer)
import Graphics.GL as GL
import Labels as Labels
import Math.Matrix (Matrix)
import Math.Matrix as M
import Molecule as Molecule
import Scene (Scene(..), sceneTitle, spaceColor)
import Scene.Entities (builderScale, builderWorldPos)
import Text as Text
import Update (State)
import World (skyColor)

-- Satellite orbits the world origin on the XZ plane.
satelliteOrbitRadius :: Number
satelliteOrbitRadius = 200.0

-- One full orbit in 5 seconds at 60 fps = 1.2 deg/frame.
satelliteDegreesPerFrame :: Number
satelliteDegreesPerFrame = 1.2

-- The backdrop (clear) color for the current scene.
clearColorFor :: Scene -> GL.Color
clearColorFor CubePoc = skyColor
clearColorFor Atomos = spaceColor
clearColorFor Molecule = spaceColor
clearColorFor Builder = spaceColor

-- Animate the HTML overlay label only when the scene or element changes (not
-- every frame). The label shows the element name and is visible only in atomos.
updateOverlay
  :: Ref.Ref { scene :: Scene, element :: Int } -> State -> Effect Unit
updateOverlay ref s = do
  prev <- Ref.read ref
  let
    sceneChanged = prev.scene /= s.scene
    elementChanged = prev.element /= s.element
    inAtomos = s.scene == Atomos
    inMolecule = s.scene == Molecule
  when sceneChanged do
    Text.scrambleInto "scene-title" (sceneTitle s.scene)
    Text.setVisible "atom-label" inAtomos
    Text.setVisible "orbital-info" inAtomos
    -- The data-driven molecule properties panel is molecule-scene only. Render
    -- its rows from Molecule.properties (one row per property) and show it on
    -- entering the scene; hide it elsewhere. Built once per scene change.
    Text.setVisible "molecule-info" inMolecule
    when inMolecule do
      Controls.renderInfoPanel "molecule-info" (Molecule.moleculeOf 0).properties
    when inAtomos do
      Text.scrambleInto "atom-label" (Atom.elementName s.element)
      Text.scrambleInto "orbital-info" (Atom.configString s.element)
  when (elementChanged && inAtomos) do
    Text.scrambleInto "atom-label" (Atom.elementName s.element)
    Text.scrambleInto "orbital-info" (Atom.configString s.element)
  when (sceneChanged || elementChanged) do
    Ref.write { scene: s.scene, element: s.element } ref

-- Satellite's world-space transform: a circular orbit at constant Y=0.
satelliteTransform :: State -> Matrix Number
satelliteTransform s =
  let
    angleRad = s.frame * satelliteDegreesPerFrame * pi / 180.0
  in
    M.translate
      (satelliteOrbitRadius * cos angleRad)
      0.0
      (satelliteOrbitRadius * sin angleRad)

-- Number of segments in each orbital ring line (smooth circle, thin wireframe).
ringSegments :: Int
ringSegments = 96

-- The projection matrix for a scene at a given canvas size. The Builder scene
-- routes through Camera.viewProjection (zoom-aware perspective × orbit rotation)
-- so the builderYaw/builderPitch orbit applies; every OTHER scene uses the plain
-- Camera.projection (they have no orbit). At yaw = pitch = 0 viewProjection is
-- byte-identical to projection, so the Builder render is unchanged until M3.
projectionFor :: State -> Number -> Number -> Matrix Number
projectionFor s w h
  | s.scene == Builder =
      Camera.viewProjection { yaw: s.builderYaw, pitch: s.builderPitch } s.zoom w h
  | otherwise = Camera.projection s.zoom w h

updateViewport :: Renderer -> CanvasElement -> State -> Effect Unit
updateViewport renderer canvas s = do
  w <- getCanvasWidth canvas
  h <- getCanvasHeight canvas
  GL.resizeRenderer renderer { width: w, height: h }
  GL.setProjection renderer (M.toVector (projectionFor s w h))

-- Clamp a number into [0, 1] (label opacity is 1 − detail, kept in range).
clamp01 :: Number -> Number
clamp01 n
  | n < 0.0 = 0.0
  | n > 1.0 = 1.0
  | otherwise = n

-- Sync the per-atom atomic-SYMBOL overlay labels for the live State. In the
-- Builder scene each placed atom is projected to backing-store pixels with the
-- EXACT projection the renderer drew with (builderWorldPos at the live zoom, so a
-- label sits on its ball), then mapped to CSS pixels (× client/backing ratio) and
-- handed to Labels.syncAtomLabels (one keyed div per atom id, text = element
-- symbol, opacity = 1 − detail so labels read in the zoomed-OUT ball layer and
-- fade as the sub-atomic detail blooms in). Off Builder the labels are cleared.
-- DOM only via the Labels FFI — never WebGL.
syncAtomLabels :: CanvasElement -> State -> Effect Unit
syncAtomLabels canvas s
  | s.scene == Builder = do
      w <- getCanvasWidth canvas
      h <- getCanvasHeight canvas
      client <- Labels.getCanvasClientSize canvas
      let
        proj = projectionFor s w h
        sx = if w > 0.0 then client.w / w else 1.0
        sy = if h > 0.0 then client.h / h else 1.0
        opacity = clamp01 (1.0 - s.detail)
        items =
          map
            ( \a ->
                let
                  scr = Builder.projectToScreen proj { w, h } (builderWorldPos a.pos)
                in
                  { id: a.id
                  , x: scr.x * sx
                  , y: scr.y * sy
                  , text: Atom.symbolOf a.z
                  , opacity
                  }
            )
            s.builder.atoms
      Labels.syncAtomLabels items
  | otherwise = Labels.clearAtomLabels

-- Pixel radius within which a pointer-down counts as picking an atom. Generous
-- enough to grab the centre atom under a real mouse drag.
pickRadius :: Number
pickRadius = 80.0

-- Production pointer pick + drag for the Builder scene. Scene-gated: it only acts
-- when the current scene is Builder, so cube-POC mouse rotation (applyMouse) is
-- untouched. The drag is routed through the SAME shared builder Ref + eagerRender
-- as window.__builder.moveAtom, so the live valence-aware re-bonding runs and the
-- change is on-screen immediately.
--
-- Picking/dragging use the EXACT projection the renderer uses
-- (perspectiveProjection at the live canvas size, which composes the camera
-- translate), and project the atom's scaled world position (builderWorldPos).
-- unprojectAtDepth places the cursor onto the camera-facing plane at the picked
-- atom's depth; dividing back out builderScale recovers the builder-model pos.
installBuilderPick
  :: CanvasElement
  -> Ref.Ref Builder.BuilderState
  -> Ref.Ref { yaw :: Number, pitch :: Number }
  -> (Builder.BuilderState -> Effect Unit)
  -> Effect State
  -> ({ dx :: Number, dy :: Number } -> { yaw :: Number, pitch :: Number } -> { yaw :: Number, pitch :: Number })
  -> Effect Unit
installBuilderPick canvas builderRef orbitRef eagerRender readState applyOrbit = do
  -- The active drag, latched on mousedown: either dragging an ATOM (with its
  -- depth + whole-molecule flag) or ORBITing (empty-space miss) — orbit tracks
  -- the previous cursor pixel so each move feeds a delta into the orbit Ref.
  dragRef <-
    Ref.new
      ( Nothing
          :: Maybe
               ( Either
                   { id :: Int, ref :: Atom.V3, whole :: Boolean }
                   { x :: Number, y :: Number }
               )
      )
  let
    -- Current full view-projection (orbit-aware) + bare projection + canvas dims
    -- (live size) at the live camera zoom and orbit, so pick/unproject use the
    -- SAME transform the renderer drew with (true-depth drag under orbit).
    projAndCanvas zoom orb = do
      w <- getCanvasWidth canvas
      h <- getCanvasHeight canvas
      pure
        { vp: Camera.viewProjection orb zoom w h
        , proj: Camera.projection zoom w h
        , orb: Camera.orbit orb.yaw orb.pitch
        , canvas: { w, h }
        }

    onDown px py detail = do
      s <- readState
      when (s.scene == Builder) do
        orb <- Ref.read orbitRef
        pc <- projAndCanvas s.zoom orb
        bs <- Ref.read builderRef
        let
          cursor = { x: px, y: py }
          -- Project every atom's scaled world position (through the orbit-aware
          -- view-projection) and measure pixel distance.
          candidates =
            map
              ( \a ->
                  let
                    scr = Builder.projectToScreen pc.vp pc.canvas (builderWorldPos a.pos)
                    dx = scr.x - cursor.x
                    dy = scr.y - cursor.y
                  in
                    { id: a.id, pos: a.pos, dist: sqrt (dx * dx + dy * dy) }
              )
              bs.atoms
          nearest = minimumBy (comparing _.dist) candidates
        case nearest of
          -- HIT: latch an atom drag (whole-molecule on single click, single atom
          -- on double click via the native event.detail).
          Just c | c.dist <= pickRadius ->
            Ref.write
              (Just (Left { id: c.id, ref: builderWorldPos c.pos, whole: detail < 2 }))
              dragRef
          -- MISS: latch ORBIT mode, seeding the previous cursor pixel.
          _ -> Ref.write (Just (Right { x: px, y: py })) dragRef

    onMove px py = do
      mdrag <- Ref.read dragRef
      case mdrag of
        Nothing -> pure unit
        Just (Left pick) -> do
          s <- readState
          when (s.scene == Builder) do
            orb <- Ref.read orbitRef
            pc <- projAndCanvas s.zoom orb
            let
              -- Reference is the picked atom's FULL world position so the
              -- orbit-aware unproject keeps the cursor on the camera-facing plane
              -- at the atom's true view-space depth. unprojectAtDepthFull rotates
              -- it through the orbit and back, so the atom moves in TRUE world
              -- depth even when the scene is orbited and the atom is off-origin.
              modelRef = pick.ref
              world =
                Builder.unprojectAtDepthFull pc.orb pc.proj pc.canvas { x: px, y: py } modelRef
              -- Back out builderScale to recover the builder-model position.
              modelPos =
                { x: world.x / builderScale
                , y: world.y / builderScale
                , z: world.z / builderScale
                }
            -- Single-atom drags pull against the LIVE slider strength
            -- (s.dragStrength, from the same readState snapshot as zoom/scene):
            -- strong bonds hold + tug their partner, weak ones snap. Whole-
            -- molecule drags translate rigidly as before.
            Ref.modify_ (if pick.whole then Builder.moveMolecule pick.id modelPos else Builder.moveAtomWith s.dragStrength pick.id modelPos) builderRef
            bs <- Ref.read builderRef
            eagerRender bs
        Just (Right prev) -> do
          s <- readState
          when (s.scene == Builder) do
            -- ORBIT: fold the cursor delta since the last move into the SINGLE
            -- orbit Ref the renderer/pick/seam share, then re-track the cursor.
            -- The next rAF frame mirrors the Ref into State and re-uploads the GPU
            -- projection (sizeRef gate now watches yaw/pitch), making the orbit
            -- visible — the GPU projection can only be re-uploaded in the draw
            -- loop (which owns the renderer), so no eager re-render here.
            let delta = { dx: px - prev.x, dy: py - prev.y }
            Ref.modify_ (applyOrbit delta) orbitRef
            Ref.write (Just (Right { x: px, y: py })) dragRef

    onUp = Ref.write Nothing dragRef
  installCanvasPointer onDown onMove onUp
