module Main.Builder
  ( clearColorFor
  , isBuilderLike
  , updateOverlay
  , satelliteTransform
  , ringSegments
  , projectionFor
  , updateViewport
  , clamp01
  , syncAtomLabels
  , renderPeOverlay
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
import Data.Array (head, mapMaybe) as Array
import Molecule as Molecule
import Pe as Pe
import PeOverlay as PeOverlay
import Scene (Scene(..), sceneTitle, spaceColor)
import Scene.Entities (builderScale, builderWorldPosWith)
import Text as Text
import Update (State)
import World (skyColor)

-- Satellite orbits the world origin on the XZ plane.
satelliteOrbitRadius :: Number
satelliteOrbitRadius = 200.0

-- One full orbit in 5 seconds at 60 fps = 1.2 deg/frame.
satelliteDegreesPerFrame :: Number
satelliteDegreesPerFrame = 1.2

-- True when the scene uses the Builder render path (Builder or Materials).
-- Nuclide is NOT builder-like: it uses its own nucleus-only render.
isBuilderLike :: Scene -> Boolean
isBuilderLike Builder = true
isBuilderLike Materials = true
isBuilderLike _ = false

-- The backdrop (clear) color for the current scene.
clearColorFor :: Scene -> GL.Color
clearColorFor CubePoc = skyColor
clearColorFor Atomos = spaceColor
clearColorFor Molecule = spaceColor
clearColorFor Builder = spaceColor
clearColorFor Materials = spaceColor
clearColorFor Nuclide = spaceColor

-- Animate the HTML overlay label only when the scene or element changes (not
-- every frame). The label shows the element name and is visible only in atomos.
-- Also shows/hides the #nuclide-info panel and #nuclide-controls drawer section.
updateOverlay
  :: Ref.Ref { scene :: Scene, element :: Int } -> State -> Effect Unit
updateOverlay ref s = do
  prev <- Ref.read ref
  let
    sceneChanged = prev.scene /= s.scene
    elementChanged = prev.element /= s.element
    inAtomos = s.scene == Atomos
    inMolecule = s.scene == Molecule
    inNuclide = s.scene == Nuclide
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
    -- Nuclide scene: show/hide the nuclide info panel + drawer section.
    Controls.showNuclidePanel inNuclide
    Controls.showNuclideSectionInDrawer inNuclide
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

-- The projection matrix for a scene at a given canvas size. The Builder and
-- Materials scenes route through Camera.viewProjection (zoom-aware perspective ×
-- orbit rotation) so the builderYaw/builderPitch orbit applies; every OTHER scene
-- uses the plain Camera.projection (no orbit). At yaw = pitch = 0 viewProjection
-- is byte-identical to projection, so both scenes are unchanged at zero orbit.
projectionFor :: State -> Number -> Number -> Matrix Number
projectionFor s w h
  | isBuilderLike s.scene =
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
  | isBuilderLike s.scene = do
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
                  scr = Builder.projectToScreen proj { w, h } (builderWorldPosWith s.layerSpace a.pos)
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

-- Number of curve samples drawn into the PE overlay (resolution of the Morse
-- polyline). 80 points span the repulsive wall, well, and dissociation tail.
peCurveResolution :: Int
peCurveResolution = 80

-- Euclidean distance between two world points (local — Builder.Geom.distance is
-- not re-exported through the facade, and this avoids a new facade export).
v3Distance :: Atom.V3 -> Atom.V3 -> Number
v3Distance a b =
  let
    dx = a.x - b.x
    dy = a.y - b.y
    dz = a.z - b.z
  in
    sqrt (dx * dx + dy * dy + dz * dz)

-- Render the Builder/Materials potential-energy (Morse) curve overlay.
-- Render-only and DOM-only (via the PeOverlay FFI) — it never mutates the
-- BuilderState.
--
-- Representative pair: the element pair of the FIRST live bond if any bond
-- exists, else a default Carbon–Carbon (6,6) so the well is always visible. The
-- curve is `Pe.peCurveSamples 80 z1 z2` for that pair.
--
-- Markers: one live dot per bond at its current internuclear distance r and the
-- Morse energy there — built by looking up each bond's two atoms (z + pos),
-- computing r, and running `Pe.bondCurveMarkers`. The overlay shows + dot(s)
-- only in Builder and Materials; every other scene hides it.
renderPeOverlay :: State -> Effect Unit
renderPeOverlay s
  | isBuilderLike s.scene = do
      let
        bs = s.builder
        -- Per-bond {z1,z2,r}: skip a bond if either atom can't be resolved.
        bondData =
          Array.mapMaybe
            ( \bd -> case Builder.atomById bs bd.a, Builder.atomById bs bd.b of
                Just a, Just b ->
                  Just { z1: a.z, z2: b.z, r: v3Distance a.pos b.pos }
                _, _ -> Nothing
            )
            bs.bonds
        -- Representative pair: first bond's elements, else Carbon–Carbon (6,6).
        rep = case Array.head bondData of
          Just d -> { z1: d.z1, z2: d.z2 }
          Nothing -> { z1: 6, z2: 6 }
        samples = Pe.peCurveSamples peCurveResolution rep.z1 rep.z2
        markers =
          map (\m -> { r: m.r, e: m.e }) (Pe.bondCurveMarkers bondData)
      PeOverlay.renderPeCurve { samples, markers }
      PeOverlay.setPeOverlayVisible true
  | otherwise = PeOverlay.setPeOverlayVisible false

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
      when (isBuilderLike s.scene) do
        orb <- Ref.read orbitRef
        pc <- projAndCanvas s.zoom orb
        bs <- Ref.read builderRef
        let
          cursor = { x: px, y: py }
          -- Project every atom's scaled world position (through the orbit-aware
          -- view-projection) and measure pixel distance. Use builderWorldPosWith
          -- so the pick projection matches the render projection (parity).
          candidates =
            map
              ( \a ->
                  let
                    scr = Builder.projectToScreen pc.vp pc.canvas (builderWorldPosWith s.layerSpace a.pos)
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
              (Just (Left { id: c.id, ref: builderWorldPosWith s.layerSpace c.pos, whole: detail < 2 }))
              dragRef
          -- MISS: latch ORBIT mode, seeding the previous cursor pixel.
          _ -> Ref.write (Just (Right { x: px, y: py })) dragRef

    onMove px py = do
      mdrag <- Ref.read dragRef
      case mdrag of
        Nothing -> pure unit
        Just (Left pick) -> do
          s <- readState
          when (isBuilderLike s.scene) do
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
              -- Back out the effective world scale (builderScale × layerSpace) to
              -- recover the builder-model position. MUST match the render scale so
              -- pick and renderer stay in parity (no desync on non-default layerSpace).
              effectiveScale = builderScale * s.layerSpace
              modelPos =
                { x: world.x / effectiveScale
                , y: world.y / effectiveScale
                , z: world.z / effectiveScale
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
          when (isBuilderLike s.scene) do
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
