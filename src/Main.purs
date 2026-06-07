module Main where

import Prelude

import Data.Array (concat, concatMap, length, take, zipWith)
import Data.Foldable (for_, minimumBy)
import Data.Int (toNumber)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Number (cos, pi, sin, sqrt, tan)
import Data.Traversable (traverse)
import Effect (Effect)
import Effect.Console (log)
import Effect.Ref as Ref
import FRP.Loop (Input, installCanvasPointer, runLoop)
import Graphics.Canvas
  ( CanvasElement
  , getCanvasElementById
  , getCanvasHeight
  , getCanvasWidth
  )
import Graphics.GL (Mesh, Renderer, SolidMesh)
import Graphics.GL as GL
import Math.Matrix (Matrix)
import Math.Matrix as M
import Atom (Nucleon(..))
import Atom as Atom
import Builder as Builder
import BuilderApi (installBuilderApi, installBuilderControls)
import Controls as Controls
import Meshes as Meshes
import Molecule as Molecule
import Palette (subshellColor)
import Scene (Scene(..), nextScene, sceneTitle, spaceColor)
import Starfield (starPositions)
import Text as Text
import Vector as V
import World (groundTransform, gridTransform, groundExtent, gridDivisions, skyColor)

type State =
  { transform :: Matrix Number
  , speed :: Number
  , mouseLast :: Maybe { x :: Int, y :: Int }
  , frame :: Number
  , scene :: Scene
  , element :: Int
  , view2D :: Boolean
  , bondProgress :: Number
  , builder :: Builder.BuilderState
  }

-- A renderable mesh is either a solid lit mesh or a wireframe mesh; the draw
-- loop dispatches on the constructor (exhaustively).
data EntityMesh
  = Solid SolidMesh
  | Wire Mesh

type Entity =
  { mesh :: EntityMesh
  , modelMatrix :: State -> Matrix Number
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

-- Vertical field of view, in radians.
fov :: Number
fov = pi / 3.0

-- Distance from the camera to the world origin along -Z.
cameraDistance :: Number
cameraDistance = 1000.0

clipNear :: Number
clipNear = 1.0

clipFar :: Number
clipFar = 2000.0

-- Satellite orbits the world origin on the XZ plane.
satelliteOrbitRadius :: Number
satelliteOrbitRadius = 200.0

-- One full orbit in 5 seconds at 60 fps = 1.2 deg/frame.
satelliteDegreesPerFrame :: Number
satelliteDegreesPerFrame = 1.2

initialState :: State
initialState =
  { transform: M.identity
  , speed: 0.0
  , mouseLast: Nothing
  , frame: 0.0
  , scene: CubePoc
  , element: 6 -- Carbon by default
  , view2D: false
  -- 1.0 = bonded resting state (nuclei at full separation, in the outer thirds).
  -- The bond animation sweeps this 1→0→1, drawing the atoms together and back.
  , bondProgress: 1.0
  , builder: Builder.emptyBuilder
  }

-- Perspective projection composed with a camera-distance translation.
perspectiveProjection :: Number -> Number -> Matrix Number
perspectiveProjection w h =
  let
    aspect = w / h
    f = 1.0 / tan (fov / 2.0)
    p = fromMaybe (M.zeros 4 4) $ M.fromArray 4 4
      [ f / aspect
      , 0.0
      , 0.0
      , 0.0
      , 0.0
      , f
      , 0.0
      , 0.0
      , 0.0
      , 0.0
      , (clipFar + clipNear) / (clipNear - clipFar)
      , (2.0 * clipFar * clipNear) / (clipNear - clipFar)
      , 0.0
      , 0.0
      , -1.0
      , 0.0
      ]
  in
    M.multiply p (M.translate 0.0 0.0 (-cameraDistance))

step :: Input -> State -> State
step input =
  applyToggle input.toggleScene
    >>> applyToggle2D input.toggle2D
    >>> applyElement input.element
    >>> applyBondProgress input.bondProgress
    >>> applyShear input.shear
    >>> applyKey input.lastKey
    >>> applyMouse input.mouse
    >>> tickFrame
  where
  tickFrame s = s { frame = s.frame + 1.0 }

-- Flip between the cube POC and atomos when the scene switch is clicked.
applyToggle :: Boolean -> State -> State
applyToggle false s = s
applyToggle true s = s { scene = nextScene s.scene }

-- Flip the 2D/3D view of the atom when the "2D" checkbox changes.
applyToggle2D :: Boolean -> State -> State
applyToggle2D false s = s
applyToggle2D true s = s { view2D = not s.view2D }

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

updateViewport :: Renderer -> CanvasElement -> Effect Unit
updateViewport renderer canvas = do
  w <- getCanvasWidth canvas
  h <- getCanvasHeight canvas
  GL.resizeRenderer renderer { width: w, height: h }
  GL.setProjection renderer (M.toVector (perspectiveProjection w h))

main :: Effect Unit
main = do
  mcanvas <- getCanvasElementById "canvas"
  case mcanvas of
    Nothing -> log "Main: canvas element with id 'canvas' not found; aborting init"
    Just canvas -> do
      renderer <- GL.initRenderer canvas
      -- Cube POC scene meshes.
      groundMesh <- GL.createSolidMesh renderer (Meshes.groundPlane groundExtent)
      gridMesh <- GL.createWireframeMesh renderer (Meshes.gridFloor groundExtent gridDivisions)
      mainMesh <- GL.createSolidMesh renderer Meshes.solidMainCube
      satMesh <- GL.createSolidMesh renderer Meshes.solidSatelliteCube
      -- Atomos scene meshes: shared spheres reused (with different model
      -- matrices) across all stars / protons / neutrons.
      starMesh <- GL.createSolidMesh renderer starSphere
      protonMesh <- GL.createSolidMesh renderer protonSphere
      neutronMesh <- GL.createSolidMesh renderer neutronSphere
      -- One thin orbital ring line + one electron sphere per sub-shell, created
      -- ONCE for the maximal element (Krypton); a smaller element uses an exact
      -- prefix. Both are coloured by sub-shell (shell hue, lighter outward), so a
      -- sub-shell's ring and its electrons share one colour.
      ringMeshes <- traverse
        ( \ss -> GL.createWireframeMesh renderer
            ( (Meshes.orbitRing ringSegments (Atom.subshellRadius ss.n ss.l) (Atom.subshellInclination ss.n ss.l))
                { color = subshellColor ss.n ss.l }
            )
        )
        (Atom.fillSubshells Atom.maxElectron)
      -- Flattened (2D) counterpart of ringMeshes: concentric, un-inclined rings.
      -- Built in the same Madelung order so the prefix `take` + the electron-mesh
      -- zip stay aligned with the 3D rings.
      ringMeshes2D <- traverse
        ( \ss -> GL.createWireframeMesh renderer
            ( (Meshes.orbitRingFlat ringSegments (Atom.subshellRadius ss.n ss.l))
                { color = subshellColor ss.n ss.l }
            )
        )
        (Atom.fillSubshells Atom.maxElectron)
      electronMeshes <- traverse
        (\ss -> GL.createSolidMesh renderer (electronSphere (subshellColor ss.n ss.l)))
        (Atom.fillSubshells Atom.maxElectron)
      -- Molecule scene: one dedicated bright electron mesh for the shared
      -- bonding pair (created ONCE, reused per shared electron position). A
      -- touch larger than atomos electrons so the central pair reads clearly
      -- between the two flanking nuclei.
      molElectronMesh <- GL.createSolidMesh renderer moleculeElectronSphere
      -- Dedicated, larger nucleus sphere for the H₂ nuclei: bigger than an atomos
      -- proton so each nucleus carries enough lit mass to read clearly in the
      -- left/right thirds and to relocate a substantial block of pixels when the
      -- bond animation draws the two atoms together.
      molNucleusMesh <- GL.createSolidMesh renderer moleculeNucleusSphere
      -- Small proton/neutron spheres for the Builder nucleus, so a multi-nucleon
      -- nucleus reads as a SMALL tight cluster (not a big loose cloud) with the
      -- electrons clearly orbiting outside it.
      builderProtonMesh <- GL.createSolidMesh renderer builderProtonSphere
      builderNeutronMesh <- GL.createSolidMesh renderer builderNeutronSphere
      let
        cubeEntities :: Array Entity
        cubeEntities =
          [ { mesh: Solid groundMesh, modelMatrix: \_ -> groundTransform }
          , { mesh: Wire gridMesh, modelMatrix: \_ -> gridTransform }
          , { mesh: Solid mainMesh, modelMatrix: _.transform }
          , { mesh: Solid satMesh, modelMatrix: satelliteTransform }
          ]

        starEntities :: Array Entity
        starEntities =
          map
            (\p -> { mesh: Solid starMesh, modelMatrix: \_ -> M.translate p.x p.y p.z })
            starPositions

        -- Nucleus is rebuilt from the current element each frame (cheap, pure);
        -- proton/neutron meshes are shared.
        nucleusEntities :: State -> Array Entity
        nucleusEntities s =
          map
            ( \n ->
                { mesh: Solid (if n.kind == Proton then protonMesh else neutronMesh)
                , modelMatrix: \_ ->
                    if s.view2D then M.translate n.pos.x n.pos.y 0.0
                    else M.translate n.pos.x n.pos.y n.pos.z
                }
            )
            (Atom.nucleons (Atom.elementOf s.element))

        -- One thin ring per filled sub-shell of the current element: the first
        -- `k` shared ring meshes (Madelung prefix). Rings are world-centered.
        ringEntities :: State -> Array Entity
        ringEntities s =
          map (\m -> { mesh: Wire m, modelMatrix: \_ -> M.identity })
            ( take (length (Atom.fillSubshells (Atom.elementOf s.element).electrons))
                (if s.view2D then ringMeshes2D else ringMeshes)
            )

        -- Discrete electrons orbiting on the rings; positions advance with frame.
        -- Each sub-shell's electrons use that sub-shell's colour mesh (matching
        -- its ring), zipping the per-sub-shell meshes with the grouped positions.
        electronEntities :: State -> Array Entity
        electronEntities s =
          concat
            ( zipWith
                ( \mesh group ->
                    map (\p -> { mesh: Solid mesh, modelMatrix: \_ -> M.translate p.x p.y p.z }) group
                )
                electronMeshes
                ( if s.view2D then Atom.electronPositionsBySubshell2D (Atom.elementOf s.element) s.frame
                  else Atom.electronPositionsBySubshell (Atom.elementOf s.element) s.frame
                )
            )

        -- Molecule scene: H₂ nuclei reuse the shared protonSphere mesh, one
        -- Solid proton per nucleon of the H₂ molecule (entry 0). Static. The
        -- whole molecule is scaled up about the origin so the two nuclei spread
        -- well apart on-screen (left/right thirds), framing the shared pair.
        moleculeNucleusEntities :: State -> Array Entity
        moleculeNucleusEntities _ =
          map
            ( \n ->
                { mesh: Solid molNucleusMesh
                , modelMatrix: \s -> moleculePlace s.bondProgress n.pos
                }
            )
            (Molecule.moleculeNucleons (Molecule.moleculeOf 0))

        -- The shared (bonding) electron pair sitting between the two nuclei;
        -- positions breathe with the frame. Reuses the single molElectronMesh.
        moleculeElectronEntities :: State -> Array Entity
        moleculeElectronEntities s =
          map
            ( \p ->
                { mesh: Solid molElectronMesh
                , modelMatrix: \st -> moleculePlace st.bondProgress p
                }
            )
            (Molecule.sharedElectronPositions (Molecule.moleculeOf 0) s.frame)

        -- Builder scene: the REAL per-element nucleus for each placed atom,
        -- instanced over the live builder model (s.builder, refreshed from the
        -- shared API Ref each frame). For every placed atom we expand its element
        -- into its nucleon cluster (Atom.nucleons ∘ Atom.elementOf), drawing one
        -- Solid protonMesh / neutronMesh per nucleon — exactly as the atomos
        -- nucleusEntities / Molecule moleculeNucleusEntities do — each nucleon
        -- offset by the atom centre then placed with builderPlace. So Carbon
        -- (6 p + 6 n) renders a denser/larger nucleus than Hydrogen (1 p). Meshes
        -- are created ONCE at init (protonMesh / neutronMesh, reused from atomos);
        -- only model matrices vary, so there is no per-frame mesh allocation.
        -- Positions are scaled about the origin (builderScale) so atoms placed in
        -- the ±60..±180 range read clearly on-screen.
        builderAtomEntities :: State -> Array Entity
        builderAtomEntities s =
          concatMap
            ( \a ->
                map
                  ( \n ->
                      { mesh: Solid (if n.kind == Proton then builderProtonMesh else builderNeutronMesh)
                      -- The nucleon OFFSET is compressed (builderNucleusCompress) so
                      -- the nucleus stays a small tight clump around the scaled atom
                      -- centre, while the atom centres + electron orbits keep the full
                      -- builderScale spacing.
                      , modelMatrix: \_ ->
                          builderPlace
                            { x: a.pos.x + n.pos.x * builderNucleusCompress
                            , y: a.pos.y + n.pos.y * builderNucleusCompress
                            , z: a.pos.z + n.pos.z * builderNucleusCompress
                            }
                      }
                  )
                  (Atom.nucleons (Atom.elementOf a.z))
            )
            s.builder.atoms

        -- Lone (non-bonding) electrons: one bright sphere per lone electron, on a
        -- small ring around each atom's centre (Builder.loneElectronPositions). A
        -- fully-bonded atom has lone count 0, so NOTHING floats above it — only the
        -- shared bond pair remains. Reuses the single molElectronMesh.
        builderLoneElectronEntities :: State -> Array Entity
        builderLoneElectronEntities s =
          map
            ( \p ->
                { mesh: Solid molElectronMesh
                , modelMatrix: \_ -> builderPlace p
                }
            )
            (Builder.loneElectronPositions s.builder s.frame)

        -- Shared (bonding) electrons: the pair sitting BETWEEN each bond's two
        -- nuclei (Builder.bondElectronPositions), breathing with the frame. Reuses
        -- the single molElectronMesh.
        builderBondElectronEntities :: State -> Array Entity
        builderBondElectronEntities s =
          map
            ( \p ->
                { mesh: Solid molElectronMesh
                , modelMatrix: \_ -> builderPlace p
                }
            )
            (Builder.bondElectronPositions s.builder s.frame)

        entitiesFor :: State -> Array Entity
        entitiesFor s = case s.scene of
          CubePoc -> cubeEntities
          Atomos -> starEntities <> ringEntities s <> nucleusEntities s <> electronEntities s
          Molecule -> starEntities <> moleculeNucleusEntities s <> moleculeElectronEntities s
          Builder ->
            starEntities
              <> builderAtomEntities s
              <> builderLoneElectronEntities s
              <> builderBondElectronEntities s
      updateViewport renderer canvas
      w0 <- getCanvasWidth canvas
      h0 <- getCanvasHeight canvas
      sizeRef <- Ref.new { w: w0, h: h0 }
      overlayRef <- Ref.new { scene: initialState.scene, element: initialState.element }
      -- Holds the most recent rendered State, so a builder mutation arriving
      -- off-loop (via window.__builder / the Add/Clear buttons) can re-render the
      -- current scene IMMEDIATELY with the updated model, rather than waiting for
      -- the next rAF frame. With preserveDrawingBuffer this makes the new geometry
      -- visible to a pixel read taken right after the mutation (deterministic E2E).
      lastStateRef <- Ref.new initialState
      let
        -- Clear + draw one frame of the given State. Reused by the rAF loop and by
        -- the eager re-render after a builder mutation.
        renderFrame :: State -> Effect Unit
        renderFrame s = do
          GL.setClearColor renderer (clearColorFor s.scene)
          GL.beginFrame renderer
          for_ (entitiesFor s) \e ->
            case e.mesh of
              Solid m -> GL.drawSolidMesh renderer m (M.toVector (e.modelMatrix s))
              Wire m -> GL.drawMesh renderer m (M.toVector (e.modelMatrix s))
      -- The builder model lives in a single Ref shared by the window.__builder
      -- automation API, the in-app Add/Clear buttons, and the pointer pick+drag;
      -- the render loop reads its live snapshot into State each frame so a mutation
      -- through any path is reflected immediately and consistently. On each
      -- mutation we eagerly re-render the current scene so the change is on-screen
      -- without a frame delay (deterministic for pixel-read E2E).
      let
        eagerRender :: Builder.BuilderState -> Effect Unit
        eagerRender bs = do
          s <- Ref.read lastStateRef
          when (s.scene == Builder) (renderFrame (s { builder = bs }))
      builderRef <- installBuilderApi eagerRender
      installBuilderControls eagerRender builderRef
      -- Production pointer pick+drag: scene-gated to Builder, routed through the
      -- SAME shared Ref + eagerRender as the API, so there is one source of truth
      -- and the drag is on-screen immediately. Reads the live canvas size to build
      -- the exact projection the renderer uses.
      installBuilderPick canvas builderRef eagerRender (Ref.read lastStateRef)
      -- Animate the glassy control bar IN on load (anime.js entrance: opacity
      -- 0→1 + slide, children staggered) and wire tasteful Add/Clear click pulses.
      -- DOM only via the Controls FFI — never touches WebGL.
      Controls.animateControlBarIn "controls"
      Controls.installButtonPulse "add-btn"
      Controls.installButtonPulse "clear-btn"
      runLoop
        { initial: initialState
        , step
        , draw: \s0 -> do
            -- Pull the live builder snapshot into the state used for rendering.
            bs <- Ref.read builderRef
            let s = s0 { builder = bs }
            Ref.write s lastStateRef
            updateOverlay overlayRef s
            w <- getCanvasWidth canvas
            h <- getCanvasHeight canvas
            prev <- Ref.read sizeRef
            when (prev.w /= w || prev.h /= h) do
              Ref.write { w, h } sizeRef
              updateViewport renderer canvas
            renderFrame s
        }

-- A single small star sphere, reused (with different model matrices) for every
-- point in the starfield.
starSphere :: Meshes.SolidSpec
starSphere = (Meshes.sphere 8 8 10.0) { color = { r: 0.95, g: 0.95, b: 1.0, a: 1.0 } }

-- Nucleon spheres: red protons, gray neutrons (shared across the nucleus).
protonSphere :: Meshes.SolidSpec
protonSphere = (Meshes.sphere 14 14 Atom.nucleonRadius) { color = { r: 0.90, g: 0.25, b: 0.22, a: 1.0 } }

neutronSphere :: Meshes.SolidSpec
neutronSphere = (Meshes.sphere 14 14 Atom.nucleonRadius) { color = { r: 0.62, g: 0.64, b: 0.67, a: 1.0 } }

-- Builder nucleus: small proton/neutron spheres + a compressed cluster, so a
-- multi-nucleon nucleus reads as a SMALL tight clump with electrons orbiting
-- clearly outside it (rather than a big loose cloud of full-size nucleons).
builderNucleonRadius :: Number
builderNucleonRadius = Atom.nucleonRadius * 0.72

builderNucleusCompress :: Number
builderNucleusCompress = 0.4

builderProtonSphere :: Meshes.SolidSpec
builderProtonSphere = (Meshes.sphere 12 12 builderNucleonRadius) { color = { r: 0.90, g: 0.25, b: 0.22, a: 1.0 } }

builderNeutronSphere :: Meshes.SolidSpec
builderNeutronSphere = (Meshes.sphere 12 12 builderNucleonRadius) { color = { r: 0.62, g: 0.64, b: 0.67, a: 1.0 } }

-- A discrete electron sphere in the given (sub-shell) colour, instanced at every
-- electron position on that sub-shell's ring.
electronSphere :: GL.Color -> Meshes.SolidSpec
electronSphere color = (Meshes.sphere 12 12 Atom.electronRadius) { color = color }

-- The H₂ nucleus sphere: the same proton red as atomos but noticeably larger, so
-- each nucleus reads clearly in its third and relocates a substantial block of
-- pixels when the bond animation draws the two atoms together.
moleculeNucleusSphere :: Meshes.SolidSpec
moleculeNucleusSphere =
  (Meshes.sphere 16 16 (Atom.nucleonRadius * 1.7)) { color = { r: 0.90, g: 0.25, b: 0.22, a: 1.0 } }

-- Electron colour: a clear sky-blue, distinct from the red proton and grey
-- neutron so electrons never read as nucleons.
moleculeElectronColor :: GL.Color
moleculeElectronColor = { r: 0.30, g: 0.68, b: 1.0, a: 1.0 }

-- The shared bonding-pair / builder electron sphere: a small blue dot — clearly
-- SMALLER than a nucleon (nucleonRadius) so an electron never looks like a proton.
moleculeElectronSphere :: Meshes.SolidSpec
moleculeElectronSphere =
  (Meshes.sphere 14 14 (Atom.electronRadius * 1.3)) { color = moleculeElectronColor }

-- The molecule is scaled up about the origin so its two nuclei spread well
-- apart on-screen (the bond runs across the view), keeping the shared pair
-- clearly framed between them.
moleculeScale :: Number
moleculeScale = 4.5

-- Place a molecule particle: scale its position about the origin, then
-- translate. (Particle meshes keep their own radius; only positions scale.)
-- The internuclear separation (the x axis, along which the two nuclei + shared
-- pair are laid out) interpolates with `bondProgress`: at 1.0 the atoms sit at
-- full separation (resting H₂), and as it sweeps toward 0.0 they draw together
-- toward the bond midpoint (x → 0), coalescing the molecule.
moleculePlace :: Number -> Atom.V3 -> Matrix Number
moleculePlace bondProgress p =
  M.translate
    (p.x * moleculeScale * bondProgress)
    (p.y * moleculeScale)
    (p.z * moleculeScale)

-- Builder positions are placed in scene units in roughly the ±60..±180 range
-- (the test addAtom calls and the in-app spawn stepping). Scale them up about
-- the origin so the placed atoms read clearly on-screen, centred on the view.
builderScale :: Number
builderScale = 2.2

-- Place a builder particle: scale its world position about the origin.
builderPlace :: Atom.V3 -> Matrix Number
builderPlace p =
  M.translate (p.x * builderScale) (p.y * builderScale) (p.z * builderScale)

-- The world-space point the renderer projects for a builder atom: its model-pos
-- scaled about the origin by builderScale (matching builderPlace's translation).
builderWorldPos :: Atom.V3 -> Atom.V3
builderWorldPos p =
  { x: p.x * builderScale, y: p.y * builderScale, z: p.z * builderScale }

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
  -> (Builder.BuilderState -> Effect Unit)
  -> Effect State
  -> Effect Unit
installBuilderPick canvas builderRef eagerRender readState = do
  pickRef <- Ref.new (Nothing :: Maybe { id :: Int, depth :: Number })
  let
    -- Current full projection + canvas dims (live size).
    projAndCanvas = do
      w <- getCanvasWidth canvas
      h <- getCanvasHeight canvas
      pure { proj: perspectiveProjection w h, canvas: { w, h } }

    onDown px py = do
      s <- readState
      when (s.scene == Builder) do
        pc <- projAndCanvas
        bs <- Ref.read builderRef
        let
          cursor = { x: px, y: py }
          -- Project every atom's scaled world position and measure pixel distance.
          candidates =
            map
              ( \a ->
                  let
                    scr = Builder.projectToScreen pc.proj pc.canvas (builderWorldPos a.pos)
                    dx = scr.x - cursor.x
                    dy = scr.y - cursor.y
                  in
                    { id: a.id, pos: a.pos, dist: sqrt (dx * dx + dy * dy) }
              )
              bs.atoms
          nearest = minimumBy (comparing _.dist) candidates
        case nearest of
          Just c | c.dist <= pickRadius ->
            Ref.write (Just { id: c.id, depth: (builderWorldPos c.pos).z }) pickRef
          _ -> pure unit

    onMove px py = do
      mpick <- Ref.read pickRef
      case mpick of
        Nothing -> pure unit
        Just pick -> do
          s <- readState
          when (s.scene == Builder) do
            pc <- projAndCanvas
            let
              -- Reference world point at the picked atom's depth (z preserved) so
              -- unprojectAtDepth lands the cursor on that camera-facing plane.
              ref = { x: 0.0, y: 0.0, z: pick.depth }
              world = Builder.unprojectAtDepth pc.proj pc.canvas { x: px, y: py } ref
              -- Back out builderScale to recover the builder-model position.
              modelPos =
                { x: world.x / builderScale
                , y: world.y / builderScale
                , z: world.z / builderScale
                }
            Ref.modify_ (Builder.moveAtom pick.id modelPos) builderRef
            bs <- Ref.read builderRef
            eagerRender bs

    onUp = Ref.write Nothing pickRef
  installCanvasPointer onDown onMove onUp
