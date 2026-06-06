module Main where

import Prelude

import Data.Array (filter)
import Data.Foldable (for_)
import Data.Int (toNumber)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Number (cos, pi, sin, tan)
import Effect (Effect)
import Effect.Console (log)
import Effect.Ref as Ref
import FRP.Loop (Input, runLoop)
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
import Meshes as Meshes
import Orbital as O
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
    >>> applyElement input.element
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

-- Select the rendered element (atomic number) from the selector. Out-of-range
-- values are clamped downstream by Atom.elementOf.
applyElement :: Maybe Int -> State -> State
applyElement Nothing s = s
applyElement (Just z) s = s { element = z }

-- The backdrop (clear) color for the current scene.
clearColorFor :: Scene -> GL.Color
clearColorFor CubePoc = skyColor
clearColorFor Atomos = spaceColor

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
  when sceneChanged do
    Text.scrambleInto "scene-title" (sceneTitle s.scene)
    Text.setVisible "atom-label" inAtomos
    Text.setVisible "orbital-info" inAtomos
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

-- Degrees the atomos atom spins per frame (slow, so structure reads in 3D).
atomSpinDegreesPerFrame :: Number
atomSpinDegreesPerFrame = 0.4

-- Fixed tilt so the spin axis isn't edge-on to the camera.
atomTiltDegrees :: Number
atomTiltDegrees = 18.0

-- Frame-driven rotation of the whole atom (a slow Y spin under a fixed tilt).
atomSpin :: Number -> Matrix Number
atomSpin frame =
  M.multiply (V.rotateX atomTiltDegrees) (V.rotateY (frame * atomSpinDegreesPerFrame))

-- Compose the atom's auto-rotation onto an entity's base model matrix. Factored
-- as a per-atom transform so a future bonding phase can offset a second atom by
-- pre-multiplying a translation, without touching the pure model.
atomModel :: Number -> Matrix Number -> Matrix Number
atomModel frame base = M.multiply (atomSpin frame) base

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
      -- Atomos QM-orbital meshes: one solid shape mesh per real orbital (s, the
      -- three p, the five d), built ONCE at unit size and scaled per element at
      -- render time. Never rebuilt per frame.
      -- Two brightness variants per shape: dim (singly occupied) and bright
      -- (paired), so Hund's rule is visible.
      sOrb1 <- GL.createSolidMesh renderer (orbitalShape O.S orbitalSColor 1)
      sOrb2 <- GL.createSolidMesh renderer (orbitalShape O.S orbitalSColor 2)
      px1 <- GL.createSolidMesh renderer (orbitalShape O.Px orbitalPColor 1)
      px2 <- GL.createSolidMesh renderer (orbitalShape O.Px orbitalPColor 2)
      py1 <- GL.createSolidMesh renderer (orbitalShape O.Py orbitalPColor 1)
      py2 <- GL.createSolidMesh renderer (orbitalShape O.Py orbitalPColor 2)
      pz1 <- GL.createSolidMesh renderer (orbitalShape O.Pz orbitalPColor 1)
      pz2 <- GL.createSolidMesh renderer (orbitalShape O.Pz orbitalPColor 2)
      dz21 <- GL.createSolidMesh renderer (orbitalShape O.Dz2 orbitalDColor 1)
      dz22 <- GL.createSolidMesh renderer (orbitalShape O.Dz2 orbitalDColor 2)
      dxz1 <- GL.createSolidMesh renderer (orbitalShape O.Dxz orbitalDColor 1)
      dxz2 <- GL.createSolidMesh renderer (orbitalShape O.Dxz orbitalDColor 2)
      dyz1 <- GL.createSolidMesh renderer (orbitalShape O.Dyz orbitalDColor 1)
      dyz2 <- GL.createSolidMesh renderer (orbitalShape O.Dyz orbitalDColor 2)
      dx2y21 <- GL.createSolidMesh renderer (orbitalShape O.Dx2y2 orbitalDColor 1)
      dx2y22 <- GL.createSolidMesh renderer (orbitalShape O.Dx2y2 orbitalDColor 2)
      dxy1 <- GL.createSolidMesh renderer (orbitalShape O.Dxy orbitalDColor 1)
      dxy2 <- GL.createSolidMesh renderer (orbitalShape O.Dxy orbitalDColor 2)
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
                , modelMatrix: \st -> atomModel st.frame (M.translate n.pos.x n.pos.y n.pos.z)
                }
            )
            (Atom.nucleons (Atom.elementOf s.element))

        -- Pick the pre-built shape mesh for a real orbital, choosing the bright
        -- (paired, occ ≥ 2) or dim (singly occupied) variant.
        meshForShape sh occ =
          let
            paired = occ >= 2
          in
            case sh of
              O.S -> if paired then sOrb2 else sOrb1
              O.Px -> if paired then px2 else px1
              O.Py -> if paired then py2 else py1
              O.Pz -> if paired then pz2 else pz1
              O.Dz2 -> if paired then dz22 else dz21
              O.Dxz -> if paired then dxz2 else dxz1
              O.Dyz -> if paired then dyz2 else dyz1
              O.Dx2y2 -> if paired then dx2y22 else dx2y21
              O.Dxy -> if paired then dxy2 else dxy1

        -- One orbital lobe per OCCUPIED real orbital of the current element,
        -- concentric at the nucleus and scaled by its physical (Slater) radius.
        -- The p/d meshes are pre-oriented, so no per-element rotation is needed;
        -- occupancy (1 vs 2) selects the dim/bright variant (Hund visible).
        orbitalEntities :: State -> Array Entity
        orbitalEntities s =
          map
            ( \o ->
                let
                  r = O.rScale s.element o.n o.l
                in
                  { mesh: Solid (meshForShape o.kind o.occ)
                  , modelMatrix: \st -> atomModel st.frame (M.scale r r r)
                  }
            )
            (filter (\o -> o.occ > 0) (O.orbitalsFor s.element))

        entitiesFor :: State -> Array Entity
        entitiesFor s = case s.scene of
          CubePoc -> cubeEntities
          Atomos -> starEntities <> orbitalEntities s <> nucleusEntities s
      updateViewport renderer canvas
      w0 <- getCanvasWidth canvas
      h0 <- getCanvasHeight canvas
      sizeRef <- Ref.new { w: w0, h: h0 }
      overlayRef <- Ref.new { scene: initialState.scene, element: initialState.element }
      runLoop
        { initial: initialState
        , step
        , draw: \s -> do
            updateOverlay overlayRef s
            w <- getCanvasWidth canvas
            h <- getCanvasHeight canvas
            prev <- Ref.read sizeRef
            when (prev.w /= w || prev.h /= h) do
              Ref.write { w, h } sizeRef
              updateViewport renderer canvas
            GL.setClearColor renderer (clearColorFor s.scene)
            GL.beginFrame renderer
            for_ (entitiesFor s) \e ->
              case e.mesh of
                Solid m -> GL.drawSolidMesh renderer m (M.toVector (e.modelMatrix s))
                Wire m -> GL.drawMesh renderer m (M.toVector (e.modelMatrix s))
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

-- ───── Orbital lobe meshes (atomos QM visualization) ──────────────────

-- Latitude/longitude resolution of each orbital balloon surface.
orbitalRes :: Int
orbitalRes = 18

-- A unit-size orbital shape mesh (scaled per element at render time), tinted by
-- sub-shell type (s/p/d) and dimmed by occupancy (singly-occupied orbitals are
-- darker than paired ones, so Hund's rule reads visually).
orbitalShape :: O.OrbShape -> GL.Color -> Int -> Meshes.SolidSpec
orbitalShape shape color occ =
  (Meshes.orbitalMesh orbitalRes orbitalRes 1.0 shape) { color = dim (O.occBrightness occ) color }

-- Scale a colour's RGB by a brightness factor (alpha unchanged).
dim :: Number -> GL.Color -> GL.Color
dim f c = { r: c.r * f, g: c.g * f, b: c.b * f, a: c.a }

-- s orbitals (cyan), p orbitals (blue), d orbitals (violet).
orbitalSColor :: GL.Color
orbitalSColor = { r: 0.35, g: 0.80, b: 0.85, a: 1.0 }

orbitalPColor :: GL.Color
orbitalPColor = { r: 0.40, g: 0.55, b: 1.0, a: 1.0 }

orbitalDColor :: GL.Color
orbitalDColor = { r: 0.70, g: 0.45, b: 0.95, a: 1.0 }
