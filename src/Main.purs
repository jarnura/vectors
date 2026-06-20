module Main
  ( main
  , module ReexportUpdate
  ) where

import Prelude

import Atom (Nucleon(..))
import Atom as Atom
import Builder as Builder
import BuilderApi (installBuilderApi, installBuilderControls)
import Camera as Camera
import Controls as Controls
import Data.Array (concat, concatMap, length, take, zipWith)
import Data.Foldable (for_)
import Data.Maybe (Maybe(..))
import Data.Traversable (traverse)
import Effect (Effect)
import Effect.Console (log)
import Effect.Ref as Ref
import FRP.Loop (installOrbitButtons, runLoop, setBuilderDetail)
import Graphics.Canvas (getCanvasElementById, getCanvasHeight, getCanvasWidth)
import Graphics.GL as GL
import Main.Builder
  ( clearColorFor
  , installBuilderPick
  , ringSegments
  , satelliteTransform
  , syncAtomLabels
  , updateOverlay
  , updateViewport
  )
import Math.Matrix as M
import Meshes as Meshes
import Molecule as Molecule
import OrbitApi (installOrbitApi)
import Palette (shellColor, subshellColor)
import Scene (Scene(..))
import Scene.Entities
  ( Entity
  , EntityMesh(..)
  , builderBallPlace
  , builderBallSphere
  , builderBondLinePlace
  , builderDetailPlace
  , builderElectronGroupEntities
  , builderNeutronSphere
  , builderNucleusCompress
  , builderProtonSphere
  , builderValenceElectronSphere
  , builderWorldPos
  , electronSphere
  , moleculeElectronSphere
  , moleculeNucleusSphere
  , moleculePlace
  , neutronSphere
  , protonSphere
  , starSphere
  )
import Starfield (starPositions)
import Update (State, applyOrbit, initialState, step)
import Update
  ( applyDragStrength
  , applyOrbit
  , applySubshellView
  , applyValenceOnly
  , initialState
  ) as ReexportUpdate
import World (groundTransform, gridTransform, groundExtent, gridDivisions)

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
      -- SHELL-only (Bohr) ring set: one ring per occupied PRINCIPAL shell of the
      -- maximal element (Krypton → 4 shells), built ONCE in parallel to the
      -- sub-shell ringMeshes. A smaller element uses an exact prefix `take`. Each
      -- ring is coloured by its shell (shellColor n) and tilted like the shell's
      -- ℓ=0 sub-shell so the ring traces the collapsed electron path. The 2D
      -- counterpart is flat (concentric) for the Bohr-diagram view.
      shellRingMeshes <- traverse
        ( \sh -> GL.createWireframeMesh renderer
            ( (Meshes.orbitRing ringSegments sh.radius (Atom.subshellInclination sh.n 0))
                { color = shellColor sh.n }
            )
        )
        (Atom.shellRings (Atom.elementOf Atom.maxElectron))
      shellRingMeshes2D <- traverse
        ( \sh -> GL.createWireframeMesh renderer
            ( (Meshes.orbitRingFlat ringSegments sh.radius)
                { color = shellColor sh.n }
            )
        )
        (Atom.shellRings (Atom.elementOf Atom.maxElectron))
      shellElectronMeshes <- traverse
        (\sh -> GL.createSolidMesh renderer (electronSphere (shellColor sh.n)))
        (Atom.shellRings (Atom.elementOf Atom.maxElectron))
      -- Molecule scene: one dedicated bright electron mesh for the shared
      -- bonding pair (created ONCE, reused per shared electron position). A
      -- touch larger than atomos electrons so the central pair reads clearly
      -- between the two flanking nuclei.
      molElectronMesh <- GL.createSolidMesh renderer moleculeElectronSphere
      -- Distinct VALENCE-electron mesh for the Builder: outermost-shell lone
      -- electrons + bonding/shared electrons use an amber/gold colour, clearly
      -- separated from the core blue, proton red, and neutron grey. Created ONCE
      -- and reused for both valence-lone and bond electrons.
      builderValenceElectronMesh <- GL.createSolidMesh renderer builderValenceElectronSphere
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
      -- Builder LOD atom-BALL mesh: a single solid sphere reused for every placed
      -- atom in the zoomed-OUT layer. Created ONCE; only its model matrix varies
      -- per atom/frame (scaled by 1−detail about each atom centre), so the ball is
      -- full size when zoomed out and vanishes as the sub-atomic detail blooms in.
      builderBallMesh <- GL.createSolidMesh renderer builderBallSphere
      -- Builder atomic-layer BOND LINE mesh: ONE thin unit beam along +Y
      -- (0,0,0)→(0,1,0) with a small baked half-width, reused for every bond.
      -- Only its model matrix varies per bond/frame (stretched + oriented between
      -- the two bonded atom centres), so there is no per-frame GL allocation. A
      -- thin solid beam (not a 1px GL_LINES segment) so the bond reliably lights
      -- pixels along its whole length, including the midpoint.
      bondLineMesh <- GL.createSolidMesh renderer Meshes.unitBeam
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

        -- Orbital ring lines for the current element, world-centered. In SUB-SHELL
        -- view (default) one ring per filled sub-shell (Madelung prefix of the
        -- shared sub-shell meshes); in SHELL-only (Bohr) view one ring per occupied
        -- principal shell (prefix of the shell meshes). Each composes with s.view2D:
        -- the flat (2D) mesh set is chosen for the camera-facing concentric circles.
        ringEntities :: State -> Array Entity
        ringEntities s =
          map (\m -> { mesh: Wire m, modelMatrix: \_ -> M.identity })
            ( if s.subshellView then
                take (length (Atom.fillSubshells (Atom.elementOf s.element).electrons))
                  (if s.view2D then ringMeshes2D else ringMeshes)
              else
                take (length (Atom.shellRings (Atom.elementOf s.element)))
                  (if s.view2D then shellRingMeshes2D else shellRingMeshes)
            )

        -- Discrete electrons orbiting on the rings; positions advance with frame.
        -- Each ring's electrons use that ring's colour mesh (matching its ring),
        -- zipping the per-ring meshes with the grouped positions. The grouping +
        -- mesh set switch on s.subshellView (sub-shell vs shell-collapsed) and the
        -- flat/inclined position helper switches on s.view2D — so the electron
        -- helper always matches the ring set (3D helper ↔ 3D rings, 2D ↔ flat).
        electronEntities :: State -> Array Entity
        electronEntities s =
          concat
            ( zipWith
                ( \mesh group ->
                    map (\p -> { mesh: Solid mesh, modelMatrix: \_ -> M.translate p.x p.y p.z }) group
                )
                (if s.subshellView then electronMeshes else shellElectronMeshes)
                ( if s.subshellView then
                    ( if s.view2D then Atom.electronPositionsBySubshell2D (Atom.elementOf s.element) s.frame
                      else Atom.electronPositionsBySubshell (Atom.elementOf s.element) s.frame
                    )
                  else
                    ( if s.view2D then Atom.electronPositionsByShell2D (Atom.elementOf s.element) s.frame
                      else Atom.electronPositionsByShell (Atom.elementOf s.element) s.frame
                    )
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
                      -- builderScale spacing. The whole nucleus BLOOMS out of the atom
                      -- centre with detail (d): at d→0 each nucleon collapses into the
                      -- centre (and shrinks to ~0), at d→1 it reaches its full offset.
                      , modelMatrix: \st ->
                          builderDetailPlace st.detail a.pos
                            { x: a.pos.x + n.pos.x * builderNucleusCompress
                            , y: a.pos.y + n.pos.y * builderNucleusCompress
                            , z: a.pos.z + n.pos.z * builderNucleusCompress
                            }
                      }
                  )
                  (Atom.nucleons (Atom.elementOf a.z))
            )
            s.builder.atoms

        -- Zoomed-OUT atom-BALL layer: one slate ball per placed atom at its
        -- centre, scaled by (1 − detail) so it is full size when zoomed out
        -- (detail→0) and vanishes as the sub-atomic detail blooms in (detail→1).
        -- The cross-fade complement of builderAtomEntities / the electron groups.
        -- Placed at builderWorldPos (builderScale) — the SAME single world
        -- coordinate system as the nucleus/electrons and the pointer pick/drag, so
        -- the ball position never desyncs from the pick projection.
        builderAtomBallEntities :: State -> Array Entity
        builderAtomBallEntities s =
          map
            ( \a ->
                { mesh: Solid builderBallMesh
                , modelMatrix: \st -> builderBallPlace st.detail (Atom.atomicRadius a.z) a.pos
                }
            )
            s.builder.atoms

        -- Atomic-layer BOND LINES: one bright unit-segment wire per bond,
        -- stretched between the two bonded atoms' scaled world positions. The
        -- segment fades out as detail rises (multiplying its length by
        -- 1−detail) so it shows in the zoomed-OUT ball layer and vanishes as
        -- the sub-atomic detail blooms in. Mesh built ONCE (bondLineMesh).
        builderBondLineEntities :: State -> Array Entity
        builderBondLineEntities s =
          map
            ( \seg ->
                { mesh: Solid bondLineMesh
                , modelMatrix: \st ->
                    builderBondLinePlace st.detail
                      (builderWorldPos seg.a)
                      (builderWorldPos seg.b)
                }
            )
            (Builder.bondSegments s.builder)

        -- CORE (inner-shell) lone electrons: one blue sphere per core lone
        -- electron, on the inner ring around each atom's centre
        -- (Builder.coreLoneElectronPositions). Reuses the single blue
        -- molElectronMesh — core electrons keep the existing colour.
        builderLoneElectronEntities :: State -> Array Entity
        builderLoneElectronEntities s =
          builderElectronGroupEntities molElectronMesh
            (Builder.coreLoneElectronGroups s.builder s.frame)

        -- VALENCE (outermost-shell) lone electrons: one amber sphere per valence
        -- lone electron, on the outer ring (Builder.valenceLoneElectronPositions).
        -- Uses the distinct builderValenceElectronMesh (amber/gold).
        builderValenceElectronEntities :: State -> Array Entity
        builderValenceElectronEntities s =
          builderElectronGroupEntities builderValenceElectronMesh
            (Builder.valenceLoneElectronGroups s.builder s.frame)

        -- Shared (bonding) electrons: the pair sitting BETWEEN each bond's two
        -- nuclei (Builder.bondElectronPositions), breathing with the frame.
        -- Bonding electrons ARE valence electrons → reuses the amber valence mesh.
        builderBondElectronEntities :: State -> Array Entity
        builderBondElectronEntities s =
          builderElectronGroupEntities builderValenceElectronMesh
            (Builder.bondElectronGroups s.builder s.frame)

        entitiesFor :: State -> Array Entity
        entitiesFor s = case s.scene of
          CubePoc -> cubeEntities
          Atomos -> starEntities <> ringEntities s <> nucleusEntities s <> electronEntities s
          Molecule -> starEntities <> moleculeNucleusEntities s <> moleculeElectronEntities s
          Builder ->
            builderAtomBallEntities s
              <> builderBondLineEntities s
              <> builderAtomEntities s
              <> (if s.valenceOnly then [] else builderLoneElectronEntities s)
              <> builderValenceElectronEntities s
              <> builderBondElectronEntities s
      updateViewport renderer canvas initialState
      w0 <- getCanvasWidth canvas
      h0 <- getCanvasHeight canvas
      sizeRef <- Ref.new
        { w: w0
        , h: h0
        , zoom: initialState.zoom
        , yaw: initialState.builderYaw
        , pitch: initialState.builderPitch
        }
      overlayRef <- Ref.new
        { scene: initialState.scene
        , element: initialState.element
        }
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
      -- The Builder camera orbit (yaw/pitch radians) lives in ONE shared Ref, the
      -- single source of truth read by the renderer (mirrored into State each
      -- frame, like builderRef), written by the empty-space orbit-drag, and
      -- read/written by window.__builder.setOrbit/getOrbit. It is camera state, so
      -- it stays OUT of the pure Builder model.
      orbitRef <- Ref.new { yaw: initialState.builderYaw, pitch: initialState.builderPitch }
      installOrbitApi orbitRef
      -- Production pointer pick+drag: scene-gated to Builder, routed through the
      -- SAME shared Ref + eagerRender as the API, so there is one source of truth
      -- and the drag is on-screen immediately. Reads the live canvas size to build
      -- the exact projection the renderer uses. An empty-space miss orbits the
      -- camera (writing the shared orbitRef) instead of moving an atom.
      installBuilderPick canvas builderRef orbitRef eagerRender (Ref.read lastStateRef) applyOrbit
      -- On-screen orbit buttons: each click folds a fixed Camera.buttonOrbitDelta
      -- step through Main.applyOrbit into the SAME shared orbitRef as the empty-
      -- space drag (no new Ref/clamp — applyOrbit already pitch-clamps), and
      -- #orbit-reset writes {yaw:0,pitch:0}. Builder-only: each handler reads the
      -- live State and only mutates when the Builder scene is active (mirrors
      -- installBuilderPick). The next rAF frame mirrors orbitRef → State and the
      -- sizeRef gate re-uploads the projection, so no eager re-render is needed.
      let
        orbitWhenBuilder :: ({ yaw :: Number, pitch :: Number } -> { yaw :: Number, pitch :: Number }) -> Effect Unit
        orbitWhenBuilder f = do
          s <- Ref.read lastStateRef
          when (s.scene == Builder) (Ref.modify_ f orbitRef)
        od = Camera.buttonOrbitDelta
      installOrbitButtons
        (orbitWhenBuilder (applyOrbit { dx: negate od, dy: 0.0 })) -- left
        (orbitWhenBuilder (applyOrbit { dx: od, dy: 0.0 })) -- right
        (orbitWhenBuilder (applyOrbit { dx: 0.0, dy: negate od })) -- up
        (orbitWhenBuilder (applyOrbit { dx: 0.0, dy: od })) -- down
        (orbitWhenBuilder (const { yaw: 0.0, pitch: 0.0 })) -- reset
      Controls.installButtonPulse "orbit-left"
      Controls.installButtonPulse "orbit-right"
      Controls.installButtonPulse "orbit-up"
      Controls.installButtonPulse "orbit-down"
      Controls.installButtonPulse "orbit-reset"
      -- Wire the glassy controls as a left DRAWER: the #panel-toggle icon slides
      -- the #controls panel IN from the left and OUT again (anime.js, closed
      -- drawer keeps pointer-events:none so it never blocks the canvas). The icon
      -- is the sole opener — the drawer no longer auto-opens on boot. Also wire a
      -- click pulse on the icon and tasteful Add/Clear pulses. DOM only via the
      -- Controls FFI — never touches WebGL.
      Controls.installPanelToggle "panel-toggle" "controls"
      Controls.installButtonPulse "panel-toggle"
      Controls.installButtonPulse "add-btn"
      Controls.installButtonPulse "clear-btn"
      runLoop
        { initial: initialState
        , step
        , draw: \s0 -> do
            -- Pull the live builder snapshot AND the live camera orbit into the
            -- state used for rendering (both are shared Refs, the single sources
            -- of truth for the model and the camera respectively).
            bs <- Ref.read builderRef
            orb <- Ref.read orbitRef
            let s = s0 { builder = bs, builderYaw = orb.yaw, builderPitch = orb.pitch }
            Ref.write s lastStateRef
            -- Publish the live eased Builder detail to window.__builderDetail every
            -- frame (deterministic E2E hook for the LOD cross-fade).
            setBuilderDetail s.detail
            updateOverlay overlayRef s
            w <- getCanvasWidth canvas
            h <- getCanvasHeight canvas
            prev <- Ref.read sizeRef
            -- Re-upload the GPU projection whenever size, zoom, OR the Builder
            -- orbit changes — without the yaw/pitch terms an orbit-only frame
            -- would skip the re-upload and the orbit would be invisible.
            when
              ( prev.w /= w || prev.h /= h || prev.zoom /= s.zoom
                  || prev.yaw /= s.builderYaw
                  || prev.pitch /= s.builderPitch
              )
              do
                Ref.write { w, h, zoom: s.zoom, yaw: s.builderYaw, pitch: s.builderPitch } sizeRef
                updateViewport renderer canvas s
            renderFrame s
            -- Sync the per-atom symbol overlay labels every frame so they follow
            -- dragged/zoomed atoms in Builder, and clear when leaving the scene.
            syncAtomLabels canvas s
        }
