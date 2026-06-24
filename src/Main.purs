module Main
  ( main
  , module ReexportUpdate
  ) where

import Prelude

import Atom (Nucleon(..))
import Atom as Atom
import Builder as Builder
import BuilderApi (installBuilderApi, installBuilderControls)
import Lattice as Lattice
import Camera as Camera
import Controls as Controls
import MaterialsCards (highlightCard, renderMaterialsCards, showMaterialsCards, showMaterialsPanel) as MaterialsCards
import Data.Array (concat, concatMap, length, range, take, zipWith)
import Data.Int (toNumber) as DI
import Data.Number (cos, pi, sin, sqrt)
import Data.Foldable (for_)
import Data.Maybe (Maybe(..))
import Data.Traversable (traverse)
import Effect (Effect)
import Effect.Console (log)
import Effect.Ref as Ref
import FRP.Loop (installNuclearControls, installOrbitButtons, runLoop, setBuilderDetail, setZoomSlider)
import Graphics.Canvas (getCanvasElementById, getCanvasHeight, getCanvasWidth)
import Graphics.GL as GL
import Main.Builder
  ( clearColorFor
  , installBuilderPick
  , isBuilderLike
  , renderPeOverlay
  , ringSegments
  , satelliteTransform
  , syncAtomLabels
  , updateOverlay
  , updateViewport
  )
import Math.Matrix as M
import Meshes as Meshes
import Molecule as Molecule
import Main.Nuclide (nuclideNucleusEntities, renderNuclideInfo, installNuclearReactions)
import NuclearApi (installNuclearApi)
import Nuclear (NuclearState, defaultNeutrons, addProton, removeProton, addNeutron, removeNeutron) as Nuclear
import OrbitApi (installOrbitApi)
import Palette (shellColor, subshellColor)
import Scene (Scene(..))
import Scene.Entities
  ( Entity
  , EntityMesh(..)
  , builderBallPlaceWith
  , builderBallSphere
  , builderBondLinePlaceWith
  , builderDetailPlaceWith
  , builderElectronGroupEntitiesWith
  , builderNeutronSphere
  , builderNucleusCompress
  , builderProtonSphere
  , builderScale
  , builderValenceElectronSphere
  , builderWorldPosWith
  , electronSphere
  , moleculeElectronSphere
  , moleculeNucleusSphere
  , moleculePlace
  , neutronSphere
  , protonSphere
  , starSphere
  )
import Builder.Vibration as Vibration
import Starfield (starPositions)
import Update (State, applyOrbit, initialState, step)
import Update
  ( applyAntibonding
  , applyDragStrength
  , applyFreeElectronsOnly
  , applyLayerSpace
  , applyOrbit
  , applySubshellView
  , applyValenceOnly
  , applyZoomSet
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
                      -- layerSpace is read from the SAME state as detail so render and
                      -- pick remain in parity.
                      , modelMatrix: \st ->
                          builderDetailPlaceWith st.layerSpace st.detail a.pos
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
                , modelMatrix: \st -> builderBallPlaceWith st.layerSpace st.detail (Atom.atomicRadius a.z) a.pos
                }
            )
            s.builder.atoms

        -- Atomic-layer BOND LINES: N parallel lines per bond of order N
        -- (M2-S5: bond multiplicity). The central sigma line runs on the bond
        -- axis; each flanking pi line is offset PERPENDICULAR to the axis using
        -- the SAME S4 basis as Builder.Electrons.piPairs, so the bond-order 2
        -- second line sits where its pi electron pair is.
        --
        -- Basis (mirrors S4 piPairs exactly):
        --   u   = normalised bond axis (a → b)
        --   ref = world-Y unless |u.y| > 0.9, then world-X
        --   p1  = normalise(ref × u)    ← first perpendicular
        --   p2  = normalise(u  × p1)    ← second perpendicular
        --
        -- For bond order N, N-1 flanking lines are placed at angles
        --   θ_k = k * π / N  (k = 1 .. N-1)
        -- giving offset (piLineOffset * builderScale) × (cos θ_k · p1 + sin θ_k · p2).
        -- At order 1 this loop is empty and only the central sigma line is drawn —
        -- byte-identical to the pre-S5 output.
        --
        -- Render-only: reads vibratedBondLines (pure read, no model mutation).
        builderBondLineEntities :: State -> Array Entity
        builderBondLineEntities s =
          concatMap bondLineGroup (Vibration.vibratedBondLines s.builder s.frame)
          where
          -- Small world-unit perpendicular offset for flanking pi lines.
          -- Sized proportionally to electronCloud (same scale as piRadius in S4)
          -- and the builderScale so the offset is visible at the atom-ball zoom level.
          piLineOffset :: Number
          piLineOffset = 12.0

          -- Compute the perpendicular basis from the (unscaled) model axis a→b.
          -- Returns { p1, p2 } matching S4 piPairs exactly.
          perpBasis
            :: { x :: Number, y :: Number, z :: Number }
            -> { x :: Number, y :: Number, z :: Number }
            -> { p1 :: { x :: Number, y :: Number, z :: Number }
               , p2 :: { x :: Number, y :: Number, z :: Number }
               }
          perpBasis a b =
            let
              ax = b.x - a.x
              ay = b.y - a.y
              az = b.z - a.z
              d = sqrt (ax * ax + ay * ay + az * az)
              u =
                if d < 1.0e-9 then { x: 1.0, y: 0.0, z: 0.0 }
                else { x: ax / d, y: ay / d, z: az / d }
              absUY = if u.y < 0.0 then -u.y else u.y
              ref =
                if absUY > 0.9 then { x: 1.0, y: 0.0, z: 0.0 }
                else { x: 0.0, y: 1.0, z: 0.0 }
              -- p1 = normalise(ref × u)
              c1x = ref.y * u.z - ref.z * u.y
              c1y = ref.z * u.x - ref.x * u.z
              c1z = ref.x * u.y - ref.y * u.x
              lc1 = sqrt (c1x * c1x + c1y * c1y + c1z * c1z)
              p1 =
                if lc1 < 1.0e-9 then { x: 1.0, y: 0.0, z: 0.0 }
                else { x: c1x / lc1, y: c1y / lc1, z: c1z / lc1 }
              -- p2 = normalise(u × p1)
              c2x = u.y * p1.z - u.z * p1.y
              c2y = u.z * p1.x - u.x * p1.z
              c2z = u.x * p1.y - u.y * p1.x
              lc2 = sqrt (c2x * c2x + c2y * c2y + c2z * c2z)
              p2 =
                if lc2 < 1.0e-9 then { x: 0.0, y: 0.0, z: 1.0 }
                else { x: c2x / lc2, y: c2y / lc2, z: c2z / lc2 }
            in
              { p1, p2 }

          -- Build the Entity list for one bond (sigma line + (order-1) pi lines).
          -- World positions are computed INSIDE the modelMatrix lambdas so they
          -- react to the live st.layerSpace (render/pick parity).
          bondLineGroup :: { a :: { x :: Number, y :: Number, z :: Number }, b :: { x :: Number, y :: Number, z :: Number }, order :: Int } -> Array Entity
          bondLineGroup seg =
            let
              basis = perpBasis seg.a seg.b
              n = seg.order
              -- Central sigma line on the axis.
              sigmaLine =
                { mesh: Solid bondLineMesh
                , modelMatrix: \st ->
                    let
                      ws = builderWorldPosWith st.layerSpace seg.a
                      we = builderWorldPosWith st.layerSpace seg.b
                    in
                      builderBondLinePlaceWith st.layerSpace st.detail ws we
                }
              -- Flanking pi lines: (order-1) of them, distributed by θ_k = k*π/N.
              piLines =
                map
                  ( \k ->
                      { mesh: Solid bondLineMesh
                      , modelMatrix: \st ->
                          let
                            ws = builderWorldPosWith st.layerSpace seg.a
                            we = builderWorldPosWith st.layerSpace seg.b
                            theta = DI.toNumber k * pi / DI.toNumber n
                            cT = cos theta
                            sT = sin theta
                            -- Pi-line offset scaled by the effective world scale.
                            off = piLineOffset * builderScale * st.layerSpace
                            ox = off * (cT * basis.p1.x + sT * basis.p2.x)
                            oy = off * (cT * basis.p1.y + sT * basis.p2.y)
                            oz = off * (cT * basis.p1.z + sT * basis.p2.z)
                            wa = { x: ws.x + ox, y: ws.y + oy, z: ws.z + oz }
                            wb = { x: we.x + ox, y: we.y + oy, z: we.z + oz }
                          in
                            builderBondLinePlaceWith st.layerSpace st.detail wa wb
                      }
                  )
                  (range 1 (n - 1))
            in
              [ sigmaLine ] <> piLines

        -- CORE (inner-shell) lone electrons: one blue sphere per core lone
        -- electron, on the inner ring around each atom's centre
        -- (Builder.coreLoneElectronPositions). Reuses the single blue
        -- molElectronMesh — core electrons keep the existing colour.
        builderLoneElectronEntities :: State -> Array Entity
        builderLoneElectronEntities s =
          builderElectronGroupEntitiesWith molElectronMesh
            (Builder.coreLoneElectronGroups s.builder s.frame)

        -- VALENCE (outermost-shell) lone electrons: one amber sphere per valence
        -- lone electron, on the outer ring (Builder.valenceLoneElectronPositions).
        -- Uses the distinct builderValenceElectronMesh (amber/gold).
        builderValenceElectronEntities :: State -> Array Entity
        builderValenceElectronEntities s =
          builderElectronGroupEntitiesWith builderValenceElectronMesh
            (Builder.valenceLoneElectronGroups s.builder s.frame)

        -- Shared (bonding / antibonding) electrons: the pair sitting BETWEEN
        -- each bond's two nuclei when s.antibonding = false (Bonding phase,
        -- standard in-phase pair), or pushed outward past each nucleus when
        -- s.antibonding = true (Antibonding phase, node at midpoint).
        -- Phase is chosen via Builder.bondElectronGroupsPhased using
        -- Builder.Bonding / Builder.Antibonding. The LOD bloom centre is always
        -- the bond midpoint regardless of phase (sensible fade in both cases).
        -- Bonding electrons ARE valence electrons → reuses the amber valence mesh.
        -- Render-only: never mutates BuilderState.
        builderBondElectronEntities :: State -> Array Entity
        builderBondElectronEntities s =
          let
            phase = if s.antibonding then Builder.Antibonding else Builder.Bonding
          in
            builderElectronGroupEntitiesWith builderValenceElectronMesh
              (Builder.bondElectronGroupsPhased phase s.builder s.frame)

        -- The Builder entity list reused for both Builder and Materials scenes.
        -- Materials loads a curated crystal lattice via loadStructure (same
        -- shared Ref), so the same atom/bond/electron entities render correctly.
        builderLikeEntities :: State -> Array Entity
        builderLikeEntities s =
          builderAtomBallEntities s
            <> builderBondLineEntities s
            <> builderAtomEntities s
            <> (if s.valenceOnly then [] else builderLoneElectronEntities s)
            <> builderValenceElectronEntities s
            <> (if s.freeElectronsOnly then [] else builderBondElectronEntities s)

        -- Nuclide scene: nucleus entities (Z protons + N neutrons via Atom.clusterPositions).
        -- Delegated to Main.Nuclide with the shared proton/neutron GL meshes partially applied.
        nuclideEntities :: State -> Array Entity
        nuclideEntities = nuclideNucleusEntities protonMesh neutronMesh

        entitiesFor :: State -> Array Entity
        entitiesFor s = case s.scene of
          CubePoc -> cubeEntities
          Atomos -> starEntities <> ringEntities s <> nucleusEntities s <> electronEntities s
          Molecule -> starEntities <> moleculeNucleusEntities s <> moleculeElectronEntities s
          Builder -> builderLikeEntities s
          -- Materials reuses the SAME Builder entity list (atom balls, bond lines,
          -- nucleus, electrons). The only difference is the initial state is loaded
          -- via loadStructure 0 (Diamond) on first entry.
          Materials -> builderLikeEntities s
          -- Nuclide: nucleus only (protons + neutrons, no electrons).
          Nuclide -> nuclideEntities s
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
      -- Tracks the scene from the previous frame so we can detect scene changes
      -- (e.g. entering Materials for the first time → default-load Diamond).
      prevSceneRef <- Ref.new initialState.scene
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
          when (isBuilderLike s.scene) (renderFrame (s { builder = bs }))
      -- A mutable slot for the "select structure i" UI callback. Filled after
      -- selectedStructureRef is created (below). This breaks the forward-reference
      -- cycle: installBuilderApi must be called before selectedStructureRef is
      -- allocated, but the loadStructure seam must invoke the callback that closes
      -- over selectedStructureRef. The Ref is written exactly once before any card
      -- click or seam call can arrive (DOM wiring is synchronous, no async gap).
      selectCallbackRef <- Ref.new (\(_i :: Int) -> pure unit :: Effect Unit)
      builderRef <- installBuilderApi eagerRender
        ( \i -> do
            cb <- Ref.read selectCallbackRef
            cb i
        )
      installBuilderControls eagerRender builderRef
      -- The Builder camera orbit (yaw/pitch radians) lives in ONE shared Ref, the
      -- single source of truth read by the renderer (mirrored into State each
      -- frame, like builderRef), written by the empty-space orbit-drag, and
      -- read/written by window.__builder.setOrbit/getOrbit. It is camera state, so
      -- it stays OUT of the pure Builder model.
      orbitRef <- Ref.new { yaw: initialState.builderYaw, pitch: initialState.builderPitch }
      installOrbitApi orbitRef
      -- The nuclear state (current nuclide) lives in a shared Ref. The render loop
      -- mirrors it into State each frame (like builderRef/orbitRef). Mutations through
      -- the NuclearApi (addProton/etc.) fire an onChange callback that eagerly
      -- re-renders the Nuclide scene so pixel reads reflect the new nucleus immediately.
      let
        eagerRenderNuclide :: Nuclear.NuclearState -> Effect Unit
        eagerRenderNuclide ns = do
          s <- Ref.read lastStateRef
          when (s.scene == Nuclide) (renderFrame (s { nuclear = ns }))
      nuclearRef <- installNuclearApi eagerRenderNuclide
      -- Wire the Nuclide scene's left-drawer controls. The reset brings the nuclide
      -- back to the Carbon-12 seed (Z=6, N=defaultNeutrons 6). The setNuclide
      -- callback handles the #nuclide-z + #nuclide-n inputs.
      let
        nuclearMutate :: (Nuclear.NuclearState -> Nuclear.NuclearState) -> Effect Unit
        nuclearMutate f = do
          Ref.modify_ f nuclearRef
          ns <- Ref.read nuclearRef
          eagerRenderNuclide ns

        c12Seed :: Nuclear.NuclearState
        c12Seed = { nuclide: { z: 6, n: Nuclear.defaultNeutrons 6 }, lastQ: 0.0, lastFission: Nothing }
      installNuclearControls
        (nuclearMutate (\st -> st { nuclide = Nuclear.addProton st.nuclide }))
        (nuclearMutate (\st -> st { nuclide = Nuclear.removeProton st.nuclide }))
        (nuclearMutate (\st -> st { nuclide = Nuclear.addNeutron st.nuclide }))
        (nuclearMutate (\st -> st { nuclide = Nuclear.removeNeutron st.nuclide }))
        (nuclearMutate (\_ -> c12Seed))
        (\z n -> nuclearMutate (\_ -> c12Seed { nuclide = { z, n } }))
      -- M3: wire named-reaction buttons (α, β−, β+/EC, fuse, fission).
      -- Delegated to Main.Nuclide.installNuclearReactions to keep Main under 800 lines.
      installNuclearReactions nuclearMutate
      -- Production pointer pick+drag: scene-gated to Builder and Materials,
      -- routed through the SAME shared Ref + eagerRender as the API, so there is
      -- one source of truth and the drag is on-screen immediately. Reads the live
      -- canvas size to build the exact projection the renderer uses. An empty-space
      -- miss orbits the camera (writing the shared orbitRef) instead of moving an atom.
      installBuilderPick canvas builderRef orbitRef eagerRender (Ref.read lastStateRef) applyOrbit
      -- On-screen orbit buttons: each click folds a fixed Camera.buttonOrbitDelta
      -- step through Main.applyOrbit into the SAME shared orbitRef as the empty-
      -- space drag (no new Ref/clamp — applyOrbit already pitch-clamps), and
      -- #orbit-reset writes {yaw:0,pitch:0}. Active for Builder and Materials
      -- (both use the Builder render path). The next rAF frame mirrors orbitRef →
      -- State and the sizeRef gate re-uploads the projection, so no eager
      -- re-render is needed.
      let
        orbitWhenBuilder :: ({ yaw :: Number, pitch :: Number } -> { yaw :: Number, pitch :: Number }) -> Effect Unit
        orbitWhenBuilder f = do
          s <- Ref.read lastStateRef
          when (isBuilderLike s.scene) (Ref.modify_ f orbitRef)
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
      -- Tracks the currently-selected curated structure index so the cards
      -- gallery can highlight the active card (updated on default-load and on
      -- card-click). Starts at 0 (Diamond, the default-load target).
      selectedStructureRef <- Ref.new 0
      -- The pure "UI side effects for selecting structure i": update the selection
      -- Ref, highlight the card, and refresh the #materials-info panel with the
      -- structure's name/formula/hybridization + properties rows. This is the
      -- callback stored in selectCallbackRef so the seam (loadStructure via
      -- installBuilderApi) can invoke it without a forward-reference cycle.
      let
        selectStructureUi :: Int -> Effect Unit
        selectStructureUi i = do
          Ref.write i selectedStructureRef
          let
            st = Lattice.structureOf i
            -- Header rows (name, formula, hybridization) + per-structure properties.
            infoRows =
              [ { label: "Name", value: st.name }
              , { label: "Formula", value: st.formula }
              , { label: "Hybridization", value: st.hybridization }
              ]
                <> st.properties
          MaterialsCards.highlightCard i
          Controls.renderInfoPanel "materials-info" infoRows
      -- Fill the callback slot so loadStructure(i) through the seam invokes
      -- selectStructureUi. Synchronous write before any card/seam call.
      Ref.write selectStructureUi selectCallbackRef
      -- Render the #materials-cards gallery ONCE at init, data-driven from
      -- Lattice.structures (one card per entry). The onSelect callback (a) writes
      -- the builderRef + eager re-renders (the model mutation), then (b) calls
      -- selectStructureUi (the UI side effects — card highlight + info panel).
      -- Both paths (card click and seam loadStructure) always call both steps.
      let
        cardDataArray =
          map
            (\s -> { name: s.name, formula: s.formula, hybridization: s.hybridization })
            Lattice.structures

        onSelectCard :: Int -> Effect Unit
        onSelectCard i = do
          let newState = (Lattice.structureOf i).build
          Ref.write newState builderRef
          eagerRender newState
          selectStructureUi i
      MaterialsCards.renderMaterialsCards cardDataArray onSelectCard
      -- One-shot state override Ref: written on Materials entry to zoom out and
      -- fit the enlarged lattice in the frustum. Consumed by the runLoop tick
      -- (applied once, then cleared to Nothing). The Builder scene is unaffected —
      -- we only write this Ref on the Materials entry edge, not for Builder.
      stateOverrideRef <- Ref.new (Nothing :: Maybe (State -> State))
      runLoop
        { initial: initialState
        , step
        , stateOverride: stateOverrideRef
        , draw: \s0 -> do
            -- Pull the live builder snapshot AND the live camera orbit AND the
            -- live nuclear state into the state used for rendering (all are shared
            -- Refs, the single sources of truth for each sub-system).
            bs0 <- Ref.read builderRef
            orb <- Ref.read orbitRef
            ns <- Ref.read nuclearRef
            -- Default-load Diamond (index 0) when entering the Materials scene
            -- for the first time (or re-entering with an empty world). This runs
            -- once per entry by detecting the scene-change edge. The Builder scene
            -- always starts empty (its Add/Clear buttons manage it) — we only
            -- auto-load for Materials.
            prevScene <- Ref.read prevSceneRef
            bs <-
              if s0.scene == Materials && prevScene /= Materials && (length bs0.atoms == 0) then do
                let latticeState = (Lattice.structureOf 0).build
                Ref.write latticeState builderRef
                -- Highlight card 0 (Diamond) on auto-load, refresh the info panel,
                -- and reset selection — via selectStructureUi (single source of truth).
                selectStructureUi 0
                -- Reframe: reset orbit to face-on and zoom out so the enlarged
                -- 2×2×2 Diamond supercell (bounding radius ≈ 660 model units ×
                -- builderScale 2.2 = 1452 world units) fits the fov=π/3 frustum.
                -- zoom = 0.08 gives effective camera distance = 1000/0.08 = 12500;
                -- the lattice fits comfortably in the wide frustum, and since 0.08
                -- is below the new detailLo (0.10) layerBlend 0.08 == 0, so the
                -- gallery opens on the whole-lattice atom-ball view (not sub-atomic).
                -- 0.08 > minZoom (0.05), so it is within the legal zoom range.
                -- The orbit reset makes Diamond's cubic symmetry immediately legible.
                -- Builder scene orbit + zoom are preserved (we only write these
                -- on the Materials entry edge, not for Builder).
                Ref.write { yaw: 0.0, pitch: 0.0 } orbitRef
                Ref.write (Just (_ { zoom = Camera.clampZoom 0.08 })) stateOverrideRef
                pure latticeState
              else
                pure bs0
            -- Show/hide the materials overlays (cards gallery + info panel) only in
            -- the Materials scene. Called on every scene-change edge so the overlays
            -- mirror how molecule-info / atom-labels are gated in updateOverlay /
            -- syncAtomLabels.
            when (prevScene /= s0.scene) do
              let inMaterials = s0.scene == Materials
              MaterialsCards.showMaterialsCards inMaterials
              MaterialsCards.showMaterialsPanel inMaterials
            Ref.write s0.scene prevSceneRef
            let s = s0 { builder = bs, builderYaw = orb.yaw, builderPitch = orb.pitch, nuclear = ns }
            Ref.write s lastStateRef
            -- Publish the live eased Builder detail to window.__builderDetail every
            -- frame (deterministic E2E hook for the LOD cross-fade).
            setBuilderDetail s.detail
            -- Sync the #zoom-slider thumb to the live zoom every frame so that
            -- programmatic zoom changes (wheel, Materials reframe) move the thumb.
            -- Setting .value programmatically does NOT fire the input listener,
            -- so there is no feedback loop.
            setZoomSlider s.zoom
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
            -- Builder + Materials potential-energy (Morse) curve overlay: draw
            -- the well for a representative pair + a live dot per bond; hide off
            -- both scenes. Render-only (DOM via PeOverlay) — no model mutation.
            renderPeOverlay s
            -- Nuclide scene: refresh the live #nuclide-info panel each frame.
            when (s.scene == Nuclide) (renderNuclideInfo s)
        }
