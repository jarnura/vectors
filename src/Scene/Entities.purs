module Scene.Entities
  ( EntityMesh(..)
  , Entity
  , starSphere
  , protonSphere
  , neutronSphere
  , builderProtonSphere
  , builderNeutronSphere
  , builderBallSphere
  , electronSphere
  , moleculeNucleusSphere
  , moleculeElectronSphere
  , builderValenceElectronSphere
  , builderNucleusCompress
  , moleculePlace
  , builderScale
  , builderPlace
  , builderDetailPlace
  , builderBallPlace
  , builderBondLinePlace
  , builderWorldPos
  , builderWorldPosWith
  , builderPlaceWith
  , builderDetailPlaceWith
  , builderBallPlaceWith
  , builderBondLinePlaceWith
  , builderElectronGroupEntities
  , builderElectronGroupEntitiesWith
  ) where

import Prelude

import Atom as Atom
import Data.Array (concatMap)
import Data.Maybe (fromMaybe)
import Data.Number (sqrt)
import Graphics.GL (SolidMesh)
import Graphics.GL as GL
import Math.Matrix (Matrix)
import Math.Matrix as M
import Meshes as Meshes
import Update (State)

-- A renderable mesh is either a solid lit mesh or a wireframe mesh; the draw
-- loop dispatches on the constructor (exhaustively).
data EntityMesh
  = Solid SolidMesh
  | Wire GL.Mesh

type Entity =
  { mesh :: EntityMesh
  , modelMatrix :: State -> Matrix Number
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

-- Builder LOD atom-ball radius: roughly the nucleus extent so the collapsed
-- ball reads as a solid atom at the same on-screen footprint as the detailed
-- nucleus cluster it replaces.
builderBallRadius :: Number
builderBallRadius = Atom.nucleusRadius * 3.0

-- The Builder atom-ball sphere: a single solid sphere reused for every placed
-- atom in the zoomed-out layer. A muted slate so it reads as a neutral atom and
-- is clearly distinct from the red proton / grey neutron / amber valence dots.
builderBallSphere :: Meshes.SolidSpec
builderBallSphere =
  (Meshes.sphere 18 18 builderBallRadius) { color = { r: 0.62, g: 0.70, b: 0.82, a: 1.0 } }

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

-- VALENCE-electron colour for the Builder: a high-saturation amber/gold. R and G
-- are high with B low → dominant channel R, clearly NOT blue (core electrons),
-- NOT proton red {0.90,0.25,0.22} (its G is high here), and NOT neutron grey
-- (its channel spread is wide). Used for outermost-shell lone + bonding electrons.
builderValenceElectronColor :: GL.Color
builderValenceElectronColor = { r: 1.0, g: 0.78, b: 0.18, a: 1.0 }

-- The valence-electron sphere: same size as the core electron dot, but amber so
-- valence (outer-shell + bonding) electrons read distinct from the blue core ones.
builderValenceElectronSphere :: Meshes.SolidSpec
builderValenceElectronSphere =
  (Meshes.sphere 14 14 (Atom.electronRadius * 1.3)) { color = builderValenceElectronColor }

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

-- Lower floor for the per-particle detail scale, so a sub-atomic sphere never
-- collapses to a zero-scale (degenerate) model matrix at detail 0.
builderDetailFloor :: Number
builderDetailFloor = 0.001

-- LOD model matrix for a sub-atomic particle (nucleon/electron) that blooms OUT
-- of its atom centre as detail (d) rises: its position interpolates from the
-- atom centre (d=0) to its full position (d=1), and its sphere shrinks by d (with
-- a small floor). Both centre & full are MODEL-space; builderPlace scales them.
builderDetailPlace :: Number -> Atom.V3 -> Atom.V3 -> Matrix Number
builderDetailPlace d center full =
  let
    lerp c f = c + (f - c) * d
    pos = { x: lerp center.x full.x, y: lerp center.y full.y, z: lerp center.z full.z }
    s = max builderDetailFloor d
  in
    M.multiply (builderPlace pos) (M.scale s s s)

-- LOD model matrix for the zoomed-OUT atom ball: full size at detail 0, shrinking
-- to nothing as detail (d) rises to 1, scaled about the (scaled) atom centre. The
-- per-element atomic-radius factor (rad) scales the ball footprint so Hydrogen
-- (rad ≈ 0.41) reads smaller than Carbon/Oxygen (rad ≈ 1.0).
builderBallPlace :: Number -> Number -> Atom.V3 -> Matrix Number
builderBallPlace d rad center =
  let
    s = rad * max builderDetailFloor (1.0 - d)
  in
    M.multiply (builderPlace center) (M.scale s s s)

-- World-space model matrix that maps the unit bond beam (along +Y, (0,0,0)→
-- (0,1,0)) so its base lands at `start` and its tip at `end`, with the beam
-- LENGTH faded by (1−detail) so the bond line shows in the zoomed-OUT ball layer
-- and shrinks to nothing as detail→1. Built as a raw 4×4 column-vector matrix:
-- translation column = start, Y-basis column = (end − start)·(1−detail), and
-- X/Z-basis columns = the unit axes (so the beam's baked half-width survives as
-- its screen width). v=(0,0,0)→start; v=(0,1,0)→start+(end−start)·f.
builderBondLinePlace :: Number -> Atom.V3 -> Atom.V3 -> Matrix Number
builderBondLinePlace d start end =
  let
    f = 1.0 - d
    dx = end.x - start.x
    dy = end.y - start.y
    dz = end.z - start.z
    yx = dx * f
    yy = dy * f
    yz = dz * f
    -- Unit screen-plane (XY) perpendicular to the bond, so the flat strip's width
    -- spans the camera-facing plane and is never edge-on. For a bond direction
    -- (dx,dy) the in-plane perpendicular is (−dy, dx); normalise it (fall back to
    -- +X for a degenerate / zero-length bond).
    plen = sqrt (dx * dx + dy * dy)
    px = if plen > 0.0 then negate dy / plen else 1.0
    py = if plen > 0.0 then dx / plen else 0.0
  in
    -- Row-major entries: each row i = [col0 col1 col2 col3]. Columns:
    -- col0 (X-basis)=(px,py,0) unit perpendicular, col1 (Y-basis)=(end−start)·f,
    -- col2 (Z-basis)=0 (flat), col3 (translation)=start; bottom row = [0,0,0,1].
    fromMaybe M.identity
      ( M.fromArray 4 4
          [ px
          , yx
          , 0.0
          , start.x
          , py
          , yy
          , 0.0
          , start.y
          , 0.0
          , yz
          , 0.0
          , start.z
          , 0.0
          , 0.0
          , 0.0
          , 1.0
          ]
      )

-- The world-space point the renderer projects for a builder atom: its model-pos
-- scaled about the origin by builderScale (matching builderPlace's translation).
builderWorldPos :: Atom.V3 -> Atom.V3
builderWorldPos p =
  { x: p.x * builderScale, y: p.y * builderScale, z: p.z * builderScale }

-- Parameterized variants: each takes the live layerSpace multiplier so the
-- effective scale = builderScale × layerSpace. At layerSpace=1.0 they are
-- byte-identical to the originals. These are used by the Builder/Materials
-- render path (Main.purs) and by syncAtomLabels / installBuilderPick in
-- Main/Builder.purs — ALL use the SAME layerSpace from State so render and
-- pick stay in parity.

-- World-space position for a builder atom, parameterized by layerSpace.
builderWorldPosWith :: Number -> Atom.V3 -> Atom.V3
builderWorldPosWith ls p =
  let
    s = builderScale * ls
  in
    { x: p.x * s, y: p.y * s, z: p.z * s }

-- Place a builder particle, parameterized by layerSpace.
builderPlaceWith :: Number -> Atom.V3 -> Matrix Number
builderPlaceWith ls p =
  let
    s = builderScale * ls
  in
    M.translate (p.x * s) (p.y * s) (p.z * s)

-- LOD model matrix for a sub-atomic particle, parameterized by layerSpace.
builderDetailPlaceWith :: Number -> Number -> Atom.V3 -> Atom.V3 -> Matrix Number
builderDetailPlaceWith ls d center full =
  let
    lerp c f = c + (f - c) * d
    pos = { x: lerp center.x full.x, y: lerp center.y full.y, z: lerp center.z full.z }
    sc = max builderDetailFloor d
  in
    M.multiply (builderPlaceWith ls pos) (M.scale sc sc sc)

-- LOD model matrix for the zoomed-OUT atom ball, parameterized by layerSpace.
builderBallPlaceWith :: Number -> Number -> Number -> Atom.V3 -> Matrix Number
builderBallPlaceWith ls d rad center =
  let
    sc = rad * max builderDetailFloor (1.0 - d)
  in
    M.multiply (builderPlaceWith ls center) (M.scale sc sc sc)

-- Bond-line model matrix, parameterized by layerSpace. The start/end arguments
-- are WORLD-space positions (already scaled via builderWorldPosWith ls) — this
-- function is identical to builderBondLinePlace because the scaling is handled
-- by the caller (builderWorldPosWith ls). Provided as a symmetrical API partner.
builderBondLinePlaceWith :: Number -> Number -> Atom.V3 -> Atom.V3 -> Matrix Number
builderBondLinePlaceWith _ls d start end = builderBondLinePlace d start end

-- Build the LOD electron entities for a set of bloom GROUPS (each a bloom centre
-- + the electron positions that bloom out of it). Every electron's model matrix
-- interpolates from its group centre (detail 0) to its full position (detail 1)
-- and shrinks with detail, via builderDetailPlace — so the electrons collapse
-- into the atom centre / bond midpoint as the atom-ball takes over. Reused by the
-- core (blue), valence (amber) and bond (amber) electron entity builders.
builderElectronGroupEntities
  :: SolidMesh -> Array { center :: Atom.V3, positions :: Array Atom.V3 } -> Array Entity
builderElectronGroupEntities mesh groups =
  concatMap
    ( \g ->
        map
          ( \p ->
              { mesh: Solid mesh
              , modelMatrix: \st -> builderDetailPlace st.detail g.center p
              }
          )
          g.positions
    )
    groups

-- Parameterized variant: reads st.layerSpace from State to pass to
-- builderDetailPlaceWith so the electron bloom radius scales with layerSpace.
builderElectronGroupEntitiesWith
  :: SolidMesh -> Array { center :: Atom.V3, positions :: Array Atom.V3 } -> Array Entity
builderElectronGroupEntitiesWith mesh groups =
  concatMap
    ( \g ->
        map
          ( \p ->
              { mesh: Solid mesh
              , modelMatrix: \st -> builderDetailPlaceWith st.layerSpace st.detail g.center p
              }
          )
          g.positions
    )
    groups
