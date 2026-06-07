module Meshes
  ( MeshSpec
  , SolidSpec
  , mainCube
  , satelliteCube
  , solidMainCube
  , solidSatelliteCube
  , groundPlane
  , gridFloor
  , orbitRing
  , orbitRingFlat
  , sphere
  ) where

import Prelude

import Data.Array (concatMap, (..))
import Data.Int (toNumber)
import Data.Number (cos, pi, sin)
import Graphics.GL (Color)

type MeshSpec =
  { vertices :: Array Number
  , indices :: Array Int
  , color :: Color
  }

type SolidSpec =
  { vertices :: Array Number
  , normals :: Array Number
  , indices :: Array Int
  , color :: Color
  }

-- ───── Wireframe variants ─────────────────────────────────────────────

mainCube :: MeshSpec
mainCube = wireCubeAt 100.0 black

satelliteCube :: MeshSpec
satelliteCube = wireCubeAt 25.0 red

wireCubeAt :: Number -> Color -> MeshSpec
wireCubeAt h color =
  { vertices: wireCubeVertices h
  , indices: wireCubeIndices
  , color
  }

wireCubeVertices :: Number -> Array Number
wireCubeVertices h =
  [ -h
  , -h
  , h -- 0: front bottom-left
  , h
  , -h
  , h -- 1: front bottom-right
  , h
  , h
  , h -- 2: front top-right
  , -h
  , h
  , h -- 3: front top-left
  , -h
  , -h
  , -h -- 4: back bottom-left
  , -h
  , h
  , -h -- 5: back top-left
  , h
  , h
  , -h -- 6: back top-right
  , h
  , -h
  , -h -- 7: back bottom-right
  ]

wireCubeIndices :: Array Int
wireCubeIndices =
  [ 0
  , 1
  , 1
  , 2
  , 2
  , 3
  , 3
  , 0 -- front face
  , 4
  , 5
  , 5
  , 6
  , 6
  , 7
  , 7
  , 4 -- back face
  , 0
  , 4
  , 1
  , 7
  , 2
  , 6
  , 3
  , 5 -- connecting edges
  ]

-- ───── Solid lit variants ─────────────────────────────────────────────

solidMainCube :: SolidSpec
solidMainCube = solidCubeAt 100.0 indigo

solidSatelliteCube :: SolidSpec
solidSatelliteCube = solidCubeAt 25.0 red

solidCubeAt :: Number -> Color -> SolidSpec
solidCubeAt h color =
  { vertices: solidCubeVertices h
  , normals: solidCubeNormals
  , indices: solidCubeIndices
  , color
  }

-- 24 vertices: 4 per face × 6 faces. Each face's vertices are listed CCW
-- when viewed from outside, so triangles are wound counter-clockwise for
-- the GPU's CCW front-face convention.
solidCubeVertices :: Number -> Array Number
solidCubeVertices h =
  [ -- Front (+Z): A,B,C,D
    -h
  , -h
  , h
  , h
  , -h
  , h
  , h
  , h
  , h
  , -h
  , h
  , h
  -- Back (-Z): F,E,H,G
  , h
  , -h
  , -h
  , -h
  , -h
  , -h
  , -h
  , h
  , -h
  , h
  , h
  , -h
  -- Top (+Y): D,C,G,H
  , -h
  , h
  , h
  , h
  , h
  , h
  , h
  , h
  , -h
  , -h
  , h
  , -h
  -- Bottom (-Y): E,F,B,A
  , -h
  , -h
  , -h
  , h
  , -h
  , -h
  , h
  , -h
  , h
  , -h
  , -h
  , h
  -- Right (+X): B,F,G,C
  , h
  , -h
  , h
  , h
  , -h
  , -h
  , h
  , h
  , -h
  , h
  , h
  , h
  -- Left (-X): E,A,D,H
  , -h
  , -h
  , -h
  , -h
  , -h
  , h
  , -h
  , h
  , h
  , -h
  , h
  , -h
  ]

solidCubeNormals :: Array Number
solidCubeNormals =
  [ 0.0
  , 0.0
  , 1.0
  , 0.0
  , 0.0
  , 1.0
  , 0.0
  , 0.0
  , 1.0
  , 0.0
  , 0.0
  , 1.0
  , 0.0
  , 0.0
  , -1.0
  , 0.0
  , 0.0
  , -1.0
  , 0.0
  , 0.0
  , -1.0
  , 0.0
  , 0.0
  , -1.0
  , 0.0
  , 1.0
  , 0.0
  , 0.0
  , 1.0
  , 0.0
  , 0.0
  , 1.0
  , 0.0
  , 0.0
  , 1.0
  , 0.0
  , 0.0
  , -1.0
  , 0.0
  , 0.0
  , -1.0
  , 0.0
  , 0.0
  , -1.0
  , 0.0
  , 0.0
  , -1.0
  , 0.0
  , 1.0
  , 0.0
  , 0.0
  , 1.0
  , 0.0
  , 0.0
  , 1.0
  , 0.0
  , 0.0
  , 1.0
  , 0.0
  , 0.0
  , -1.0
  , 0.0
  , 0.0
  , -1.0
  , 0.0
  , 0.0
  , -1.0
  , 0.0
  , 0.0
  , -1.0
  , 0.0
  , 0.0
  ]

-- 36 indices = 6 faces × 2 triangles × 3 vertices.
-- For face starting at vertex i (i, i+1, i+2, i+3), the two triangles are
-- (i, i+1, i+2) and (i, i+2, i+3).
solidCubeIndices :: Array Int
solidCubeIndices =
  [ 0
  , 1
  , 2
  , 0
  , 2
  , 3 -- front
  , 4
  , 5
  , 6
  , 4
  , 6
  , 7 -- back
  , 8
  , 9
  , 10
  , 8
  , 10
  , 11 -- top
  , 12
  , 13
  , 14
  , 12
  , 14
  , 15 -- bottom
  , 16
  , 17
  , 18
  , 16
  , 18
  , 19 -- right
  , 20
  , 21
  , 22
  , 20
  , 22
  , 23 -- left
  ]

-- ───── World backdrop (M1: pure geometry) ─────────────────────────────

-- A flat ground quad at local Y = 0, spanning [-e, +e] in X and Z, with a
-- +Y normal. Vertices are wound CCW seen from above so the quad survives
-- BACK-face culling. The model matrix positions it in the world.
groundPlane :: Number -> SolidSpec
groundPlane e =
  { vertices:
      [ -e
      , 0.0
      , e -- 0: front-left  (+Z)
      , e
      , 0.0
      , e -- 1: front-right
      , e
      , 0.0
      , -e -- 2: back-right
      , -e
      , 0.0
      , -e -- 3: back-left
      ]
  , normals:
      [ 0.0, 1.0, 0.0, 0.0, 1.0, 0.0, 0.0, 1.0, 0.0, 0.0, 1.0, 0.0 ]
  , indices: [ 0, 1, 2, 0, 2, 3 ]
  , color: groundGreen
  }

-- A grid of floor lines at local Y = 0: (n+1) lines parallel to X and (n+1)
-- parallel to Z, each spanning [-e, +e], drawn as GL_LINES. n divisions ⇒
-- 2*(n+1) segments ⇒ 4*(n+1) index entries.
gridFloor :: Number -> Int -> MeshSpec
gridFloor e n =
  { vertices: gridVertices e n
  , indices: gridIndices n
  , color: gridGray
  }

gridVertices :: Number -> Int -> Array Number
gridVertices e n =
  concatMap line (0 .. n)
  where
  step = (2.0 * e) / toNumber n
  coord i = -e + step * toNumber i
  -- For each i: a line parallel to X (fixed Z) and a line parallel to Z (fixed X).
  line i =
    let
      c = coord i
    in
      [ -e
      , 0.0
      , c
      , e
      , 0.0
      , c -- parallel to X at Z = c
      , c
      , 0.0
      , -e
      , c
      , 0.0
      , e
      ] -- parallel to Z at X = c

gridIndices :: Int -> Array Int
gridIndices n = 0 .. (4 * (n + 1) - 1)

-- ───── Orbital ring line (atomos: one thin ring per sub-shell) ─────────

-- A thin wireframe ring (closed GL_LINES loop) of `segments` points at radius
-- `r`, lying in an orbital plane tilted by `incl`. The vertex formula is exactly
-- the electron-path formula from Atom.electronPositions, so the ring traces the
-- electrons' orbit. `segments` vertices ⇒ 2·`segments` index entries (loop closed
-- by joining the last point back to the first).
orbitRing :: Int -> Number -> Number -> MeshSpec
orbitRing segments r incl =
  { vertices: concatMap vertex (0 .. (segments - 1))
  , indices: concatMap segment (0 .. (segments - 1))
  , color: orbitBlue
  }
  where
  vertex k =
    let
      theta = 2.0 * pi * toNumber k / toNumber segments
    in
      [ r * cos theta, -r * sin theta * sin incl, r * sin theta * cos incl ]
  segment k = [ k, mod (k + 1) segments ]

-- Like `orbitRing`, but FLAT in the XY plane (z = 0), facing the camera.
orbitRingFlat :: Int -> Number -> MeshSpec
orbitRingFlat segments r =
  { vertices: concatMap vertex (0 .. (segments - 1))
  , indices: concatMap segment (0 .. (segments - 1))
  , color: orbitBlue
  }
  where
  vertex k =
    let
      theta = 2.0 * pi * toNumber k / toNumber segments
    in
      [ r * cos theta, r * sin theta, 0.0 ]
  segment k = [ k, mod (k + 1) segments ]

-- ───── UV sphere (atomos: protons/neutrons/electrons/stars) ───────────

-- A solid UV sphere of `latSeg` latitude bands × `longSeg` longitude bands and
-- radius `r`, centered at the local origin. Vertices lie on the sphere; normals
-- are the unit position vectors. Default color is white — callers override
-- `color` via record update (e.g. `(sphere 12 12 1.0) { color = protonRed }`).
sphere :: Int -> Int -> Number -> SolidSpec
sphere latSeg longSeg r =
  { vertices: positions \nx ny nz -> [ r * nx, r * ny, r * nz ]
  , normals: positions \nx ny nz -> [ nx, ny, nz ]
  , indices: sphereIndices latSeg longSeg
  , color: sphereWhite
  }
  where
  -- Build a flat array by mapping every (lat i, long j) vertex through `f`,
  -- where (nx,ny,nz) is the unit surface normal at that vertex.
  positions f =
    concatMap
      ( \i ->
          concatMap
            ( \j ->
                let
                  theta = pi * toNumber i / toNumber latSeg
                  phi = 2.0 * pi * toNumber j / toNumber longSeg
                  nx = sin theta * cos phi
                  ny = cos theta
                  nz = sin theta * sin phi
                in
                  f nx ny nz
            )
            (0 .. longSeg)
      )
      (0 .. latSeg)

-- Two triangles per quad; (longSeg+1) vertices per latitude row.
sphereIndices :: Int -> Int -> Array Int
sphereIndices latSeg longSeg =
  concatMap
    ( \i ->
        concatMap
          ( \j ->
              let
                a = i * (longSeg + 1) + j
                b = a + (longSeg + 1)
              in
                [ a, b, a + 1, a + 1, b, b + 1 ]
          )
          (0 .. (longSeg - 1))
    )
    (0 .. (latSeg - 1))

-- ───── Colors ─────────────────────────────────────────────────────────

black :: Color
black = { r: 0.0, g: 0.0, b: 0.0, a: 1.0 }

red :: Color
red = { r: 0.85, g: 0.20, b: 0.20, a: 1.0 }

-- Muted green ground — distinct from the white sky and the indigo cube so
-- E2E pixel checks are unambiguous.
groundGreen :: Color
groundGreen = { r: 0.22, g: 0.45, b: 0.27, a: 1.0 }

-- Darker grid lines, readable against the green ground.
gridGray :: Color
gridGray = { r: 0.13, g: 0.20, b: 0.15, a: 1.0 }

-- Neutral default for spheres; callers override per particle.
sphereWhite :: Color
sphereWhite = { r: 0.90, g: 0.90, b: 0.90, a: 1.0 }

-- Dim blue for the orbital ring lines: visible against near-black space as a
-- subtle guide, without competing with the bright electron/nucleon spheres.
orbitBlue :: Color
orbitBlue = { r: 0.30, g: 0.45, b: 0.70, a: 1.0 }

-- Deeper blue-purple — looks more vibrant under directional light than
-- pure black.
indigo :: Color
indigo = { r: 0.20, g: 0.30, b: 0.85, a: 1.0 }
