-- Pure world model for the interactive molecule builder. Atoms are placed and
-- moved in scene units; bonds are recomputed from geometry with a valence cap
-- and break hysteresis; connected components form molecules with Unicode-
-- subscript formulae. Includes pure pick/unproject helpers so a screen cursor
-- can be mapped to/from world space without any WebGL or Effect dependency.
--
-- Everything here is pure, total and deterministic.
--
-- This module is a FACADE: the world model is decomposed into cohesive
-- Builder.* sub-modules (Types, Geom, Spawn, Overlap, Bonds, Electrons), which
-- are re-exported here alongside the orchestration ops (addAtom/moveAtom/…) and
-- the pick/unproject helpers, so importers keep one stable public surface.
module Builder
  ( module Builder.Types
  , module Builder.Spawn
  , module Builder.Overlap
  , module Builder.Bonds
  , module Builder.Electrons
  , module GeomReExport
  , addAtom
  , moveAtom
  , moveAtomWith
  , moveMolecule
  , componentOf
  , clear
  , molecules
  , formulaOf
  , projectToScreen
  , unprojectAtDepth
  , unprojectAtDepthFull
  ) where

import Prelude

import Atom (V3, elementOf)
import Builder.Bonds (bondMidpoints, bondSegments, breakFrac, pullBonds, pullPasses, recomputeBonds)
import Builder.Electrons
  ( BondPhase(..)
  , Spin(..)
  , bondElectronGroups
  , bondElectronGroupsPhased
  , bondElectronPositions
  , bondElectronPositionsPhased
  , bondSigmaSpins
  , coreLoneElectronGroups
  , coreLoneElectronPositions
  , degreeOf
  , loneCountOf
  , loneElectronPositions
  , valenceLoneElectronGroups
  , valenceLoneElectronPositions
  , valenceShellOf
  )
import Builder.Geom (atomById, entry, mulVec)
import Builder.Geom (atomById, bondThreshold, breakThreshold) as GeomReExport
import Builder.Overlap
  ( absoluteMin
  , contactFactor
  , floorCeil
  , floorMargin
  , minSeparation
  , relaxPasses
  , resolveOverlaps
  )
import Builder.Spawn (spawnPos)
import Builder.Types (BBond, BuilderState, PlacedAtom, emptyBuilder)
import Data.Array (filter, foldl, index, length, mapWithIndex, nub, snoc, sortBy, (!!))
import Data.Foldable (elem)
import Data.Maybe (Maybe(..), fromMaybe)
import Math.Matrix (Matrix)
import Math.Matrix as M

-- Place a new atom of atomic number `z` at `pos`. It receives a fresh id from
-- nextId, is appended, and nextId is bumped. Then the Pauli-exclusion
-- separation constraint runs (`resolveOverlaps`) BEFORE bonds are recomputed:
-- the anchors are the PRE-EXISTING atom ids, so the NEW atom yields (is pushed
-- off any occupied spot) while every existing atom stays exactly put. Physics:
-- filled shells never interpenetrate; the covalent shared pair remains the one
-- allowed overlap (bonding still happens, just never below the contact floor).
-- Returns a new record.
addAtom :: Int -> V3 -> BuilderState -> BuilderState
addAtom z pos st =
  recomputeBonds
    ( resolveOverlaps (map _.id st.atoms)
        st
          { atoms = snoc st.atoms { id: st.nextId, z, pos }
          , nextId = st.nextId + 1
          }
    )

-- Move the atom with id `aid` to `pos` (immutably replacing only that atom's
-- position), with infinite drag strength: every bond's energy is finite, so
-- `pullBonds` never pulls and the legacy pipeline runs unchanged — set pos →
-- Pauli-exclusion separation (`resolveOverlaps [aid]`, the dragged atom lands
-- EXACTLY at the requested target while overlapping atoms yield) →
-- `recomputeBonds`. Byte-compatible with the pre-strength behaviour.
moveAtom :: Int -> V3 -> BuilderState -> BuilderState
moveAtom = moveAtomWith strengthInfinity

-- A drag strength larger than any finite bond energy, so `pullBonds` holds
-- nothing and `moveAtomWith strengthInfinity` degenerates to the legacy
-- energy-blind `moveAtom`.
strengthInfinity :: Number
strengthInfinity = 1.0e18

-- Strength-aware single-atom move: set the atom's position, then let strong
-- bonds TUG their partners along (`pullBonds strength aid` — bonds whose
-- energy beats the drag strength pull the far endpoint back to rest length,
-- weaker bonds are left stretched to break), then the Pauli-exclusion
-- separation constraint (`resolveOverlaps [aid]`, the dragged atom is the
-- anchor and lands exactly at the target), then `recomputeBonds` (stretched
-- weak bonds past breakThreshold drop here). Pure, total, deterministic.
moveAtomWith :: Number -> Int -> V3 -> BuilderState -> BuilderState
moveAtomWith strength aid pos st =
  recomputeBonds
    ( resolveOverlaps [ aid ]
        ( pullBonds strength aid
            st
              { atoms = map (\a -> if a.id == aid then a { pos = pos } else a) st.atoms }
        )
    )

-- The connected component (molecule) containing `aid`, as the sorted atom ids —
-- reusing the SAME connected-component flood `molecules` uses, so there is one
-- definition of "molecule". A lone atom is its own singleton; an absent id is
-- the empty array.
componentOf :: BuilderState -> Int -> Array Int
componentOf st aid = fromMaybe [] (find (\comp -> elem aid comp) (molecules st))
  where
  find pred xs = index (filter pred xs) 0

-- Move the WHOLE molecule of the anchor atom `aid` rigidly so the anchor lands
-- at `pos`: the SAME delta (pos − anchor.pos) is applied to every atom in the
-- anchor's connected component, preserving relative geometry (and therefore the
-- internal bonds). Then the Pauli-exclusion separation constraint runs
-- (`resolveOverlaps`) BEFORE bonds are recomputed: the anchors are ALL the ids
-- of the moved component (computed on the pre-shift state — the rigid shift
-- does not change bonds, so the component is identical after it), so external
-- atoms yield while the moved molecule stays rigid; intra-component pairs are
-- both-anchor and skipped. Physics: filled shells never interpenetrate; the
-- covalent shared pair remains the one allowed overlap. An absent anchor is
-- identity. A lone/singleton atom reduces to `moveAtom`. Single-click+drag
-- uses this; a double-click+drag uses `moveAtom` to move just the one atom.
moveMolecule :: Int -> V3 -> BuilderState -> BuilderState
moveMolecule aid pos st =
  case atomById st aid of
    Nothing -> st
    Just anchor ->
      let
        comp = componentOf st aid
        dx = pos.x - anchor.pos.x
        dy = pos.y - anchor.pos.y
        dz = pos.z - anchor.pos.z
        shift a =
          if elem a.id comp then
            a { pos = { x: a.pos.x + dx, y: a.pos.y + dy, z: a.pos.z + dz } }
          else a
      in
        recomputeBonds (resolveOverlaps comp (st { atoms = map shift st.atoms }))

-- Reset to the empty world.
clear :: BuilderState -> BuilderState
clear _ = emptyBuilder

-- ───── Molecules (connected components) + formulae ───────────────────

-- Neighbours of `aid` in the bond graph.
neighbours :: Array BBond -> Int -> Array Int
neighbours bonds aid =
  map (\bd -> if bd.a == aid then bd.b else bd.a)
    (filter (\bd -> bd.a == aid || bd.b == aid) bonds)

-- Connected components over the bond graph (atom ids). A lone atom is its own
-- singleton component. Components are ordered by their ascending minimum id, and
-- each component's ids are sorted ascending.
molecules :: BuilderState -> Array (Array Int)
molecules st =
  sortWith minId (go (map _.id st.atoms) [])
  where
  sortWith f = sortBy (\x y -> compare (f x) (f y))

  minId comp = fromMaybe 0 (index comp 0)

  go remaining acc =
    case index remaining 0 of
      Nothing -> acc
      Just seed ->
        let
          comp = sortBy compare (nub (flood [ seed ] []))
          rest = filter (\i -> not (elem i comp)) remaining
        in
          go rest (snoc acc comp)

  flood frontier visited =
    case index frontier 0 of
      Nothing -> visited
      Just x ->
        let
          rest = fromMaybe [] (tailOf frontier)
        in
          if elem x visited then flood rest visited
          else flood (rest <> neighbours st.bonds x) (snoc visited x)

  tailOf xs = map _.tail (uncons' xs)

  uncons' xs = case index xs 0 of
    Nothing -> Nothing
    Just h -> Just { head: h, tail: dropFirst xs }

  dropFirst xs = filterWithIndex (\i _ -> i /= 0) xs

  filterWithIndex pred xs =
    map _.v (filter (\t -> pred t.i t.v) (mapWithIndex (\i v -> { i, v }) xs))

-- Element symbols for Z = 1..36 (for formula assembly).
symbolFor :: Int -> String
symbolFor z = (elementOf z).symbol

-- Build a Hill-ish formula string for a component (array of atom ids) with the
-- Unicode subscripts Molecule uses (e.g. {H:2} → "H₂", {O:1,H:2} → "H₂O").
-- Carbon first, then hydrogen, then the rest alphabetically (Hill system).
formulaOf :: BuilderState -> Array Int -> String
formulaOf st comp =
  foldl (\acc s -> acc <> s.symbol <> subscript s.count) "" ordered
  where
  zs = map _.z (filter (\a -> elem a.id comp) st.atoms)
  syms = nub (map symbolFor zs)
  counted =
    map (\s -> { symbol: s, count: length (filter (\z -> symbolFor z == s) zs) })
      syms
  ordered = sortBy hill counted
  hill a b = compare (rank a.symbol) (rank b.symbol) <> compare a.symbol b.symbol
  -- Hill ordering: carbon (0) first, hydrogen (1) next, everything else (2)
  -- alphabetically.
  rank "C" = 0
  rank "H" = 1
  rank _ = 2

-- Render a count as a Unicode subscript suffix; 1 is omitted (e.g. "H₂", "O").
subscript :: Int -> String
subscript 1 = ""
subscript n = digits n
  where
  digits k
    | k < 10 = sub k
    | otherwise = digits (k / 10) <> sub (k `mod` 10)
  sub d = fromMaybe "" (subDigits !! d)
  subDigits =
    [ "₀", "₁", "₂", "₃", "₄", "₅", "₆", "₇", "₈", "₉" ]

-- ───── Pure pick / unproject helpers ─────────────────────────────────

-- Project a world point to a screen pixel using a 4x4 projection matrix
-- (row-major, M*v convention matching the test's testProjection). The point is
-- taken to homogeneous coords, multiplied by the matrix, perspective-divided,
-- then mapped from NDC [-1,1] to pixels with y pointing DOWN.
projectToScreen
  :: Matrix Number -> { w :: Number, h :: Number } -> V3 -> { x :: Number, y :: Number }
projectToScreen proj canvas v =
  let
    clip = mulVec proj v
    ndcX = clip.x / clip.w
    ndcY = clip.y / clip.w
  in
    { x: (ndcX * 0.5 + 0.5) * canvas.w
    , y: (1.0 - (ndcY * 0.5 + 0.5)) * canvas.h
    }

-- Invert the projection for a cursor pixel, placing the result on the plane at
-- the reference atom's depth so projectToScreen ∘ unprojectAtDepth round-trips.
-- We recover the reference point's clip-space w (its depth scale) and use it to
-- undo the perspective divide, then solve x and y from the inverse of the
-- matrix's diagonal scale terms.
unprojectAtDepth
  :: Matrix Number
  -> { w :: Number, h :: Number }
  -> { x :: Number, y :: Number }
  -> V3
  -> V3
unprojectAtDepth proj canvas px ref =
  let
    refClip = mulVec proj ref
    refW = refClip.w
    -- Pixel → NDC (y up), then NDC → clip by re-applying the reference w.
    ndcX = (px.x / canvas.w) * 2.0 - 1.0
    ndcY = (1.0 - px.y / canvas.h) * 2.0 - 1.0
    clipX = ndcX * refW
    clipY = ndcY * refW
    -- The test's projection has clipX = m00 * x and clipY = m11 * y (no cross
    -- terms, no translation feeding x/y), so invert the diagonal scale terms.
    m00 = entry proj 0 0
    m11 = entry proj 1 1
  in
    { x: clipX / m00
    , y: clipY / m11
    , z: ref.z
    }

-- Orbit-aware unproject: invert a cursor pixel back to a world point at the
-- reference atom's depth, accounting for the Builder orbit rotation. Because the
-- orbit matrix is orthogonal (orbitᵀ = orbit⁻¹), no general inverse is needed:
--   1. rotate the world reference into view space (mulVec orbit worldRef),
--   2. unproject the pixel against the plain diagonal `projection` at that depth
--      (reusing unprojectAtDepth, whose diagonal assumption holds in view space),
--   3. rotate the resulting view-space point back into world space (mulVec orbitᵀ).
-- At zero orbit (orbit = identity) this collapses exactly to unprojectAtDepth.
unprojectAtDepthFull
  :: Matrix Number
  -> Matrix Number
  -> { w :: Number, h :: Number }
  -> { x :: Number, y :: Number }
  -> V3
  -> V3
unprojectAtDepthFull orbit proj canvas px worldRef =
  let
    vr = mulVec orbit worldRef
    viewRef = { x: vr.x, y: vr.y, z: vr.z }
    viewPoint = unprojectAtDepth proj canvas px viewRef
    wp = mulVec (M.transpose orbit) viewPoint
  in
    { x: wp.x, y: wp.y, z: wp.z }
