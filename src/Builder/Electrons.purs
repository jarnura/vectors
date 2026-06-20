-- Lone / bonding electron placement for the Builder (electron conservation):
-- bond degree, lone counts, the valence shell split, and the model-space
-- positions of shared bonding pairs and per-atom core/valence lone electrons —
-- both flat arrays and per-atom grouped forms for the LOD bloom cross-fade.
-- Pure, total, deterministic — no Effect/WebGL.
module Builder.Electrons
  ( degreeOf
  , loneCountOf
  , valenceShellOf
  , bondElectronPositions
  , bondElectronGroups
  , loneElectronPositions
  , coreLoneElectronPositions
  , valenceLoneElectronPositions
  , coreLoneElectronGroups
  , valenceLoneElectronGroups
  ) where

import Prelude

import Atom (V3, electronShells, nucleonRadius, nucleusRadius)
import Builder.Geom (atomById, degreeIn, distance)
import Builder.Types (BuilderState, PlacedAtom)
import Data.Array (concat, concatMap, foldl, last, length, mapWithIndex, range, snoc, uncons)
import Data.Int (toNumber)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Number (cos, pi, sin)

-- Number of bonds incident to atom `aid` in the world's bond set. A public view
-- of the internal degree count, exposed for the renderer's electron clouds.
degreeOf :: BuilderState -> Int -> Int
degreeOf st aid = degreeIn st.bonds aid

-- Lone (non-bonding) electron count for atom `aid`: its element valence minus its
-- bond degree, clamped at 0. Unknown ids (no such atom) yield 0. Together with the
-- bonding electrons (2 per incident bond) this conserves Chem.valence per atom.
-- An atom shows ALL its electrons (Z for a neutral atom): the ones used in bonds
-- live in the bonds (degree of them), the rest are its lone electrons. So a free
-- Carbon shows 6, a free Oxygen 8 — not just the valence/bonding count. Bond
-- FORMATION is still capped by `valence` in recomputeBonds; this is display only.
loneCountOf :: BuilderState -> Int -> Int
loneCountOf st aid =
  case atomById st aid of
    Just a -> max 0 (a.z - degreeOf st aid)
    Nothing -> 0

-- Shared (bonding) electron positions: 2 electrons per resolvable bond, placed
-- mirrored about the bond midpoint along/around the inter-atom axis with a small,
-- bounded frame-driven offset. The pair is symmetric, so their MEAN stays exactly
-- at the bond midpoint; the offset breathes with the frame (frame 0 ≠ frame 60).
-- Length = 2 × (# resolvable bonds). Pure, total, deterministic. Model-space.
bondElectronPositions :: BuilderState -> Number -> Array V3
bondElectronPositions st frame = concatMap bondPair st.bonds
  where
  bondPair bd =
    case atomById st bd.a, atomById st bd.b of
      Just a, Just b -> bondPairAt a b frame
      _, _ -> []

-- The bloom centre + the two shared electrons of each resolvable bond (the bond
-- MIDPOINT and its mirrored pair). Used by the renderer's LOD cross-fade so a
-- bond's shared pair blooms out of the bond midpoint as detail rises. Same data
-- as `bondElectronPositions`, grouped by bond with its midpoint as the centre.
bondElectronGroups
  :: BuilderState -> Number -> Array { center :: V3, positions :: Array V3 }
bondElectronGroups st frame = foldl collect [] st.bonds
  where
  collect acc bd =
    case atomById st bd.a, atomById st bd.b of
      Just a, Just b ->
        let
          mid =
            { x: (a.pos.x + b.pos.x) / 2.0
            , y: (a.pos.y + b.pos.y) / 2.0
            , z: (a.pos.z + b.pos.z) / 2.0
            }
        in
          snoc acc { center: mid, positions: bondPairAt a b frame }
      _, _ -> acc

-- The shared electron pair for the resolved bond endpoints `a`/`b` at `frame`
-- (mirrored about their midpoint). Extracted so both `bondElectronPositions` and
-- `bondElectronGroups` share one definition.
bondPairAt :: PlacedAtom -> PlacedAtom -> Number -> Array V3
bondPairAt a b frame =
  let
    mid =
      { x: (a.pos.x + b.pos.x) / 2.0
      , y: (a.pos.y + b.pos.y) / 2.0
      , z: (a.pos.z + b.pos.z) / 2.0
      }
    span = distance a.pos b.pos / 2.0
    speed = 0.03
    phase = frame * speed
    dx = 0.25 * span * cos phase
    dy = electronCloud * sin phase
    dz = electronCloud * cos phase
  in
    [ { x: mid.x + dx, y: mid.y + dy, z: mid.z + dz }
    , { x: mid.x - dx, y: mid.y - dy, z: mid.z - dz }
    ]

-- Per-atom CORE lone electrons grouped with the bloom centre (the atom centre):
-- the same data as `coreLoneElectronPositions`, kept per atom so the renderer can
-- bloom each atom's core electrons out of that atom's centre under the LOD fade.
coreLoneElectronGroups
  :: BuilderState -> Number -> Array { center :: V3, positions :: Array V3 }
coreLoneElectronGroups st frame =
  map (\a -> { center: a.pos, positions: atomCorePositions st a frame }) st.atoms

-- Per-atom VALENCE lone electrons grouped with the bloom centre (the atom
-- centre): the per-atom companion to `valenceLoneElectronPositions`.
valenceLoneElectronGroups
  :: BuilderState -> Number -> Array { center :: V3, positions :: Array V3 }
valenceLoneElectronGroups st frame =
  map (\a -> { center: a.pos, positions: atomValencePositions st a frame }) st.atoms

-- Lone electron positions: `loneCountOf` electrons per atom, orbiting on a ring
-- clearly OUTSIDE the nucleus cluster (radius `loneOrbitRadius`) around the atom
-- centre, evenly spaced and rotated by the frame so they visibly orbit the
-- nucleus. Length = Σ loneCountOf over atoms. Deterministic for a fixed frame.
-- Pure, total. Model-space.
-- Lone electrons arranged in concentric Bohr SHELLS: an atom's lone electrons
-- fill shells inner-first (capacities 2, 8, 18, 32), each shell a ring at an
-- increasing radius, evenly spaced and frame-rotated (inner shells faster). So a
-- free Carbon (6) shows 2 on an inner ring + 4 on an outer ring; a free Oxygen (8)
-- → 2 + 6; Hydrogen (1) → 1. Length = Σ loneCountOf over atoms. Deterministic for
-- a fixed frame. Pure, total. Model-space.
loneElectronPositions :: BuilderState -> Number -> Array V3
loneElectronPositions st frame =
  coreLoneElectronPositions st frame <> valenceLoneElectronPositions st frame

-- Outermost-shell electron count of element `z`: the last entry of
-- Atom.electronShells (per-principal-shell totals). Carbon→4, Oxygen→6,
-- Hydrogen→1, Neon→8. Clamp-safe (0 for an empty/unknown shell list).
valenceShellOf :: Int -> Int
valenceShellOf z = fromMaybe 0 (last (electronShells z))

-- CORE lone electrons: the inner (always-lone) shells. Per atom, coreCount =
-- z − valenceShellOf z electrons are distributed across the inner shell rings
-- (indices 0..numInner-1) using the SAME ring layout as the old single-pass
-- fillShells placement. Length = Σ (z − valenceShellOf z) over atoms.
-- Deterministic for a fixed frame. Pure, total. Model-space.
coreLoneElectronPositions :: BuilderState -> Number -> Array V3
coreLoneElectronPositions st frame =
  concatMap (\a -> atomCorePositions st a frame) st.atoms

-- CORE lone electron positions for a single atom (the inner always-lone shells).
-- Shared by `coreLoneElectronPositions` (flat) and `coreLoneElectronGroups`.
atomCorePositions :: BuilderState -> PlacedAtom -> Number -> Array V3
atomCorePositions _ a frame =
  let
    coreCount = a.z - valenceShellOf a.z
  in
    concat (mapWithIndex (\i count -> shellRing a frame i count) (fillShells coreCount))

-- VALENCE lone electrons: the outermost shell's non-bonding electrons. Per atom,
-- valenceLone = max 0 (valenceShellOf z − degree) electrons sit on the VALENCE
-- ring at index numInner (= number of core shells = length (electronShells z) −
-- 1), so the valence ring is strictly outside every core ring. Length =
-- Σ max 0 (valenceShellOf z − degree). Deterministic for a fixed frame. Pure,
-- total. Model-space.
valenceLoneElectronPositions :: BuilderState -> Number -> Array V3
valenceLoneElectronPositions st frame =
  concatMap (\a -> atomValencePositions st a frame) st.atoms

-- VALENCE lone electron positions for a single atom (its outermost-shell
-- non-bonding electrons). Shared by `valenceLoneElectronPositions` (flat) and
-- `valenceLoneElectronGroups`.
atomValencePositions :: BuilderState -> PlacedAtom -> Number -> Array V3
atomValencePositions st a frame =
  let
    valenceCount = valenceShellOf a.z
    valenceLone = max 0 (valenceCount - degreeOf st a.id)
    numInner = max 0 (length (electronShells a.z) - 1)
  in
    shellRing a frame numInner valenceLone

-- Shared ring-placement helper: `count` electrons evenly spaced on the ring at
-- shell index `idx` around atom `a`'s centre, frame-rotated. Radius grows with
-- idx (loneOrbitRadius + idx*shellSpacing) so outer rings sit strictly outside
-- inner ones; inner rings orbit faster (phase 0.05/(idx+1)). Used by both the
-- core and valence placements so their radii and phases stay consistent.
shellRing :: PlacedAtom -> Number -> Int -> Int -> Array V3
shellRing a frame idx count
  | count <= 0 = []
  | otherwise =
      let
        r = loneOrbitRadius + toNumber idx * shellSpacing
        -- Inner shells orbit faster, like the atomos rings.
        phase = frame * (0.05 / (toNumber idx + 1.0))
      in
        map
          ( \k ->
              let
                theta = 2.0 * pi * toNumber k / toNumber (max 1 count) + phase
              in
                { x: a.pos.x + r * cos theta
                , y: a.pos.y + r * sin theta
                , z: a.pos.z
                }
          )
          (range 0 (count - 1))

-- Distribute `total` electrons into shells (capacities 2, 8, 18, 32), filling the
-- inner shells first; any overflow lands in a final shell. fillShells 6 = [2,4],
-- fillShells 8 = [2,6], fillShells 1 = [1], fillShells 0 = [].
shellCapacities :: Array Int
shellCapacities = [ 2, 8, 18, 32 ]

fillShells :: Int -> Array Int
fillShells = go shellCapacities
  where
  go caps remaining
    | remaining <= 0 = []
    | otherwise = case uncons caps of
        Nothing -> [ remaining ]
        Just { head: cap, tail } ->
          let
            here = min remaining cap
          in
            [ here ] <> go tail (remaining - here)

-- Small transverse radius for the shared bonding-pair breathe.
electronCloud :: Number
electronCloud = nucleonRadius

-- Radius of the innermost electron shell: well outside the nucleon cluster (which
-- spans ~nucleusRadius), so the nucleus reads clearly and the electrons visibly
-- ring it instead of sitting inside it.
loneOrbitRadius :: Number
loneOrbitRadius = nucleusRadius * 1.4

-- Radial gap between successive electron shells.
shellSpacing :: Number
shellSpacing = nucleusRadius * 1.0
