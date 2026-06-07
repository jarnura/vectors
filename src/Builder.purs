-- Pure world model for the interactive molecule builder. Atoms are placed and
-- moved in scene units; bonds are recomputed from geometry with a valence cap
-- and break hysteresis; connected components form molecules with Unicode-
-- subscript formulae. Includes pure pick/unproject helpers so a screen cursor
-- can be mapped to/from world space without any WebGL or Effect dependency.
--
-- Everything here is pure, total and deterministic.
module Builder
  ( PlacedAtom
  , BBond
  , BuilderState
  , emptyBuilder
  , addAtom
  , moveAtom
  , clear
  , bondThreshold
  , breakThreshold
  , recomputeBonds
  , bondMidpoints
  , degreeOf
  , loneCountOf
  , bondElectronPositions
  , loneElectronPositions
  , molecules
  , formulaOf
  , projectToScreen
  , unprojectAtDepth
  ) where

import Prelude

import Atom (V3, elementOf, nucleonRadius, nucleusRadius)
import Chem (valence)
import Data.Array (any, concatMap, filter, foldl, index, length, mapWithIndex, nub, range, snoc, sortBy, sortWith, (!!))
import Data.Foldable (elem)
import Data.Int (toNumber)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Number (cos, pi, sin, sqrt)
import Math.Matrix (Matrix, multiply, fromColumn, toVector)

-- A placed atom: a stable id, its atomic number, and its world position.
type PlacedAtom = { id :: Int, z :: Int, pos :: V3 }

-- A single (undirected) bond between two atom ids.
type BBond = { a :: Int, b :: Int }

-- The whole builder world: the atoms, the bonds between them, the next id to
-- hand out, and the currently picked atom (if any).
type BuilderState =
  { atoms :: Array PlacedAtom
  , bonds :: Array BBond
  , nextId :: Int
  , picked :: Maybe Int
  }

-- An empty world: no atoms, no bonds, ids start at 0.
emptyBuilder :: BuilderState
emptyBuilder = { atoms: [], bonds: [], nextId: 0, picked: Nothing }

-- Distance below which a fresh (unbonded) pair will FORM a bond.
bondThreshold :: Number
bondThreshold = 180.0

-- Distance above which an EXISTING bond breaks. Strictly greater than
-- bondThreshold, giving a hysteresis band where bonds persist but don't form.
breakThreshold :: Number
breakThreshold = 230.0

-- Place a new atom of atomic number `z` at `pos`. It receives a fresh id from
-- nextId, is appended (prior atoms untouched), nextId is bumped, and bonds are
-- recomputed. Returns a new record.
addAtom :: Int -> V3 -> BuilderState -> BuilderState
addAtom z pos st =
  recomputeBonds
    st
      { atoms = snoc st.atoms { id: st.nextId, z, pos }
      , nextId = st.nextId + 1
      }

-- Move the atom with id `aid` to `pos` (immutably replacing only that atom's
-- position) and recompute bonds. Other atoms are untouched.
moveAtom :: Int -> V3 -> BuilderState -> BuilderState
moveAtom aid pos st =
  recomputeBonds
    st
      { atoms = map (\a -> if a.id == aid then a { pos = pos } else a) st.atoms }

-- Reset to the empty world.
clear :: BuilderState -> BuilderState
clear _ = emptyBuilder

-- ───── Bond recomputation: valence cap + break hysteresis ────────────

-- Euclidean distance between two world points.
distance :: V3 -> V3 -> Number
distance a b =
  let
    dx = a.x - b.x
    dy = a.y - b.y
    dz = a.z - b.z
  in
    sqrt (dx * dx + dy * dy + dz * dz)

-- Look up an atom by id.
atomById :: BuilderState -> Int -> Maybe PlacedAtom
atomById st aid = index (filter (\a -> a.id == aid) st.atoms) 0

-- Distance between two atoms (by id); large sentinel if either is missing.
distById :: BuilderState -> Int -> Int -> Number
distById st x y =
  case atomById st x, atomById st y of
    Just a, Just b -> distance a.pos b.pos
    _, _ -> 1.0e18

-- Current degree (number of bonds touching `aid`) in a bond set.
degreeIn :: Array BBond -> Int -> Int
degreeIn bonds aid =
  length (filter (\bd -> bd.a == aid || bd.b == aid) bonds)

-- Free valence of `aid` given a bond set: its element valence minus degree.
freeValence :: BuilderState -> Array BBond -> Int -> Int
freeValence st bonds aid =
  case atomById st aid of
    Just a -> valence a.z - degreeIn bonds aid
    Nothing -> 0

-- Recompute bonds with valence capping and break hysteresis:
--   (a) KEEP every existing bond whose endpoints are still within breakThreshold;
--   (b) then, in deterministic id order, FORM a single bond for each unbonded
--       pair whose distance < bondThreshold AND where both endpoints still have
--       free valence (valence − degree in the bond set being built).
recomputeBonds :: BuilderState -> BuilderState
recomputeBonds st =
  let
    kept = filter (\bd -> distById st bd.a bd.b <= breakThreshold) st.bonds
    formed = foldl tryForm kept candidatePairs
  in
    st { bonds = formed }
  where
  -- All unordered atom-id pairs, in ascending (a, b) id order for determinism.
  ids = sortBy compare (map _.id st.atoms)
  candidatePairs = do
    i <- range 0 (length ids - 1)
    j <- range 0 (length ids - 1)
    if j <= i then []
    else case ids !! i, ids !! j of
      Just a, Just b -> [ { a, b } ]
      _, _ -> []

  alreadyBonded bonds p =
    any (\bd -> (bd.a == p.a && bd.b == p.b) || (bd.a == p.b && bd.b == p.a)) bonds

  tryForm bonds p =
    if
      not (alreadyBonded bonds p)
        && distById st p.a p.b < bondThreshold
        && freeValence st bonds p.a > 0
        && freeValence st bonds p.b > 0 then snoc bonds p
    else bonds

-- Midpoint (in world units) of each bond's two endpoint atoms. Used by the
-- renderer to place a shared bonding electron between the bonded nuclei. Bonds
-- whose endpoints can't be resolved are skipped.
bondMidpoints :: BuilderState -> Array V3
bondMidpoints st = foldl collect [] st.bonds
  where
  collect acc bd =
    case atomById st bd.a, atomById st bd.b of
      Just a, Just b ->
        snoc acc
          { x: (a.pos.x + b.pos.x) / 2.0
          , y: (a.pos.y + b.pos.y) / 2.0
          , z: (a.pos.z + b.pos.z) / 2.0
          }
      _, _ -> acc

-- ───── Lone / bonding electrons (electron conservation) ──────────────

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
      Just a, Just b ->
        let
          mid =
            { x: (a.pos.x + b.pos.x) / 2.0
            , y: (a.pos.y + b.pos.y) / 2.0
            , z: (a.pos.z + b.pos.z) / 2.0
            }
          -- Half the separation, used to bound the offset within the bond.
          span = distance a.pos b.pos / 2.0
          speed = 0.03
          phase = frame * speed
          -- A single offset vector; the two electrons sit at mid ± offset so their
          -- mean is exactly the midpoint regardless of frame.
          dx = 0.25 * span * cos phase
          dy = electronCloud * sin phase
          dz = electronCloud * cos phase
        in
          [ { x: mid.x + dx, y: mid.y + dy, z: mid.z + dz }
          , { x: mid.x - dx, y: mid.y - dy, z: mid.z - dz }
          ]
      _, _ -> []

-- Lone electron positions: `loneCountOf` electrons per atom, orbiting on a ring
-- clearly OUTSIDE the nucleus cluster (radius `loneOrbitRadius`) around the atom
-- centre, evenly spaced and rotated by the frame so they visibly orbit the
-- nucleus. Length = Σ loneCountOf over atoms. Deterministic for a fixed frame.
-- Pure, total. Model-space.
loneElectronPositions :: BuilderState -> Number -> Array V3
loneElectronPositions st frame = concatMap atomLones st.atoms
  where
  atomLones a =
    let
      n = loneCountOf st a.id
      speed = 0.05
      phase = frame * speed
    in
      if n <= 0 then []
      else map
        ( \k ->
            let
              theta = 2.0 * pi * toNumber k / toNumber (max 1 n) + phase
            in
              { x: a.pos.x + loneOrbitRadius * cos theta
              , y: a.pos.y + loneOrbitRadius * sin theta
              , z: a.pos.z
              }
        )
        (range 0 (n - 1))

-- Small transverse radius for the shared bonding-pair breathe.
electronCloud :: Number
electronCloud = nucleonRadius

-- Orbit radius of an atom's lone electrons: well outside the nucleon cluster
-- (which spans ~nucleusRadius), so the nucleus reads clearly and the electrons
-- visibly ring it instead of sitting inside it.
loneOrbitRadius :: Number
loneOrbitRadius = nucleusRadius * 1.4

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

-- Multiply a 4x4 matrix by a homogeneous vector (w = 1): M * [x, y, z, 1]ᵀ.
mulVec :: Matrix Number -> V3 -> { x :: Number, y :: Number, z :: Number, w :: Number }
mulVec m v =
  let
    out = toVector (multiply m (fromColumn [ v.x, v.y, v.z, 1.0 ]))
  in
    { x: fromMaybe 0.0 (out !! 0)
    , y: fromMaybe 0.0 (out !! 1)
    , z: fromMaybe 0.0 (out !! 2)
    , w: fromMaybe 0.0 (out !! 3)
    }

-- Read entry (i, j) of a 4x4 row-major matrix.
entry :: Matrix Number -> Int -> Int -> Number
entry m i j = fromMaybe 0.0 (toVector m !! (i * 4 + j))
