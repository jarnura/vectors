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
  , spawnPos
  , addAtom
  , moveAtom
  , moveAtomWith
  , pullBonds
  , pullPasses
  , moveMolecule
  , componentOf
  , atomById
  , clear
  , bondThreshold
  , breakThreshold
  , contactFactor
  , absoluteMin
  , floorMargin
  , floorCeil
  , relaxPasses
  , minSeparation
  , resolveOverlaps
  , recomputeBonds
  , bondMidpoints
  , bondSegments
  , degreeOf
  , loneCountOf
  , valenceShellOf
  , bondElectronPositions
  , bondElectronGroups
  , loneElectronPositions
  , coreLoneElectronPositions
  , valenceLoneElectronPositions
  , coreLoneElectronGroups
  , valenceLoneElectronGroups
  , molecules
  , formulaOf
  , projectToScreen
  , unprojectAtDepth
  , unprojectAtDepthFull
  ) where

import Prelude

import Atom (V3, atomicRadius, electronShells, elementOf, nucleonRadius, nucleusRadius)
import Chem (bondEnergy, valence)
import Data.Array (any, concat, concatMap, filter, find, foldl, index, last, length, mapWithIndex, nub, range, snoc, sortBy, sortWith, uncons, (!!))
import Data.Foldable (elem)
import Data.Int (toNumber)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Number (cos, pi, sin, sqrt)
import Math.Matrix (Matrix, multiply, fromColumn, toVector)
import Math.Matrix as M

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

-- A deterministic 3D spawn position for the atom at insertion index `i`, laid
-- out on a growing golden-angle / Fibonacci shell so successive atoms differ in
-- x, y AND z (never the old collinear y = z = 0 line). The azimuth advances by
-- the golden angle, the latitude sweeps a band so points stay well-spread, and
-- the radius grows slowly (∝ sqrt i) — tied to bondThreshold so near atoms can
-- still auto-bond while the Pauli contact floor survives resolveOverlaps. Pure,
-- total, NaN-free (sqrt domains guarded with max).
spawnPos :: Int -> V3
spawnPos i =
  let
    fi = toNumber i
    azimuth = fi * goldenAngle
    -- Cycle the latitude band every `spawnLatK` atoms (radius keeps growing with
    -- the true index) so the shell never collapses to a pole/axis at high counts.
    -- `i mod spawnLatK` is the identity for i < spawnLatK, so the first
    -- `spawnLatK` atoms are byte-unchanged.
    latIndex = toNumber (i `mod` spawnLatK)
    yUnit = clampUnit (1.0 - 2.0 * ((latIndex + 0.5) / toNumber spawnLatK))
    r = sqrt (max 0.0 (1.0 - yUnit * yUnit))
    radius = bondThreshold * 0.5 + bondThreshold * 0.28 * sqrt (max 0.0 fi)
  in
    { x: radius * r * cos azimuth
    , y: radius * yUnit
    , z: radius * r * sin azimuth
    }

-- The golden angle, pi * (3 - sqrt 5) ≈ 2.399963 rad — the irrational turn that
-- keeps successive Fibonacci-shell azimuths maximally spread.
goldenAngle :: Number
goldenAngle = pi * (3.0 - sqrt 5.0)

-- Latitude-band size for spawnPos: the band cycles every `spawnLatK` atoms
-- (radius still grows with the true index), so the shell spreads in 3D at any
-- count instead of collapsing to a pole/axis past the band edge.
spawnLatK :: Int
spawnLatK = 12

-- Clamp a value into [-1, 1] so acos/sqrt latitude maths can never go NaN.
clampUnit :: Number -> Number
clampUnit x = max (-1.0) (min 1.0 x)

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

-- FIXED number of bond-pull passes: like `relaxPasses`, the solver is bounded
-- (no convergence loop), so it always terminates; 10 passes propagate a tug
-- down chains of ~10 bonds.
pullPasses :: Int
pullPasses = 10

-- Slack pulled bonds settle inside the FORM threshold: a pulled pair lands at
-- bondThreshold − pullSlack (= 160), comfortably inside both thresholds so the
-- bond is stable after the pull.
pullSlack :: Number
pullSlack = 20.0

-- Rest length a strong bond is pulled back to: inside the bond-form threshold
-- but never below the pair's Pauli contact floor (`minSeparation`), so the
-- pull and the overlap solver agree (a pulled pair is a fixed point of both).
pullRestLen :: Int -> Int -> Number
pullRestLen za zb = max (minSeparation za zb) (bondThreshold - pullSlack)

-- Strength-aware bond tug. For the drag of atom `draggedAid` at strength
-- `strength`, run exactly `pullPasses` Gauss-Seidel passes; each pass walks
-- the bonds in deterministic ascending (min, max) endpoint-id order and, for
-- each bond stretched past breakThreshold whose energy resists the drag
-- (`Chem.bondEnergy za zb >= strength`), pulls ONE endpoint along the bond
-- axis toward the other so the pair lands at `pullRestLen`:
--   * the dragged atom is NEVER moved — a bond incident to it pulls the
--     other endpoint;
--   * a chain bond not incident to the dragged atom pulls the endpoint
--     FARTHER from the dragged atom's current position toward the nearer one
--     (this is what propagates the tug down a chain across passes).
-- Strength-beaten bonds (energy < strength) are skipped and left stretched,
-- so `recomputeBonds` breaks them afterwards. Coincident endpoints reuse the
-- deterministic `separationDir`/`tieBreakDir` machinery, so degenerate axes
-- are NaN-free. Bonds are not re-derived here; only positions change. Pure,
-- total, deterministic.
pullBonds :: Number -> Int -> BuilderState -> BuilderState
pullBonds strength draggedAid st0 =
  foldl (\s _ -> pullPass s) st0 (range 1 pullPasses)
  where
  ordered =
    sortBy
      ( \x y ->
          compare (min x.a x.b) (min y.a y.b)
            <> compare (max x.a x.b) (max y.a y.b)
      )
      st0.bonds

  pullPass s = foldl (pullBond strength draggedAid) s ordered

-- One bond-tug step: pull this bond's far endpoint back to rest length if the
-- bond is overstretched AND strong enough to resist the drag (see pullBonds).
pullBond :: Number -> Int -> BuilderState -> BBond -> BuilderState
pullBond strength draggedAid s bd =
  case atomById s bd.a, atomById s bd.b of
    Just pa, Just pb ->
      let
        d = distance pa.pos pb.pos
      in
        if d <= breakThreshold || bondEnergy pa.z pb.z < strength then s
        else pullPair draggedAid s pa pb d
    _, _ -> s

-- Pull the chosen endpoint of an overstretched strong bond along the bond
-- axis so the pair lands at `pullRestLen`. The dragged atom is never the one
-- moved; for a bond with both endpoints free, the endpoint farther from the
-- dragged atom's current position is pulled toward the nearer one (ties pull
-- the second endpoint — deterministic either way).
pullPair :: Int -> BuilderState -> PlacedAtom -> PlacedAtom -> Number -> BuilderState
pullPair draggedAid s pa pb d =
  let
    picked =
      if pa.id == draggedAid then { mover: pb, target: pa }
      else if pb.id == draggedAid then { mover: pa, target: pb }
      else if dragDist pa <= dragDist pb then { mover: pb, target: pa }
      else { mover: pa, target: pb }
    -- Unit direction from the held endpoint toward the pulled one (id-derived
    -- tie-break when coincident, so no division by zero / NaN is possible).
    dir = separationDir picked.target picked.mover d
    rest = pullRestLen picked.mover.z picked.target.z
  in
    setAtomPos picked.mover.id (alongFrom picked.target.pos dir rest) s
  where
  -- Distance from an endpoint to the dragged atom's CURRENT position; a
  -- missing dragged atom makes both sentinel-equal, hitting the tie branch.
  dragDist atom =
    case atomById s draggedAid of
      Just dAtom -> distance atom.pos dAtom.pos
      Nothing -> 1.0e18

-- The connected component (molecule) containing `aid`, as the sorted atom ids —
-- reusing the SAME connected-component flood `molecules` uses, so there is one
-- definition of "molecule". A lone atom is its own singleton; an absent id is
-- the empty array.
componentOf :: BuilderState -> Int -> Array Int
componentOf st aid = fromMaybe [] (find (\comp -> elem aid comp) (molecules st))

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
    formed = foldl tryForm kept (idPairs st)
  in
    st { bonds = formed }
  where
  alreadyBonded bonds p =
    any (\bd -> (bd.a == p.a && bd.b == p.b) || (bd.a == p.b && bd.b == p.a)) bonds

  tryForm bonds p =
    if
      not (alreadyBonded bonds p)
        && distById st p.a p.b < bondThreshold
        && freeValence st bonds p.a > 0
        && freeValence st bonds p.b > 0 then snoc bonds p
    else bonds

-- All unordered atom-id pairs, in ascending (a, b) id order for determinism.
-- The single candidate-pair walk shared by `recomputeBonds` (bond formation)
-- and `resolveOverlaps` (separation constraints).
idPairs :: BuilderState -> Array { a :: Int, b :: Int }
idPairs st = do
  i <- range 0 (length ids - 1)
  j <- range 0 (length ids - 1)
  if j <= i then []
  else case ids !! i, ids !! j of
    Just a, Just b -> [ { a, b } ]
    _, _ -> []
  where
  ids = sortBy compare (map _.id st.atoms)

-- ───── Pauli exclusion: minimum-separation constraint model ──────────
--
-- Physics: the Pauli exclusion principle forbids the FILLED electron shells of
-- two atoms from interpenetrating — two atoms can never collapse onto each
-- other, so their centres always keep a hard minimum distance. The covalent
-- shared bonding pair is the one ALLOWED overlap: bonded atoms may sit close
-- (anywhere below bondThreshold), just never below the contact floor. The
-- solver below only moves atom CENTRES; the shared electron-pair rendering and
-- all bond logic are untouched.

-- Scale from the summed normalised covalent radii (Atom.atomicRadius) of a
-- pair to its world-unit contact floor.
contactFactor :: Number
contactFactor = 55.0

-- Hard non-collapse floor (world units): whatever the elements, nuclei /
-- filled shells can never interpenetrate closer than this.
absoluteMin :: Number
absoluteMin = 130.0

-- Safety margin keeping every contact floor strictly below bondThreshold.
floorMargin :: Number
floorMargin = 15.0

-- Ceiling on the contact floor (= 165.0 = bondThreshold − floorMargin): EVERY
-- pair's floor stays strictly below bondThreshold (180), so valence
-- auto-bonding keeps working for any element pair, however large.
floorCeil :: Number
floorCeil = bondThreshold - floorMargin

-- Clamp a raw radius-derived floor into [absoluteMin, floorCeil].
clampFloor :: Number -> Number
clampFloor x = max absoluteMin (min floorCeil x)

-- Minimum allowed centre distance between atoms of atomic numbers z1/z2:
-- proportional to the sum of their normalised covalent radii, clamped so it
-- never drops below the hard floor nor reaches bondThreshold. Symmetric.
minSeparation :: Int -> Int -> Number
minSeparation z1 z2 =
  clampFloor (contactFactor * (atomicRadius z1 + atomicRadius z2))

-- FIXED number of relaxation passes: the solver is bounded (no convergence
-- loop), so it always terminates; 10 passes settle small clusters.
relaxPasses :: Int
relaxPasses = 10

-- Below this pair distance the separation direction is degenerate (coincident
-- atoms) and the deterministic id-derived tie-break direction is used instead.
coincidentEps :: Number
coincidentEps = 1.0e-9

-- Anchor-aware bounded relaxation enforcing `minSeparation` between every atom
-- pair. Runs exactly `relaxPasses` Gauss-Seidel passes; each pass walks all
-- unordered id pairs in ascending (a, b) order (the same `idPairs` walk
-- `recomputeBonds` uses) and projects each violating pair back onto its floor:
--   * d >= floor          → no change (valid states are fixed points, so the
--                           solver is idempotent — no drag jitter);
--   * both ids anchored   → SKIP (documented limitation: an intra-rigid-
--                           component overlap is left to the component itself);
--   * exactly one anchor  → push ONLY the non-anchor along the centre line,
--                           away from the anchor, landing exactly at the floor;
--   * neither anchored    → push BOTH apart symmetrically by (floor − d)/2
--                           each along the centre line.
-- Atom array order and ids are preserved (only `pos` changes); bonds are NOT
-- recomputed here (M1 is the pure model only). Pure, total, deterministic.
resolveOverlaps :: Array Int -> BuilderState -> BuilderState
resolveOverlaps anchors st0 = foldl (\s _ -> relaxPass s) st0 (range 1 relaxPasses)
  where
  relaxPass s = foldl (separatePair anchors) s (idPairs s)

-- Project one id pair back onto its separation floor (one Gauss-Seidel step).
-- Pairs that already satisfy the floor, fully-anchored pairs, and pairs with a
-- missing endpoint are returned unchanged.
separatePair :: Array Int -> BuilderState -> { a :: Int, b :: Int } -> BuilderState
separatePair anchors s p =
  case atomById s p.a, atomById s p.b of
    Just pa, Just pb ->
      let
        floorDist = minSeparation pa.z pb.z
        d = distance pa.pos pb.pos
        aFixed = elem p.a anchors
        bFixed = elem p.b anchors
      in
        if d >= floorDist || (aFixed && bFixed) then s
        else
          let
            -- Unit direction from atom a toward atom b (id-derived tie-break
            -- when coincident, so no division by zero / NaN is possible).
            dir = separationDir pa pb d
          in
            if aFixed then setAtomPos p.b (alongFrom pa.pos dir floorDist) s
            else if bFixed then setAtomPos p.a (alongFrom pb.pos (negV3 dir) floorDist) s
            else
              let
                push = (floorDist - d) / 2.0
              in
                setAtomPos p.b (alongFrom pb.pos dir push)
                  (setAtomPos p.a (alongFrom pa.pos (negV3 dir) push) s)
    _, _ -> s

-- Unit separation direction from `pa` toward `pb` given their distance `d`
-- (passed in so it is computed once). A coincident pair (d < coincidentEps)
-- has no direction, so a deterministic tie-break direction is derived from the
-- pair ids instead — reproducible run-to-run, never NaN, never zero-length.
separationDir :: PlacedAtom -> PlacedAtom -> Number -> V3
separationDir pa pb d
  | d < coincidentEps = tieBreakDir pa.id pb.id
  | otherwise =
      { x: (pb.pos.x - pa.pos.x) / d
      , y: (pb.pos.y - pa.pos.y) / d
      , z: (pb.pos.z - pa.pos.z) / d
      }

-- Deterministic unit direction for a coincident pair: +x rotated in the XY
-- plane by an id-derived angle, so distinct coincident pairs fan out in
-- distinct directions while every run produces identical results. cos/sin of
-- a finite number is always finite, so the result is NaN-free by construction.
tieBreakDir :: Int -> Int -> V3
tieBreakDir a b =
  let
    theta = toNumber (a * 7 + b * 13) * 0.61803398875
  in
    { x: cos theta, y: sin theta, z: 0.0 }

-- The point `dist` world units from `origin` along the unit direction `dir`.
alongFrom :: V3 -> V3 -> Number -> V3
alongFrom origin dir dist =
  { x: origin.x + dir.x * dist
  , y: origin.y + dir.y * dist
  , z: origin.z + dir.z * dist
  }

-- Negate a vector (flip a direction).
negV3 :: V3 -> V3
negV3 v = { x: -v.x, y: -v.y, z: -v.z }

-- Replace ONLY the position of atom `aid` (immutably; array order, ids and all
-- other atoms untouched).
setAtomPos :: Int -> V3 -> BuilderState -> BuilderState
setAtomPos aid pos s =
  s { atoms = map (\a -> if a.id == aid then a { pos = pos } else a) s.atoms }

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

-- Endpoint pairs (in world units) for each bond: the two bonded atoms' centres.
-- One segment per bond whose BOTH endpoints resolve; bonds with a missing
-- endpoint are dropped. Used by the renderer to draw a bond line/stick.
bondSegments :: BuilderState -> Array { a :: V3, b :: V3 }
bondSegments st = foldl collect [] st.bonds
  where
  collect acc bd =
    case atomById st bd.a, atomById st bd.b of
      Just a, Just b -> snoc acc { a: a.pos, b: b.pos }
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
