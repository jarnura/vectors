-- Bond recomputation (valence cap + break hysteresis) and the strength-aware
-- Gauss-Seidel bond pull for the Builder, plus bond geometry helpers. Pure,
-- total, deterministic — no Effect/WebGL.
module Builder.Bonds
  ( recomputeBonds
  , pullBonds
  , pullPasses
  , bondMidpoints
  , bondSegments
  ) where

import Prelude

import Atom (V3)
import Builder.Geom
  ( alongFrom
  , atomById
  , bondThreshold
  , breakThreshold
  , degreeIn
  , distById
  , distance
  , idPairs
  , separationDir
  , setAtomPos
  )
import Builder.Overlap (minSeparation)
import Builder.Types (BBond, BuilderState, PlacedAtom)
import Chem (bondEnergy, valence)
import Data.Array (any, filter, foldl, range, snoc, sortBy)
import Data.Maybe (Maybe(..))

-- ───── Bond recomputation: valence cap + break hysteresis ────────────

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
