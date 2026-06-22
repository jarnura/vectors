module Test.LatticeSpec where

import Prelude

import Data.Array (all, any, filter, head, length, nub, range, sortBy, (!!))
import Data.Foldable (sum)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Number (abs)
import Effect (Effect)
import Effect.Console (log)
import Builder.Geom (bondThreshold, distance)
import Builder.Overlap (minSeparation)
import Lattice (CuratedStructure, structureOf, structures)
import Test.Util (check)

-- ─── helpers ──────────────────────────────────────────────────────────────────

-- Count bonds incident to an atom id (degree in the bond graph).
degreeById :: Array { a :: Int, b :: Int, order :: Int } -> Int -> Int
degreeById bonds aid =
  sum (map (\bd -> if bd.a == aid || bd.b == aid then 1 else 0) bonds)

-- Check that all ids are unique and contiguous 0..n-1.
idsContiguous :: Array { id :: Int, z :: Int, pos :: { x :: Number, y :: Number, z :: Number } } -> Boolean
idsContiguous atoms =
  let
    ids = sortBy compare (map _.id atoms)
    expected = range 0 (length atoms - 1)
  in
    ids == expected

-- Euclidean distance wrapper (V3).
dist3 :: { x :: Number, y :: Number, z :: Number } -> { x :: Number, y :: Number, z :: Number } -> Number
dist3 = distance

latticeSpec :: Effect Unit
latticeSpec = do
  log "Lattice registry properties:"

  -- ── 1. Registry length and clamp-safe structureOf ─────────────────────────
  check "structures length >= 2" $ length structures >= 2

  let
    diamond = structureOf 0
    graphene = structureOf 1

  check "structureOf 0 is Diamond" $ diamond.name == "Diamond"
  check "structureOf 1 is Graphene" $ graphene.name == "Graphene"

  -- clamp-safe: out-of-range indices clamp without crashing
  check "structureOf (-1) clamps to index 0 (Diamond)" $
    (structureOf (-1)).name == "Diamond"
  check "structureOf 999 clamps to last index (Graphene)" $
    (structureOf 999).name == "Graphene"

  log "  registry + clamp-safe ok."

  -- ── 2. Both structures: atom invariants ───────────────────────────────────
  let
    checkAtomInvariants :: String -> CuratedStructure -> Effect Unit
    checkAtomInvariants label s = do
      let bs = s.build
      check (label <> ": atoms length > 1") $ length bs.atoms > 1
      check (label <> ": all atoms z == 6 (carbon)") $
        all (\a -> a.z == 6) bs.atoms
      check (label <> ": ids unique and contiguous 0..n-1") $
        idsContiguous bs.atoms
      check (label <> ": nextId == length atoms") $
        bs.nextId == length bs.atoms
      check (label <> ": picked == Nothing") $
        bs.picked == Nothing
      check (label <> ": nub ids == ids (no duplicates)") $
        nub (map _.id bs.atoms) == sortBy compare (map _.id bs.atoms)

  checkAtomInvariants "Diamond" diamond
  checkAtomInvariants "Graphene" graphene

  log "  atom invariants ok."

  -- ── 3. Determinism ────────────────────────────────────────────────────────
  check "structureOf 0 is deterministic (atoms)" $
    (structureOf 0).build.atoms == (structureOf 0).build.atoms
  check "structureOf 0 is deterministic (bonds)" $
    (structureOf 0).build.bonds == (structureOf 0).build.bonds
  check "structureOf 1 is deterministic (atoms)" $
    (structureOf 1).build.atoms == (structureOf 1).build.atoms
  check "structureOf 1 is deterministic (bonds)" $
    (structureOf 1).build.bonds == (structureOf 1).build.bonds

  log "  determinism ok."

  -- ── 4. Diamond: supercell count + coordination 4 ──────────────────────────
  let
    dbs = diamond.build
    dAtoms = dbs.atoms
    dBonds = dbs.bonds

    dDegrees = map (\a -> degreeById dBonds a.id) dAtoms

  -- Supercell: at least 20 atoms (genuine multi-cell diamond structure).
  check "Diamond: atom count > 20 (supercell)" $
    length dAtoms > 20

  -- At least 4 atoms with degree 4 (fully sp³-coordinated interior atoms).
  check "Diamond: at least 4 atoms have bond-degree 4 (sp3 interior)" $
    length (filter (_ == 4) dDegrees) >= 4

  -- Every bonded pair distance < bondThreshold (180) and close to cc = 165.
  check "Diamond: all bonded pair distances < bondThreshold (180)" $
    all
      ( \bd ->
          let
            pa = filter (\a -> a.id == bd.a) dAtoms
            pb = filter (\a -> a.id == bd.b) dAtoms
          in
            case pa, pb of
              [ a ], [ b ] -> dist3 a.pos b.pos < bondThreshold
              _, _ -> false
      )
      dBonds

  check "Diamond: all bonded pair distances close to cc = 165 (within 1.0)" $
    all
      ( \bd ->
          let
            pa = filter (\a -> a.id == bd.a) dAtoms
            pb = filter (\a -> a.id == bd.b) dAtoms
          in
            case pa, pb of
              [ a ], [ b ] -> abs (dist3 a.pos b.pos - 165.0) < 1.0
              _, _ -> false
      )
      dBonds

  -- Every distinct atom-pair distance >= minSeparation(6,6) = 130.
  check "Diamond: all atom-pair distances >= minSeparation(6,6) = 130" $
    all
      ( \p ->
          let
            pa = filter (\a -> a.id == p.a) dAtoms
            pb = filter (\a -> a.id == p.b) dAtoms
          in
            case pa, pb of
              [ a ], [ b ] -> dist3 a.pos b.pos >= minSeparation 6 6
              _, _ -> true
      )
      ( do
          i <- range 0 (length dAtoms - 1)
          j <- range 0 (length dAtoms - 1)
          if j <= i then []
          else case dAtoms !! i, dAtoms !! j of
            Just ai, Just aj -> [ { a: ai.id, b: aj.id } ]
            _, _ -> []
      )

  -- 2nd-neighbor distances must be > bondThreshold (no spurious bonds).
  check "Diamond: non-bonded pair distances all > bondThreshold (no spurious bonds)" $
    all
      ( \p ->
          let
            isBonded =
              any (\bd -> (bd.a == p.a && bd.b == p.b) || (bd.a == p.b && bd.b == p.a)) dBonds
            pa = filter (\a -> a.id == p.a) dAtoms
            pb = filter (\a -> a.id == p.b) dAtoms
          in
            if isBonded then true
            else
              case pa, pb of
                [ a ], [ b ] -> dist3 a.pos b.pos > bondThreshold
                _, _ -> true
      )
      ( do
          i <- range 0 (length dAtoms - 1)
          j <- range 0 (length dAtoms - 1)
          if j <= i then []
          else case dAtoms !! i, dAtoms !! j of
            Just ai, Just aj -> [ { a: ai.id, b: aj.id } ]
            _, _ -> []
      )

  log "  Diamond constraints ok."

  -- ── 5. Graphene: supercell count + coordination 3, planarity, minSeparation
  let
    gbs = graphene.build
    gAtoms = gbs.atoms
    gBonds = gbs.bonds
    gDegrees = map (\a -> degreeById gBonds a.id) gAtoms

  -- Supercell: at least 25 atoms (genuine multi-cell graphene sheet).
  check "Graphene: atom count > 25 (supercell)" $
    length gAtoms > 25

  -- At least 6 atoms with degree 3 (fully sp²-coordinated interior atoms).
  check "Graphene: at least 6 atoms have bond-degree 3 (sp2 interior)" $
    length (filter (_ == 3) gDegrees) >= 6

  -- All atoms coplanar: z coordinate is constant to 1e-10.
  let
    gZcoords = map (_.z <<< _.pos) gAtoms
    gZ0 = fromMaybe 0.0 (head gZcoords)

  check "Graphene: all atoms coplanar (z constant to 1e-10)" $
    all (\z -> abs (z - gZ0) < 1.0e-10) gZcoords

  -- Every bonded pair distance < bondThreshold (180).
  check "Graphene: all bonded pair distances < bondThreshold (180)" $
    all
      ( \bd ->
          let
            pa = filter (\a -> a.id == bd.a) gAtoms
            pb = filter (\a -> a.id == bd.b) gAtoms
          in
            case pa, pb of
              [ a ], [ b ] -> dist3 a.pos b.pos < bondThreshold
              _, _ -> false
      )
      gBonds

  -- Every distinct atom-pair distance >= minSeparation(6,6) = 130.
  check "Graphene: all atom-pair distances >= minSeparation(6,6) = 130" $
    all
      ( \p ->
          let
            pa = filter (\a -> a.id == p.a) gAtoms
            pb = filter (\a -> a.id == p.b) gAtoms
          in
            case pa, pb of
              [ a ], [ b ] -> dist3 a.pos b.pos >= minSeparation 6 6
              _, _ -> true
      )
      ( do
          i <- range 0 (length gAtoms - 1)
          j <- range 0 (length gAtoms - 1)
          if j <= i then []
          else case gAtoms !! i, gAtoms !! j of
            Just ai, Just aj -> [ { a: ai.id, b: aj.id } ]
            _, _ -> []
      )

  -- Non-bonded pair distances must all be > bondThreshold.
  check "Graphene: non-bonded pair distances all > bondThreshold (no spurious bonds)" $
    all
      ( \p ->
          let
            isBonded =
              any (\bd -> (bd.a == p.a && bd.b == p.b) || (bd.a == p.b && bd.b == p.a)) gBonds
            pa = filter (\a -> a.id == p.a) gAtoms
            pb = filter (\a -> a.id == p.b) gAtoms
          in
            if isBonded then true
            else
              case pa, pb of
                [ a ], [ b ] -> dist3 a.pos b.pos > bondThreshold
                _, _ -> true
      )
      ( do
          i <- range 0 (length gAtoms - 1)
          j <- range 0 (length gAtoms - 1)
          if j <= i then []
          else case gAtoms !! i, gAtoms !! j of
            Just ai, Just aj -> [ { a: ai.id, b: aj.id } ]
            _, _ -> []
      )

  log "  Graphene constraints ok."

  -- ── 6. Properties metadata ─────────────────────────────────────────────────
  check "Diamond: hybridization is sp3" $ diamond.hybridization == "sp³"
  check "Graphene: hybridization is sp2" $ graphene.hybridization == "sp²"
  check "Diamond: properties non-empty" $ length diamond.properties > 0
  check "Graphene: properties non-empty" $ length graphene.properties > 0

  log "all Lattice properties hold."
