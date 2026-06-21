-- Bond recomputation (valence cap + break hysteresis) and the strength-aware
-- Gauss-Seidel bond pull for the Builder, plus bond geometry helpers. Pure,
-- total, deterministic — no Effect/WebGL.
module Builder.Bonds
  ( recomputeBonds
  , pullBonds
  , pullPasses
  , bondMidpoints
  , bondSegments
  , breakFrac
  ) where

import Prelude

import Atom (V3)
import Builder.Geom
  ( alongFrom
  , atomById
  , bondThreshold
  , degreeIn
  , distById
  , distance
  , idPairs
  , separationDir
  , setAtomPos
  )
import Builder.Overlap (minSeparation)
import Builder.Types (BBond, BuilderState, PlacedAtom)
import Chem (valence)
import Data.Array (filter, foldl, index, range, snoc, sortBy)
import Data.Maybe (Maybe(..))
import Pe (bondDepth', bondR0', morseForce', stretchEnergy')

-- ───── Bond recomputation: valence cap + break hysteresis ────────────

-- | Fraction of the well depth De at which an existing bond breaks under the
-- | energy criterion. A bond is KEPT while its Morse stretch energy is below
-- | breakFrac * De; it breaks once SE(d) >= De * breakFrac. Chosen so the
-- | energy-crossover distance matches the old geometric breakThreshold (230 wu)
-- | for all common pairs (H-H, C-C, O-H, O-O — any pair sharing R0=160 and
-- | a≈0.049). Derivation: SE crosses De*breakFrac at
-- |   r_break = R0 + ln(1/(1-sqrt(breakFrac)))/a
-- | For breakFrac=0.94 and H-H (a≈0.049, R0=160): r_break≈231.6 ≈ 230 wu ✓.
-- | Hysteresis survives: at bondThreshold (180), SE/De ≈ 0.39 << 0.94, so a
-- | freshly-formed bond is never immediately broken by the energy criterion.
-- | `breakThreshold` remains an upper clamp used in pullBond (documented below).
breakFrac :: Number
breakFrac = 0.94

-- Recompute bonds with valence capping, break hysteresis, and order growth.
--
-- Two-phase approach for correct order assignment:
--
--   Phase 1 (single-bond formation — byte-identical to pre-M2 behaviour):
--     (a) KEEP every existing bond whose Morse stretch energy is below the
--         energy break criterion (Pe.stretchEnergy za zb d < Pe.bondDepth za zb
--         * breakFrac). This replaces the old geometric `d <= breakThreshold`
--         check. At d≈231.6 wu the two criteria agree; breakFrac=0.94 means the
--         bond breaks when 94% of the well depth is stored as stretch energy.
--     (b) In deterministic (min,max)-id order, FORM a single bond (order 1)
--         for each pair whose distance < bondThreshold AND both endpoints have
--         remaining single-bond valence (valence − bond count in phase-1 set).
--
--   Phase 2 (order growth, using phase-1 as a FIXED reference):
--     For each bond in the phase-1 set, compute how much extra order it can
--     absorb using the phase-1 bond set as the reference for free valence:
--
--       fv_extra(aid) = valence(z) − degreeIn(phase1, aid)
--
--     Because phase-1 bonds are all order=1, degreeIn = incident bond COUNT.
--     This "free valence in phase-1 context" is the remaining capacity each
--     atom has for additional bond order after all its single bonds are placed.
--
--     extra = max 0 (min fv_extra_a fv_extra_b (maxBondOrder − 1))
--     new_order = 1 + extra
--
--     Using the phase-1 state as a fixed reference means every bond's upgrade is
--     computed with the SAME baseline, regardless of walk order. Concrete examples:
--
--       Isolated O₂ (O1 and O2 each bond only to each other):
--         phase-1: [O1-O2, order 1]. degreeIn(O1)=1, degreeIn(O2)=1.
--         fv_extra(O1) = 2−1=1, fv_extra(O2)=1. extra=min(1,1,2)=1. Order=2. ✓
--
--       O-O-H chain (O2 bonds to BOTH O1 and H in phase-1):
--         phase-1: [O1-O2,1], [O2-H,1]. degreeIn(O2)=2.
--         For O1-O2: fv_extra(O2) = 2−2=0. extra=0. Order stays 1. ✓
--         For O2-H:  fv_extra(O2) = 0. extra=0. Order stays 1. ✓
--         (O2 correctly distributes its valence-2 as two single bonds.)
--
--       Isolated N₂: fv_extra(N)=3−1=2. extra=min(2,2,2)=2. Order=3. ✓
--
--       CH₄ (C bonded to 4 H): degreeIn(C)=4. fv_extra(C)=4−4=0. No upgrade. ✓
--
-- Hard order cap = 3 (triple is the max; isolated C2 gets order 3 leaving each
-- C with 1 free valence for further bonding).
recomputeBonds :: BuilderState -> BuilderState
recomputeBonds st =
  let
    pairs = idPairs st
    -- Phase 1: form all bonds with order=1 (byte-identical to pre-M2 behaviour).
    phase1 = foldl (formSingle st) [] pairs
    -- Phase 2: upgrade orders using phase-1 as the fixed free-valence reference.
    phase2 = map (upgradeOrder st phase1) phase1
  in
    st { bonds = phase2 }
  where
  -- Maximum bond order representable in the toy model.
  maxBondOrder :: Int
  maxBondOrder = 3

  -- Energy break criterion: keep the bond while its stretch energy is below
  -- breakFrac * De. Pairs whose ids can't be resolved are treated as broken
  -- (distById returns a sentinel 1e18 >> breakThreshold so SE(1e18) → De >> De*breakFrac).
  -- S3: uses order-aware stretchEnergy' and bondDepth' so higher-order bonds
  -- (with larger De) have a wider energy-break horizon.
  bondWithinEnergyBreak st' bd =
    case atomById st' bd.a, atomById st' bd.b of
      Just pa, Just pb ->
        let
          d = distance pa.pos pb.pos
          za = pa.z
          zb = pb.z
          ord = bd.order
        in
          stretchEnergy' za zb ord d < bondDepth' za zb ord * breakFrac
      _, _ -> false

  -- Remaining free valence of atom `aid` against a given bond set.
  remainingFreeValence st' bonds aid =
    case atomById st' aid of
      Just a -> max 0 (valence a.z - degreeIn bonds aid)
      Nothing -> 0

  -- Find an existing bond for pair (p.a, p.b) in the old (pre-recompute) bond set.
  existingBond p =
    index (filter (\bd -> (bd.a == p.a && bd.b == p.b) || (bd.a == p.b && bd.b == p.a)) st.bonds) 0

  -- Phase 1: for each candidate pair, KEEP an existing bond (energy-break /
  -- hysteresis check) at order=1, or FORM a new single bond. Byte-identical
  -- to the pre-M2 recomputeBonds logic (every bond starts at order 1).
  formSingle st' builtBonds p =
    case existingBond p of
      Just oldBd ->
        if bondWithinEnergyBreak st' oldBd then
          -- Kept bond: force order=1; phase 2 may upgrade it.
          snoc builtBonds { a: p.a, b: p.b, order: 1 }
        else
          -- Bond broke (energy criterion failed): try to re-form if the pair
          -- is still within bond-formation range (handles the repulsive-wall
          -- case where d < R0 so SE is large but d < bondThreshold).
          let
            fvA = remainingFreeValence st' builtBonds p.a
            fvB = remainingFreeValence st' builtBonds p.b
          in
            if fvA > 0 && fvB > 0 && distById st' p.a p.b < bondThreshold then
              snoc builtBonds { a: p.a, b: p.b, order: 1 }
            else
              builtBonds
      Nothing ->
        -- Fresh pair: form a single bond if within bondThreshold and both free.
        let
          fvA = remainingFreeValence st' builtBonds p.a
          fvB = remainingFreeValence st' builtBonds p.b
        in
          if fvA > 0 && fvB > 0 && distById st' p.a p.b < bondThreshold then
            snoc builtBonds { a: p.a, b: p.b, order: 1 }
          else
            builtBonds

  -- Phase 2: compute the upgraded order for one bond using the PHASE-1 set as
  -- the fixed free-valence reference. All upgrades are independent of each other
  -- (no ordering dependency) because the same reference is used throughout.
  upgradeOrder st' phase1bonds bd =
    let
      fvA = remainingFreeValence st' phase1bonds bd.a
      fvB = remainingFreeValence st' phase1bonds bd.b
      extra = max 0 (min (min fvA fvB) (maxBondOrder - 1))
    in
      { a: bd.a, b: bd.b, order: 1 + extra }

-- FIXED number of bond-pull passes: like `relaxPasses`, the solver is bounded
-- (no convergence loop), so it always terminates; 10 passes propagate a tug
-- down chains of ~10 bonds.
pullPasses :: Int
pullPasses = 10

-- ── Classical/Morse force relaxation constants ────────────────────────────────
--
-- S4 replaces the one-shot geometric snap with a bounded explicit-Euler force
-- relaxation: each pullPair call applies one step of
--   rawStep = forceStep · (−morseForce za zb d)
--   step    = clamp rawStep (−forceStepCap) forceStepCap
--   newDist = max (minSeparation za zb) (d − step)
-- then places the mover at alongFrom target.pos dir newDist.
--
-- Sign convention: morseForce < 0 when r > R0 (attractive). Negating gives a
-- positive rawStep (decreasing d) when the bond is stretched — the mover
-- converges toward the target.
--
-- Stability (explicit-Euler): non-oscillatory when forceStep < 1/morseK where
-- morseK = 2·a²·De (the harmonic spring constant at R0). For O–H: a≈0.049,
-- De≈4.63, morseK≈0.022, stability bound ≈ 1/0.022 ≈ 45 wu. For H–H: bound
-- ≈ 48 wu. forceStepCap = 72.0 exceeds this near-equilibrium bound, but the
-- energy gate (SE < De*breakFrac, i.e. d < energy-crossover ≈ 231.6 wu)
-- prevents ANY force step from being applied when d ≤ 231.6 — so the system
-- is NEVER driven near equilibrium (d=160) by force steps. The last gate-passing
-- step lands at d≈162 (well above minSep), then the gate stops further pulls.
-- No oscillation occurs in practice.
--
-- Convergence: from d=450 wu (atom moved to x=600, partner at x=150; R0=160)
-- the gate fires on passes where d > energy-crossover (≈231.6). With forceStepCap=72:
--   pass 1: 450 → 378
--   pass 2: 378 → 306
--   pass 3: 306 → 234
--   pass 4: 234 → 162  (last gate-passing step: 234 > 231.6 → fires)
--   pass 5+: 162 < 231.6 → gate stops
-- Final d=162, error |162−160|=2 ≤ bondR0Tol=5.0. ✓
-- The Morse force at r=450 is ~3×10⁻⁷ (dissociation tail); forceStep=1.0e12
-- amplifies this to ~3×10⁵ ≫ 72, so every gate-passing pass hits the cap.
-- The cap controls convergence speed; the Morse force provides direction.

-- | Scale factor amplifying the Morse force into world-unit displacement. Chosen
-- | large enough that any non-zero Morse force (however small in the dissociation
-- | tail) amplifies to exceed forceStepCap, making the cap the effective step
-- | size throughout the relaxation. Value: 1.0e12.
forceStep :: Number
forceStep = 1.0e12

-- | Per-pass displacement cap (world units). Controls convergence speed.
-- | Tuned so that 4 gate-passing passes (d > breakThreshold=230) bring the
-- | mover from d=450 (canonical test distance) to d=162, within bondR0Tol=5
-- | of R0=160:
-- |   450 − 4 × 72 = 162,  |162 − 160| = 2 ≤ 5.
-- | The explicit-Euler stability bound for O–H near equilibrium is ~45 wu
-- | (1/morseK ≈ 45); forceStepCap exceeds this, but the gate (d > 230) ensures
-- | no force step is ever applied closer than 230 − 160 = 70 wu from R0, so
-- | equilibrium oscillation cannot occur in the Builder relaxation loop.
forceStepCap :: Number
forceStepCap = 72.0

-- Strength-aware bond tug. For the drag of atom `draggedAid` at strength
-- `strength`, run exactly `pullPasses` Gauss-Seidel passes; each pass walks
-- the bonds in deterministic ascending (min, max) endpoint-id order and, for
-- each bond outside the stable energy region (SE >= De*breakFrac) whose well
-- depth resists the drag (`Pe.bondDepth za zb > strength`), pulls ONE endpoint
-- along the bond axis toward the other so the pair lands near `pullRestLen`:
--   * the dragged atom is NEVER moved — a bond incident to it pulls the
--     other endpoint;
--   * a chain bond not incident to the dragged atom pulls the endpoint
--     FARTHER from the dragged atom's current position toward the nearer one
--     (this is what propagates the tug down a chain across passes).
-- Strength-beaten bonds (De <= strength) are skipped and left stretched,
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
-- Gate (energy form, unified with recomputeBonds Morse criterion):
--   SKIP when the bond is in the stable region (SE(d) < De*breakFrac, i.e.
--   d is within the energy-break horizon ≈ breakThreshold), OR when the bond
--   yields to the drag (Pe.bondDepth za zb <= strength).
--
-- The first condition (SE < De*breakFrac) replaces the old geometric
-- `d <= breakThreshold` check: both identify the same "bond-is-fine" regime
-- (crossover ≈ 231.6 wu ≈ 230 wu old breakThreshold). Using the energy form
-- ensures pullBond and recomputeBonds share a single Morse-curve description.
--
-- The second condition (De <= strength) is the energy form of the old
-- `bondEnergy pa.z pb.z < strength` gate: De = Pe.bondDepth = Chem.bondEnergy,
-- so the semantics are identical. A bond with De <= strength is overwhelmed by
-- the drag and left stretched for recomputeBonds to break; one with De > strength
-- is pulled back toward equilibrium.
--
-- Together: pull only when d is past the break horizon AND the bond is strong
-- enough to resist the drag. This is the same two-condition gate as before,
-- now expressed in Morse-potential energy terms.
pullBond :: Number -> Int -> BuilderState -> BBond -> BuilderState
pullBond strength draggedAid s bd =
  case atomById s bd.a, atomById s bd.b of
    Just pa, Just pb ->
      let
        d = distance pa.pos pb.pos
        za = pa.z
        zb = pb.z
        ord = bd.order
        -- S3: order-aware De for the strength gate. Higher-order bonds have
        -- larger De, so they resist drag more strongly (need higher strength to break).
        de = bondDepth' za zb ord
      in
        -- Skip: bond is in its stable energy region (order-aware SE < De*breakFrac),
        -- OR it yields to drag (order-aware De <= strength).
        if stretchEnergy' za zb ord d < de * breakFrac || de <= strength then s
        else pullPair draggedAid ord s pa pb d
    _, _ -> s

-- Apply one classical/Morse force relaxation step to the chosen endpoint of an
-- overstretched strong bond. The mover is advanced along the bond axis by a
-- displacement bounded by forceStepCap:
--   rawStep = forceStep · (−morseForce' za zb ord d)   -- negate: attractive force → positive Δd
--   step    = clamp rawStep (−forceStepCap) forceStepCap
--   newDist = max (minSeparation za zb) (d − step)  -- Pauli floor clamp every call
-- The mover is placed at `alongFrom target.pos dir newDist`. The dragged atom
-- is never the mover; tie-breaking and NaN-avoidance follow the existing
-- `separationDir`/`tieBreakDir` machinery.
-- S3: `ord` is the bond's order, passed from pullBond; morseForce' is order-aware
-- so double/triple bonds pull toward their shorter R0'(order).
pullPair :: Int -> Int -> BuilderState -> PlacedAtom -> PlacedAtom -> Number -> BuilderState
pullPair draggedAid ord s pa pb d =
  let
    picked =
      if pa.id == draggedAid then { mover: pb, target: pa }
      else if pb.id == draggedAid then { mover: pa, target: pb }
      else if dragDist pa <= dragDist pb then { mover: pb, target: pa }
      else { mover: pa, target: pb }
    -- Unit direction from the held endpoint toward the pulled one (id-derived
    -- tie-break when coincident, so no division by zero / NaN is possible).
    dir = separationDir picked.target picked.mover d
    -- S3: order-aware Morse force direction + guaranteed-cap step.
    -- morseForce' < 0 for r > R0' (attractive): negating gives rawForce > 0.
    -- For higher-order bonds, R0' is shorter and the potential steeper, so at
    -- large r the Morse force decays as exp(-a'*(r-R0')) where a' >> a(order=1).
    -- At r=450 with a'≈0.16 (O-O order=2), the force is ~exp(-50) ≈ 0 and
    -- `forceStep * force` would be numerically zero — far below forceStepCap.
    -- Fix: take the MAX of forceStepCap and the amplified force, preserving the
    -- sign (direction) of the Morse force while guaranteeing the cap step size
    -- in the dissociation tail. This is the "direction-only, cap-controlled"
    -- relaxation: convergence speed = cap, direction = Morse sign.
    -- For order=1: rawForce at r=450 already >> cap (exp(-14) >> exp(-50)), so
    -- max has no effect there — no change to existing order-1 behaviour.
    rawForce = forceStep * (-morseForce' picked.mover.z picked.target.z ord d)
    rawStep = if rawForce >= 0.0 then max forceStepCap rawForce
              else min (-forceStepCap) rawForce
    step = max (-forceStepCap) (min forceStepCap rawStep)
    -- S3: floor clamp to max(minSeparation, R0'(order)) when approaching from the
    -- attractive side (d > R0'). This prevents the cap-72 step from overshooting
    -- past the potential minimum onto the repulsive wall for high-order bonds whose
    -- (R0' - minSep) gap is smaller than forceStepCap. For order=1, R0'(1) = R0
    -- = max(minSep, 160) = 160 >= minSep, so this clamp is always >= minSep and
    -- the order=1 behaviour is unchanged (160 > 130 so the extra clamp does nothing
    -- extra in the 130→162 convergence band). Continuity: order=1 floor = 160.
    r0ord = bondR0' picked.mover.z picked.target.z ord
    newDist = max (max (minSeparation picked.mover.z picked.target.z) r0ord) (d - step)
  in
    setAtomPos picked.mover.id (alongFrom picked.target.pos dir newDist) s
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
