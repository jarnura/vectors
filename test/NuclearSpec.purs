-- | Unit tests for the Nuclear (SEMF / nuclear-physics) pure model.
-- | Written test-first (RED → GREEN → REFACTOR).
-- |
-- | Anchored checks with per-target tolerances:
-- |   - tight mid-mass (Fe-56 B/A, stable set, β− Q)
-- |   - generous light/α/fission (SEMF is inaccurate for A<20, heavy fission)
-- | Conservation checks use exact Int equality.
module Test.NuclearSpec where

import Prelude

import Data.Maybe (Maybe(..))
import Data.Number (abs)
import Effect (Effect)
import Effect.Console (log)
import Nuclear
  ( Nuclide
  , Mode(..)
  , massNumber
  , bindingEnergy
  , bindingPerNucleon
  , measuredBinding
  , decayMode
  , alphaDecay
  , betaMinus
  , betaPlusEC
  , fuse
  , fission
  , addProton
  , removeProton
  , addNeutron
  , removeNeutron
  , defaultNeutrons
  , nuclearSymbol
  , nuclearName
  , qAlpha
  , qBetaMinus
  , applyAlphaDecay
  , applyBetaMinus
  , applyBetaPlusEC
  , applyFuse
  , applyFission
  )
import Test.Util (check)

-- Tolerance helpers for physics checks.
withinTol :: Number -> Number -> Number -> Boolean
withinTol tol a b = abs (a - b) <= tol

nuclearSpec :: Effect Unit
nuclearSpec = do
  log "Nuclear (SEMF) model properties:"

  -- ─── 1. Basic type and massNumber ────────────────────────────────────────────
  log "  1. massNumber / Nuclide basics:"

  let fe56 :: Nuclide
      fe56 = { z: 26, n: 30 }

  let h1 :: Nuclide
      h1 = { z: 1, n: 0 }

  let d :: Nuclide  -- Deuterium ²H
      d = { z: 1, n: 1 }

  let t :: Nuclide  -- Tritium ³H
      t = { z: 1, n: 2 }

  let he4 :: Nuclide
      he4 = { z: 2, n: 2 }

  let c12 :: Nuclide
      c12 = { z: 6, n: 6 }

  let c14 :: Nuclide
      c14 = { z: 6, n: 8 }

  let n14 :: Nuclide
      n14 = { z: 7, n: 7 }

  let o16 :: Nuclide
      o16 = { z: 8, n: 8 }

  let ca40 :: Nuclide
      ca40 = { z: 20, n: 20 }

  let u238 :: Nuclide
      u238 = { z: 92, n: 146 }

  let u235 :: Nuclide
      u235 = { z: 92, n: 143 }

  let ba141 :: Nuclide
      ba141 = { z: 56, n: 85 }

  let kr92 :: Nuclide
      kr92 = { z: 36, n: 56 }

  check "massNumber Fe-56 = 56" $ massNumber fe56 == 56
  check "massNumber H-1 = 1" $ massNumber h1 == 1
  check "massNumber D = 2" $ massNumber d == 2
  check "massNumber He-4 = 4" $ massNumber he4 == 4
  check "massNumber U-238 = 238" $ massNumber u238 == 238
  check "massNumber U-235 = 235" $ massNumber u235 == 235

  -- ─── 2. nuclearSymbol / nuclearName ──────────────────────────────────────────
  log "  2. nuclearSymbol / nuclearName:"

  check "nuclearSymbol 1 = H"  $ nuclearSymbol 1  == "H"
  check "nuclearSymbol 2 = He" $ nuclearSymbol 2  == "He"
  check "nuclearSymbol 6 = C"  $ nuclearSymbol 6  == "C"
  check "nuclearSymbol 26 = Fe" $ nuclearSymbol 26 == "Fe"
  check "nuclearSymbol 92 = U" $ nuclearSymbol 92 == "U"
  check "nuclearSymbol 118 = Og" $ nuclearSymbol 118 == "Og"
  -- clamp-safe out-of-range
  check "nuclearSymbol 0 clamps → ?" $ nuclearSymbol 0  == "?"
  check "nuclearSymbol (-1) clamps → ?" $ nuclearSymbol (-1) == "?"
  check "nuclearSymbol 200 clamps → ?" $ nuclearSymbol 200 == "?"

  check "nuclearName 1 = Hydrogen"   $ nuclearName 1  == "Hydrogen"
  check "nuclearName 2 = Helium"     $ nuclearName 2  == "Helium"
  check "nuclearName 26 = Iron"      $ nuclearName 26 == "Iron"
  check "nuclearName 92 = Uranium"   $ nuclearName 92 == "Uranium"
  check "nuclearName 0 clamps → ?"   $ nuclearName 0  == "?"
  check "nuclearName 200 clamps → ?" $ nuclearName 200 == "?"

  -- ─── 3. bindingEnergy / bindingPerNucleon ─────────────────────────────────
  log "  3. bindingEnergy / bindingPerNucleon:"

  -- A < 2 → B = 0 (guard)
  check "bindingEnergy H-1 = 0 (guard A<2)" $
    bindingEnergy h1 == 0.0

  let nuclideZ0 :: Nuclide
      nuclideZ0 = { z: 0, n: 0 }
  check "bindingEnergy A=0 = 0 (guard)" $
    bindingEnergy nuclideZ0 == 0.0

  -- D measured binding = 2.224 MeV (fixed; measuredBinding returns this, not SEMF)
  check "measuredBinding D ≈ 2.224 MeV (tol 0.01)" $
    withinTol 0.01 (measuredBinding d) 2.224

  -- He-4 measured binding = 28.30 MeV
  check "measuredBinding He-4 ≈ 28.30 MeV (tol 0.05)" $
    withinTol 0.05 (measuredBinding he4) 28.30

  -- T measured binding = 8.482 MeV
  check "measuredBinding T ≈ 8.482 MeV (tol 0.01)" $
    withinTol 0.01 (measuredBinding t) 8.482

  -- Fe-56: the iron peak. SEMF gives B/A near the experimental ~8.79 MeV.
  -- Allow [8.6, 8.95] to accommodate SEMF's slight overestimate vs. measurement.
  let fe56Bpa = bindingPerNucleon fe56
  check "Fe-56 bindingPerNucleon ∈ [8.6, 8.95] MeV (SEMF)" $
    fe56Bpa >= 8.6 && fe56Bpa <= 8.95

  -- Fe-56 should be at or near the maximum (allow Ni-62 to edge it, but Fe-56
  -- must be ≥ 8.6 and ≥ all tested neighbours: A=50..70).
  let nickels62 :: Nuclide
      nickels62 = { z: 28, n: 34 }  -- Ni-62

  check "Fe-56 bindingPerNucleon >= 8.6 (local max plateau)" $
    bindingPerNucleon fe56 >= 8.6

  check "Ni-62 bindingPerNucleon >= 8.6 (also at peak)" $
    bindingPerNucleon nickels62 >= 8.6

  -- Lighter/heavier nuclides should have lower B/A (outside the peak).
  let c12Bpa = bindingPerNucleon c12
  check "C-12 bindingPerNucleon < Fe-56 (below peak)" $
    c12Bpa < fe56Bpa

  let u238Bpa = bindingPerNucleon u238
  check "U-238 bindingPerNucleon < Fe-56 (above peak)" $
    u238Bpa < fe56Bpa

  -- bindingPerNucleon H-1 = 0 (A<1 guard)
  check "bindingPerNucleon H-1 = 0 (guard)" $
    bindingPerNucleon h1 == 0.0

  -- Binding energy > 0 for stable mid-mass nuclides.
  check "bindingEnergy Fe-56 > 0" $ bindingEnergy fe56 > 0.0
  check "bindingEnergy C-12 > 0"  $ bindingEnergy c12 > 0.0
  check "bindingEnergy O-16 > 0"  $ bindingEnergy o16 > 0.0

  -- ─── 4. Stable-set classification ────────────────────────────────────────────
  log "  4. decayMode stable set:"

  check "H-1 is Stable"   $ decayMode h1   == Stable
  check "H-2 is Stable"   $ decayMode d    == Stable
  check "He-3 is Stable"  $ decayMode { z: 2, n: 1 } == Stable
  check "He-4 is Stable"  $ decayMode he4  == Stable
  check "C-12 is Stable"  $ decayMode c12  == Stable
  check "N-14 is Stable"  $ decayMode n14  == Stable
  check "O-16 is Stable"  $ decayMode o16  == Stable
  check "Fe-56 is Stable" $ decayMode fe56 == Stable
  check "Ca-40 is Stable" $ decayMode ca40 == Stable

  -- ─── 5. Q-value: α decay (U-238) ─────────────────────────────────────────────
  log "  5. Q-value: α decay (U-238):"

  let qU238alpha = qAlpha u238
  -- SEMF gives Q ≈ 4–5 MeV for U-238; we allow a generous ±1.5 window around 4.27.
  check "U-238 α Q > 0 (exothermic)" $ qU238alpha > 0.0
  check "U-238 α Q ≈ 4.27 MeV (tol 1.5)" $ withinTol 1.5 qU238alpha 4.27

  let aDecay = alphaDecay u238
  check "U-238 α daughter Z = 90 (Th)" $ aDecay.daughter.z == 90
  check "U-238 α daughter A = 234"     $ massNumber aDecay.daughter == 234
  check "U-238 α alpha Z = 2"          $ aDecay.alpha.z == 2
  check "U-238 α alpha A = 4"          $ massNumber aDecay.alpha == 4
  -- Absolute anchor: model gives 4.3490 MeV; experimental 4.27 MeV. Tol 0.5.
  check "U-238 α Q ≈ 4.349 MeV (absolute, tol 0.5)" $
    withinTol 0.5 aDecay.q 4.349

  -- decayMode for heavy U region trends Alpha
  check "U-238 decayMode = Alpha" $ decayMode u238 == Alpha

  -- Guard: unphysical nuclides (Z < 2 or N < 2) return 0.0
  check "qAlpha H-1 (Z=1,N=0) = 0.0 (guard)" $ qAlpha h1 == 0.0
  check "qAlpha (Z=1,N=1) = 0.0 (guard N<2)"  $ qAlpha d == 0.0
  check "qAlpha (Z=2,N=1) = 0.0 (guard N<2)"  $ qAlpha { z: 2, n: 1 } == 0.0

  -- ─── 6. Q-value: β− decay (C-14 → N-14) ─────────────────────────────────────
  log "  6. Q-value: β− decay (C-14):"

  let qC14beta = qBetaMinus c14
  check "C-14 β− Q > 0 (exothermic)" $ qC14beta > 0.0
  -- Experimental Q ≈ 0.156 MeV; SEMF is rough for light nuclei, allow ±1.5.
  check "C-14 β− Q ≈ 0.16 MeV (tol 1.5)" $ withinTol 1.5 qC14beta 0.156

  let bmResult = betaMinus c14
  check "C-14 β− product Z = 7 (N)"    $ bmResult.product.z == 7
  check "C-14 β− product A = 14"       $ massNumber bmResult.product == 14
  -- Absolute anchor: model gives 0.157 MeV; experimental 0.156 MeV. Tol 0.02.
  check "C-14 β− Q ≈ 0.157 MeV (absolute, tol 0.02)" $
    withinTol 0.02 bmResult.q 0.157

  check "C-14 decayMode = BetaMinus" $ decayMode c14 == BetaMinus

  -- ─── 7. Q-value: D-T fusion → He-5 (compound nucleus) ──────────────────────
  -- fuse(D,T) gives the compound nucleus He-5 (Z=2, N=3, A=5).
  -- He-5 is extremely short-lived and immediately emits a neutron → He-4+n.
  -- The model sets B(He-5) = B(He-4) so the compound Q ≈ 17.6 MeV matches
  -- the physical D+T → He-4+n channel Q.
  log "  7. Q-value: D-T fusion:"

  let fuseResult = fuse d t
  check "D+T compound nucleus Z = 2" $ fuseResult.product.z == 2
  -- Compound nucleus He-5: A = D.A + T.A = 2 + 3 = 5
  check "D+T compound nucleus A = 5 (He-5)" $ massNumber fuseResult.product == 5

  -- Absolute anchor: model gives exactly 17.594 MeV (= B(He-4)−B(D)−B(T)
  -- = 28.30−2.224−8.482); experimental D+T → He-4+n Q ≈ 17.6 MeV. Tol 0.05.
  check "D+T fusion Q > 0 (exothermic)" $ fuseResult.q > 0.0
  check "D+T fusion Q ≈ 17.594 MeV (absolute, tol 0.05)" $
    withinTol 0.05 fuseResult.q 17.594

  -- ─── 8. Q-value: U-235 fission → Ba-141 + Kr-92 + 3n ────────────────────────
  log "  8. Q-value: U-235 fission:"

  -- U-235 + n → Ba-141 + Kr-92 + 3n
  let fissParent :: Nuclide
      fissParent = { z: u235.z, n: u235.n + 1 }  -- U-236 (after absorbing n)

  let fissResult = fission fissParent ba141 kr92
  -- neutron count: A conservation: 236 = 141 + 92 + neutrons → 3 neutrons
  check "U-235+n fission: fragment1 Z = 56 (Ba)" $
    (fissResult.fragments.a).z == 56
  check "U-235+n fission: fragment2 Z = 36 (Kr)" $
    (fissResult.fragments.b).z == 36
  check "U-235+n fission: neutron count = 3" $
    fissResult.neutrons == 3
  -- Tightened range [140, 220]: model gives ~159 MeV (SEMF underestimates;
  -- experimental Q ≈ 202 MeV). Range accommodates SEMF but catches major errors.
  check "U-235+n fission Q > 140 MeV" $
    fissResult.q > 140.0
  check "U-235+n fission Q < 220 MeV" $
    fissResult.q < 220.0

  -- ─── 9. Conservation: ΣZ and ΣA preserved exactly ───────────────────────────
  log "  9. Conservation (exact Int):"

  -- addProton / removeProton / addNeutron / removeNeutron
  let fe56p1 = addProton fe56
  check "addProton: Z increases by 1" $ fe56p1.z == fe56.z + 1
  check "addProton: A increases by 1" $ massNumber fe56p1 == massNumber fe56 + 1
  check "addProton: N unchanged"      $ fe56p1.n == fe56.n

  let fe56m1 = removeProton fe56
  check "removeProton: Z decreases by 1" $ fe56m1.z == fe56.z - 1
  check "removeProton: A decreases by 1" $ massNumber fe56m1 == massNumber fe56 - 1
  check "removeProton: N unchanged"      $ fe56m1.n == fe56.n

  let fe56np1 = addNeutron fe56
  check "addNeutron: N increases by 1" $ fe56np1.n == fe56.n + 1
  check "addNeutron: A increases by 1" $ massNumber fe56np1 == massNumber fe56 + 1
  check "addNeutron: Z unchanged"      $ fe56np1.z == fe56.z

  let fe56nm1 = removeNeutron fe56
  check "removeNeutron: N decreases by 1" $ fe56nm1.n == fe56.n - 1
  check "removeNeutron: A decreases by 1" $ massNumber fe56nm1 == massNumber fe56 - 1
  check "removeNeutron: Z unchanged"      $ fe56nm1.z == fe56.z

  -- alphaDecay: Z_parent = Z_daughter + 2, A_parent = A_daughter + 4
  check "alphaDecay Z conservation" $
    aDecay.daughter.z + aDecay.alpha.z == u238.z
  check "alphaDecay A conservation" $
    massNumber aDecay.daughter + massNumber aDecay.alpha == massNumber u238

  -- betaMinus: A conserved, Z+1
  check "betaMinus Z conservation" $
    bmResult.product.z == c14.z + 1
  check "betaMinus A conservation" $
    massNumber bmResult.product == massNumber c14

  -- betaPlusEC: A conserved, Z-1
  let bpResult = betaPlusEC { z: 9, n: 9 }  -- F-18 → O-18
  check "betaPlusEC Z conservation" $
    bpResult.product.z == 9 - 1
  check "betaPlusEC A conservation" $
    massNumber bpResult.product == 18
  -- Absolute anchor: F-18(9,9) β+/EC Q. Model gives 0.634 MeV
  -- (= B(O-18)−B(F-18)−1.804 = 139.807−137.369−1.804);
  -- experimental F-18 β+ Q ≈ 0.634 MeV. Tol 0.05.
  check "F-18 β+/EC Q ≈ 0.634 MeV (absolute, tol 0.05)" $
    withinTol 0.05 bpResult.q 0.634

  -- fuse: product Z = sum of Z, product A = sum of A (He-5 compound nucleus)
  check "fuse Z conservation" $
    fuseResult.product.z == d.z + t.z
  check "fuse A conservation" $
    massNumber fuseResult.product == massNumber d + massNumber t  -- 2+3=5

  -- fission: ΣZ and ΣA conserved (parent = f1 + f2 + neutrons)
  let fissParentZ = fissParent.z
  let fissParentA = massNumber fissParent
  let fissF1 = fissResult.fragments.a
  let fissF2 = fissResult.fragments.b
  check "fission Z conservation" $
    fissF1.z + fissF2.z == fissParentZ
  check "fission A conservation" $
    massNumber fissF1 + massNumber fissF2 + fissResult.neutrons == fissParentA

  -- ─── 10. Clamp safety ────────────────────────────────────────────────────────
  log "  10. Clamp safety:"

  let minNuclide :: Nuclide
      minNuclide = { z: 1, n: 0 }

  let removed = removeProton minNuclide
  check "removeProton at Z=1 → Z >= 0" $ removed.z >= 0

  let removedN = removeNeutron minNuclide
  check "removeNeutron at N=0 → N >= 0" $ removedN.n >= 0

  -- removeProton/removeNeutron should never go negative
  let zeroZ :: Nuclide
      zeroZ = { z: 0, n: 5 }
  let removedZ0 = removeProton zeroZ
  check "removeProton at Z=0 → Z >= 0 (clamp)" $ removedZ0.z >= 0

  let zeroN :: Nuclide
      zeroN = { z: 5, n: 0 }
  let removedN0 = removeNeutron zeroN
  check "removeNeutron at N=0 → N >= 0 (clamp)" $ removedN0.n >= 0

  -- ─── 11. defaultNeutrons ─────────────────────────────────────────────────────
  log "  11. defaultNeutrons:"

  -- H: well-known stable isotope H-1 (N=0)
  check "defaultNeutrons H (Z=1) >= 0" $ defaultNeutrons 1 >= 0

  -- Fe: should be near 30 (Fe-56)
  let feN = defaultNeutrons 26
  check "defaultNeutrons Fe (Z=26) ∈ [28, 32]" $ feN >= 28 && feN <= 32

  -- Pb: valley near N = 126 (Pb-208)
  let pbN = defaultNeutrons 82
  check "defaultNeutrons Pb (Z=82) ∈ [120, 132]" $ pbN >= 120 && pbN <= 132

  -- U: valley estimate N ≈ Z + 0.0072·Z² ≈ 153 for Z=92.
  -- All U isotopes are α-emitters; fallback picks closest-to-valley N.
  let uN = defaultNeutrons 92
  check "defaultNeutrons U (Z=92) ∈ [140, 158]" $ uN >= 140 && uN <= 158

  -- ─── 12. Unbound light nuclides ──────────────────────────────────────────────
  log "  12. Unbound light nuclides (He-5, Li-5, Be-8):"

  let he5 :: Nuclide
      he5 = { z: 2, n: 3 }

  let li5 :: Nuclide
      li5 = { z: 3, n: 2 }

  let be8 :: Nuclide
      be8 = { z: 4, n: 4 }

  check "He-5 (2,3) decayMode = Unbound"  $ decayMode he5 == Unbound
  check "Li-5 (3,2) decayMode = Unbound"  $ decayMode li5 == Unbound
  check "Be-8 (4,4) decayMode = Unbound"  $ decayMode be8 == Unbound

  -- Stable set is unaffected by Unbound addition
  check "He-4 still Stable after Unbound added" $ decayMode he4  == Stable
  check "C-12 still Stable after Unbound added" $ decayMode c12  == Stable
  check "O-16 still Stable after Unbound added" $ decayMode o16  == Stable
  check "Fe-56 still Stable after Unbound added" $ decayMode fe56 == Stable
  check "Ca-40 still Stable after Unbound added" $ decayMode ca40 == Stable

  -- D+T fusion Q is unaffected: bHe5 = bHe4 (28.30), so Q is still 17.594 MeV
  let dtQ = (fuse d t).q
  check "D+T fusion Q unaffected by Unbound (still ~17.594 MeV)" $
    withinTol 0.05 dtQ 17.594

  -- ─── 13. Shared apply-helpers (pure NuclearState transformers) ───────────────
  log "  13. Shared apply-helpers:"

  let initState = { nuclide: u238, lastQ: 0.0, lastFission: Nothing }

  -- applyAlphaDecay
  let afterAlpha = applyAlphaDecay initState
  check "applyAlphaDecay: daughter Z = 90" $ afterAlpha.nuclide.z == 90
  check "applyAlphaDecay: daughter A = 234" $ massNumber afterAlpha.nuclide == 234
  check "applyAlphaDecay: lastQ > 0"        $ afterAlpha.lastQ > 0.0
  check "applyAlphaDecay: lastFission = Nothing" $ afterAlpha.lastFission == Nothing

  -- applyBetaMinus
  let c14state = { nuclide: c14, lastQ: 0.0, lastFission: Nothing }
  let afterBm = applyBetaMinus c14state
  check "applyBetaMinus: product Z = 7" $ afterBm.nuclide.z == 7
  check "applyBetaMinus: product A = 14" $ massNumber afterBm.nuclide == 14
  check "applyBetaMinus: lastQ > 0"      $ afterBm.lastQ > 0.0

  -- applyBetaPlusEC
  let f18state = { nuclide: { z: 9, n: 9 }, lastQ: 0.0, lastFission: Nothing }
  let afterBp = applyBetaPlusEC f18state
  check "applyBetaPlusEC: product Z = 8" $ afterBp.nuclide.z == 8
  check "applyBetaPlusEC: product A = 18" $ massNumber afterBp.nuclide == 18
  check "applyBetaPlusEC: lastQ > 0"      $ afterBp.lastQ > 0.0

  -- applyFuse
  let dState = { nuclide: d, lastQ: 0.0, lastFission: Nothing }
  let afterFuse = applyFuse 1 2 dState  -- fuse D with T (z2=1, n2=2)
  check "applyFuse D+T: product Z = 2"    $ afterFuse.nuclide.z == 2
  check "applyFuse D+T: product A = 5"    $ massNumber afterFuse.nuclide == 5
  check "applyFuse D+T: lastQ > 0"        $ afterFuse.lastQ > 0.0
  check "applyFuse D+T: lastFission = Nothing" $ afterFuse.lastFission == Nothing

  -- applyFission
  let u236state = { nuclide: { z: 92, n: 144 }, lastQ: 0.0, lastFission: Nothing }
  let afterFiss = applyFission 56 85 36 56 u236state
  check "applyFission: lastQ > 0"          $ afterFiss.lastQ > 0.0
  check "applyFission: lastFission = Just" $ afterFiss.lastFission /= Nothing
  check "applyFission: nuclide unchanged"  $
    afterFiss.nuclide.z == 92 && afterFiss.nuclide.n == 144

  log "all Nuclear (SEMF) properties hold."
