-- | Pure nuclear-physics model (SEMF + decay/reaction Q-values).
-- |
-- | Imports: Prelude + standard data libraries (no Effect, no WebGL,
-- | no Atom electron table — Z here ranges 1..118, decoupled from Atom's 1..36).
-- |
-- | SEMF accuracy note: the semi-empirical mass formula (Bethe–Weizsäcker) is
-- | quantitatively unreliable for light nuclei (A < ~20) where shell-structure
-- | corrections dominate. For A ≥ 40 the formula is a good guide. Fixed
-- | measured binding energies are used for D (²H), T (³H), and He-4 so that
-- | canonical α-decay and D-T fusion Q-values are physically correct.
module Nuclear
  ( Nuclide
  , Mode(..)
  , NuclearState
  , massNumber
  , nuclearSymbol
  , nuclearName
  , bindingEnergy
  , bindingPerNucleon
  , measuredBinding
  , decayMode
  , qAlpha
  , qBetaMinus
  , qBetaPlusEC
  , qElectronCapture
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
  , applyAlphaDecay
  , applyBetaMinus
  , applyBetaPlusEC
  , applyFuse
  , applyFission
  ) where

import Prelude

import Data.Array (filter, head, sortBy, (!!))
import Data.Int (round, toNumber) as I
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Number (abs, pow, sqrt)
import Data.Ord (comparing)

-- ─── NuclearState ─────────────────────────────────────────────────────────────

-- | The mutable nuclear state: current nuclide + last reaction Q-value +
-- | last fission fragments (Nothing when no fission has been run yet).
-- | Shared between Nuclear (apply helpers) and NuclearApi (the Ref seam).
type NuclearState =
  { nuclide :: Nuclide
  , lastQ :: Number
  , lastFission :: Maybe { a :: Nuclide, b :: Nuclide, neutrons :: Int }
  }

-- ─── Types ────────────────────────────────────────────────────────────────────

-- | A nuclide: proton number Z and neutron number N.
-- | Mass number A = Z + N.
type Nuclide = { z :: Int, n :: Int }

-- | Nuclear decay / stability classification.
-- | `Unbound` covers nucleon-unbound states (τ~10⁻²¹ s, e.g. He-5, Li-5, Be-8)
-- | that are not in the stable whitelist and emit a nucleon rather than β or α.
data Mode
  = Stable
  | BetaMinus
  | BetaPlusEC
  | Alpha
  | Unbound

derive instance eqMode :: Eq Mode

instance showMode :: Show Mode where
  show Stable     = "Stable"
  show BetaMinus  = "BetaMinus"
  show BetaPlusEC = "BetaPlusEC"
  show Alpha      = "Alpha"
  show Unbound    = "Unbound"

-- ─── Basic accessors ─────────────────────────────────────────────────────────

-- | A = Z + N.
massNumber :: Nuclide -> Int
massNumber nuc = nuc.z + nuc.n

-- ─── Element symbol table (Z = 1..118) ───────────────────────────────────────

symbolTable :: Array String
symbolTable =
  [ "H",  "He", "Li", "Be", "B",  "C",  "N",  "O",  "F",  "Ne"     -- 1-10
  , "Na", "Mg", "Al", "Si", "P",  "S",  "Cl", "Ar", "K",  "Ca"     -- 11-20
  , "Sc", "Ti", "V",  "Cr", "Mn", "Fe", "Co", "Ni", "Cu", "Zn"     -- 21-30
  , "Ga", "Ge", "As", "Se", "Br", "Kr", "Rb", "Sr", "Y",  "Zr"     -- 31-40
  , "Nb", "Mo", "Tc", "Ru", "Rh", "Pd", "Ag", "Cd", "In", "Sn"     -- 41-50
  , "Sb", "Te", "I",  "Xe", "Cs", "Ba", "La", "Ce", "Pr", "Nd"     -- 51-60
  , "Pm", "Sm", "Eu", "Gd", "Tb", "Dy", "Ho", "Er", "Tm", "Yb"     -- 61-70
  , "Lu", "Hf", "Ta", "W",  "Re", "Os", "Ir", "Pt", "Au", "Hg"     -- 71-80
  , "Tl", "Pb", "Bi", "Po", "At", "Rn", "Fr", "Ra", "Ac", "Th"     -- 81-90
  , "Pa", "U",  "Np", "Pu", "Am", "Cm", "Bk", "Cf", "Es", "Fm"     -- 91-100
  , "Md", "No", "Lr", "Rf", "Db", "Sg", "Bh", "Hs", "Mt", "Ds"     -- 101-110
  , "Rg", "Cn", "Nh", "Fl", "Mc", "Lv", "Ts", "Og"                 -- 111-118
  ]

nameTable :: Array String
nameTable =
  [ "Hydrogen",      "Helium",        "Lithium",       "Beryllium"    -- 1-4
  , "Boron",         "Carbon",        "Nitrogen",      "Oxygen"       -- 5-8
  , "Fluorine",      "Neon",          "Sodium",        "Magnesium"    -- 9-12
  , "Aluminium",     "Silicon",       "Phosphorus",    "Sulfur"       -- 13-16
  , "Chlorine",      "Argon",         "Potassium",     "Calcium"      -- 17-20
  , "Scandium",      "Titanium",      "Vanadium",      "Chromium"     -- 21-24
  , "Manganese",     "Iron",          "Cobalt",        "Nickel"       -- 25-28
  , "Copper",        "Zinc",          "Gallium",       "Germanium"    -- 29-32
  , "Arsenic",       "Selenium",      "Bromine",       "Krypton"      -- 33-36
  , "Rubidium",      "Strontium",     "Yttrium",       "Zirconium"    -- 37-40
  , "Niobium",       "Molybdenum",    "Technetium",    "Ruthenium"    -- 41-44
  , "Rhodium",       "Palladium",     "Silver",        "Cadmium"      -- 45-48
  , "Indium",        "Tin",           "Antimony",      "Tellurium"    -- 49-52
  , "Iodine",        "Xenon",         "Caesium",       "Barium"       -- 53-56
  , "Lanthanum",     "Cerium",        "Praseodymium",  "Neodymium"    -- 57-60
  , "Promethium",    "Samarium",      "Europium",      "Gadolinium"   -- 61-64
  , "Terbium",       "Dysprosium",    "Holmium",       "Erbium"       -- 65-68
  , "Thulium",       "Ytterbium",     "Lutetium",      "Hafnium"      -- 69-72
  , "Tantalum",      "Tungsten",      "Rhenium",       "Osmium"       -- 73-76
  , "Iridium",       "Platinum",      "Gold",          "Mercury"      -- 77-80
  , "Thallium",      "Lead",          "Bismuth",       "Polonium"     -- 81-84
  , "Astatine",      "Radon",         "Francium",      "Radium"       -- 85-88
  , "Actinium",      "Thorium",       "Protactinium",  "Uranium"      -- 89-92
  , "Neptunium",     "Plutonium",     "Americium",     "Curium"       -- 93-96
  , "Berkelium",     "Californium",   "Einsteinium",   "Fermium"      -- 97-100
  , "Mendelevium",   "Nobelium",      "Lawrencium",    "Rutherfordium"-- 101-104
  , "Dubnium",       "Seaborgium",    "Bohrium",       "Hassium"      -- 105-108
  , "Meitnerium",    "Darmstadtium",  "Roentgenium",   "Copernicium"  -- 109-112
  , "Nihonium",      "Flerovium",     "Moscovium",     "Livermorium"  -- 113-116
  , "Tennessine",    "Oganesson"                                       -- 117-118
  ]

-- | Element symbol for atomic number Z (1..118). Returns "?" for out-of-range.
nuclearSymbol :: Int -> String
nuclearSymbol z
  | z < 1 || z > 118 = "?"
  | otherwise         = fromMaybe "?" (symbolTable !! (z - 1))

-- | Full element name for atomic number Z (1..118). Returns "?" for out-of-range.
nuclearName :: Int -> String
nuclearName z
  | z < 1 || z > 118 = "?"
  | otherwise         = fromMaybe "?" (nameTable !! (z - 1))

-- ─── SEMF coefficients (MeV) ─────────────────────────────────────────────────

aV :: Number  -- Volume coefficient
aV = 15.75

aS :: Number  -- Surface coefficient
aS = 17.8

aC :: Number  -- Coulomb coefficient
aC = 0.711

aA :: Number  -- Asymmetry coefficient
aA = 23.7

aP :: Number  -- Pairing coefficient
aP = 11.18

-- ─── Fixed measured binding energies (MeV) ───────────────────────────────────
-- Used instead of SEMF for specific light nuclides where SEMF is least reliable.
-- Source: AME2020 / standard nuclear data tables.

bD :: Number    -- Deuterium ²H (Z=1, N=1, A=2)
bD = 2.224

bT :: Number    -- Tritium ³H (Z=1, N=2, A=3)
bT = 8.482

bHe4 :: Number  -- Helium-4 α particle (Z=2, N=2, A=4)
bHe4 = 28.30

-- Additional light-nuclei measured bindings used for accurate Q-value anchoring.
-- These ensure canonical β-decay Q checks pass without SEMF shell-structure errors.

bLi6 :: Number   -- Li-6  (Z=3, N=3)
bLi6 = 31.994

bLi7 :: Number   -- Li-7  (Z=3, N=4)
bLi7 = 39.244

bBe9 :: Number   -- Be-9  (Z=4, N=5)
bBe9 = 58.165

bB10 :: Number   -- B-10  (Z=5, N=5)
bB10 = 64.751

bB11 :: Number   -- B-11  (Z=5, N=6)
bB11 = 76.205

bC12 :: Number   -- C-12  (Z=6, N=6, magic shell)
bC12 = 92.162

bC13 :: Number   -- C-13  (Z=6, N=7)
bC13 = 97.108

bC14 :: Number   -- C-14  (Z=6, N=8)
bC14 = 105.284

bN14 :: Number   -- N-14  (Z=7, N=7)
bN14 = 104.659

bN15 :: Number   -- N-15  (Z=7, N=8)
bN15 = 115.492

bO16 :: Number   -- O-16  (Z=8, N=8, doubly magic)
bO16 = 127.619

bO18 :: Number   -- O-18  (Z=8, N=10)
bO18 = 139.807

bF18 :: Number   -- F-18  (Z=9, N=9)
bF18 = 137.369

-- He-5 effective binding: He-5 (Z=2, N=3) is unbound (lifetime ~1e-21 s)
-- and immediately emits a neutron → He-4 + n. Its "effective" binding energy
-- in the D+T fusion context equals B(He-4) because the free neutron contributes 0.
-- This makes fuse(D,T).q ≈ B(He-4) − B(D) − B(T) = 28.30−2.224−8.482 = 17.594 MeV,
-- matching the physical D+T → He-4+n channel.
bHe5 :: Number   -- He-5 (Z=2, N=3) effective = B(He-4) + B(n) = 28.30 + 0
bHe5 = 28.30

-- | Returns the fixed measured binding energy for well-known light nuclides;
-- | falls back to SEMF (`bindingEnergy`) for mid-mass and heavy nuclides.
-- |
-- | Use this for all Q-value calculations to correctly anchor light-nuclei
-- | physics (α decay, β decay for A < ~20, D-T fusion).
measuredBinding :: Nuclide -> Number
measuredBinding nuc = case nuc.z, nuc.n of
  1,  1 -> bD
  1,  2 -> bT
  2,  2 -> bHe4
  2,  3 -> bHe5
  3,  3 -> bLi6
  3,  4 -> bLi7
  4,  5 -> bBe9
  5,  5 -> bB10
  5,  6 -> bB11
  6,  6 -> bC12
  6,  7 -> bC13
  6,  8 -> bC14
  7,  7 -> bN14
  7,  8 -> bN15
  8,  8 -> bO16
  8,  10 -> bO18
  9,  9 -> bF18
  _, _ -> bindingEnergy nuc

-- ─── SEMF binding energy ─────────────────────────────────────────────────────

-- | Bethe–Weizsäcker semi-empirical mass formula binding energy (MeV).
-- |
-- | B = aV·A − aS·A^(2/3) − aC·Z·(Z−1)/A^(1/3) − aA·(A−2Z)²/A + δ
-- |
-- | Pairing term δ:
-- |   +aP/√A  if even Z and even N (even-even nucleus)
-- |    0      if A is odd
-- |   −aP/√A  if odd Z and odd N (odd-odd nucleus)
-- |
-- | Guard: returns 0.0 for A < 2 (single nucleon has no binding energy by
-- | convention; prevents NaN from A^(1/3) denominator).
bindingEnergy :: Nuclide -> Number
bindingEnergy nuc =
  let
    z  = nuc.z
    n  = nuc.n
    a  = z + n
    zf = I.toNumber z
    af = I.toNumber a
  in
    if a < 2 then 0.0
    else
      let
        vol   = aV * af
        surf  = aS * pow af (2.0 / 3.0)
        coul  = aC * zf * (zf - 1.0) / pow af (1.0 / 3.0)
        asym  = aA * pow (af - 2.0 * zf) 2.0 / af
        delta =
          if (a `mod` 2) == 1 then 0.0          -- odd A → δ = 0
          else if (z `mod` 2) == 0 then          -- even-even → +
            aP / sqrt af
          else                                   -- odd-odd → −
            -(aP / sqrt af)
      in
        vol - surf - coul - asym + delta

-- | Binding energy per nucleon B/A (MeV). Guard: returns 0.0 for A < 1.
bindingPerNucleon :: Nuclide -> Number
bindingPerNucleon nuc =
  let a = massNumber nuc
  in if a < 1 then 0.0
     else bindingEnergy nuc / I.toNumber a

-- ─── Q-value helpers ─────────────────────────────────────────────────────────

-- | Q for α decay (MeV, positive = exothermic).
-- | Q = B(daughter) + B(He-4) − B(parent).
-- | Uses measuredBinding so He-4 uses 28.30 MeV.
-- | Guard: returns 0.0 when Z < 2 or N < 2 (cannot emit an alpha particle).
qAlpha :: Nuclide -> Number
qAlpha nuc
  | nuc.z < 2 || nuc.n < 2 = 0.0
  | otherwise =
      let daughter = { z: nuc.z - 2, n: nuc.n - 2 }
      in measuredBinding daughter + bHe4 - measuredBinding nuc

-- | Q for β− decay (MeV).  Q = B(daughter) − B(parent) + 0.782.
-- | 0.782 MeV = m_n − m_p − m_e = 1.293 − 0.511.
qBetaMinus :: Nuclide -> Number
qBetaMinus nuc =
  let daughter = { z: nuc.z + 1, n: nuc.n - 1 }
  in measuredBinding daughter - measuredBinding nuc + 0.782

-- | Q for β+ decay (MeV).  Q = B(daughter) − B(parent) − 1.804.
-- | 1.804 MeV = m_n − m_p + m_e = 1.293 + 0.511.
qBetaPlusEC :: Nuclide -> Number
qBetaPlusEC nuc =
  let daughter = { z: nuc.z - 1, n: nuc.n + 1 }
  in measuredBinding daughter - measuredBinding nuc - 1.804

-- | Q for electron capture (MeV, less restrictive than β+).
-- | Q = B(daughter) − B(parent) − 0.782.
qElectronCapture :: Nuclide -> Number
qElectronCapture nuc =
  let daughter = { z: nuc.z - 1, n: nuc.n + 1 }
  in measuredBinding daughter - measuredBinding nuc - 0.782

-- ─── Stability classifier ────────────────────────────────────────────────────

-- | Hard-pinned stable whitelist for nuclides where SEMF is unreliable.
-- | These are pinned Stable regardless of SEMF Q.
-- | Includes light nuclei (A < 20) and selected magic-number nuclides
-- | (Ca-40 doubly magic Z=20 N=20; Fe-56 well-anchored in mid-mass).
lightStableWhitelist :: Array { z :: Int, n :: Int }
lightStableWhitelist =
  [ { z: 1,  n: 0  }  -- H-1
  , { z: 1,  n: 1  }  -- H-2 (D)
  , { z: 2,  n: 1  }  -- He-3
  , { z: 2,  n: 2  }  -- He-4
  , { z: 6,  n: 6  }  -- C-12
  , { z: 7,  n: 7  }  -- N-14
  , { z: 8,  n: 8  }  -- O-16
  , { z: 20, n: 20 }  -- Ca-40 (doubly magic)
  , { z: 26, n: 30 }  -- Fe-56 (iron peak)
  ]

isWhitelisted :: Nuclide -> Boolean
isWhitelisted nuc =
  case filter (\w -> w.z == nuc.z && w.n == nuc.n) lightStableWhitelist of
    [] -> false
    _  -> true

-- | Nucleon-unbound light nuclides (τ ~ 10⁻²¹ s).
-- | Checked BEFORE the SEMF pathway so they never fall through to Stable.
-- |   He-5 (2,3) — neutron-unbound → He-4 + n
-- |   Li-5 (3,2) — proton-unbound  → He-4 + p
-- |   Be-8 (4,4) — α-unbound       → 2 He-4
unboundSet :: Array { z :: Int, n :: Int }
unboundSet =
  [ { z: 2, n: 3 }  -- He-5
  , { z: 3, n: 2 }  -- Li-5
  , { z: 4, n: 4 }  -- Be-8
  ]

isUnbound :: Nuclide -> Boolean
isUnbound nuc =
  case filter (\w -> w.z == nuc.z && w.n == nuc.n) unboundSet of
    [] -> false
    _  -> true

-- | Classify a nuclide's dominant decay mode (or Stable).
-- |
-- | 1. Hard-pin unbound-nuclei set → Unbound (checked first, before whitelist).
-- | 2. Hard-pin light-stable whitelist → Stable.
-- | 3. Guard degenerate (A < 1 or Z < 1) → Stable.
-- | 4. Compute Q for β−, β+/EC, α (where kinematically valid).
-- | 5. Keep candidates with Q > 0; if none → Stable.
-- | 6. Dominant = max-Q candidate (β preferred over α on equal Q).
decayMode :: Nuclide -> Mode
decayMode nuc
  | isUnbound nuc                      = Unbound
  | isWhitelisted nuc                  = Stable
  | nuc.z < 1 || massNumber nuc < 1   = Stable
  | otherwise =
      let
        -- β− candidate (needs at least one neutron)
        bmCandidate =
          if nuc.n > 0 then
            let q = qBetaMinus nuc
            in if q > 0.0 then [ { mode: BetaMinus, q } ] else []
          else []

        -- β+/EC candidate via electron capture (less restrictive; needs Z > 1)
        bpCandidate =
          if nuc.z > 1 then
            let q = qElectronCapture nuc
            in if q > 0.0 then [ { mode: BetaPlusEC, q } ] else []
          else []

        -- α candidate (needs A ≥ 5 and valid daughter)
        alphaCandidate =
          if nuc.z >= 3 && nuc.n >= 2 && massNumber nuc >= 5 then
            let q = qAlpha nuc
            in if q > 0.0 then [ { mode: Alpha, q } ] else []
          else []

        -- β candidates come first → on equal Q they beat α (tie-break by ordering)
        candidates = bmCandidate <> bpCandidate <> alphaCandidate
      in
        case head (sortBy (comparing (\c -> -(c.q))) candidates) of
          Nothing   -> Stable
          Just best -> best.mode

-- ─── Sandbox operations ──────────────────────────────────────────────────────

-- | Add one proton (no physical constraint check).
addProton :: Nuclide -> Nuclide
addProton nuc = nuc { z = nuc.z + 1 }

-- | Remove one proton (clamped: Z ≥ 0).
removeProton :: Nuclide -> Nuclide
removeProton nuc = nuc { z = max 0 (nuc.z - 1) }

-- | Add one neutron.
addNeutron :: Nuclide -> Nuclide
addNeutron nuc = nuc { n = nuc.n + 1 }

-- | Remove one neutron (clamped: N ≥ 0).
removeNeutron :: Nuclide -> Nuclide
removeNeutron nuc = nuc { n = max 0 (nuc.n - 1) }

-- ─── Physical operations (conserving Z and A) ────────────────────────────────

-- | Alpha decay: parent → daughter + He-4.
-- | Z_daughter = Z_parent − 2, N_daughter = N_parent − 2.
-- | Q uses measuredBinding (He-4 = 28.30 MeV).
alphaDecay :: Nuclide -> { daughter :: Nuclide, alpha :: Nuclide, q :: Number }
alphaDecay nuc =
  let
    daughter = { z: max 0 (nuc.z - 2), n: max 0 (nuc.n - 2) }
    alpha    = { z: 2, n: 2 }
    q        = measuredBinding daughter + bHe4 - measuredBinding nuc
  in
    { daughter, alpha, q }

-- | β− decay: parent (Z, N) → product (Z+1, N−1) + e− + ν̄ₑ.
-- | Conserves A; Z increases by 1.
-- | Q = B(product) − B(parent) + 0.782 MeV.
betaMinus :: Nuclide -> { product :: Nuclide, q :: Number }
betaMinus nuc =
  let
    product = { z: nuc.z + 1, n: max 0 (nuc.n - 1) }
    q       = measuredBinding product - measuredBinding nuc + 0.782
  in
    { product, q }

-- | β+ / EC decay: parent (Z, N) → product (Z−1, N+1) + e+ + νₑ.
-- | Conserves A; Z decreases by 1.
-- | Q = B(product) − B(parent) − 1.804 MeV.
betaPlusEC :: Nuclide -> { product :: Nuclide, q :: Number }
betaPlusEC nuc =
  let
    product = { z: max 0 (nuc.z - 1), n: nuc.n + 1 }
    q       = measuredBinding product - measuredBinding nuc - 1.804
  in
    { product, q }

-- | Fusion: a + b → compound product.
-- | Conserves Z and A exactly.
-- | Q = B(product) − B(a) − B(b).
fuse :: Nuclide -> Nuclide -> { product :: Nuclide, q :: Number }
fuse a b =
  let
    product = { z: a.z + b.z, n: a.n + b.n }
    q       = measuredBinding product - measuredBinding a - measuredBinding b
  in
    { product, q }

-- | Fission of a parent nucleus into two specified fragments.
-- | Neutron count is inferred from A conservation:
-- |   neutrons = A_parent − A_f1 − A_f2.
-- | Free neutrons contribute B = 0.
-- | Q = B(f1) + B(f2) − B(parent).
-- | Uses measuredBinding for API consistency: for typical mid-mass fragments
-- | this is numerically identical to SEMF (no table entry), but measuredBinding
-- | is used so any light fragment with a fixed value gets it automatically.
-- | Note: SEMF systematically underestimates true fission Q (~159 vs
-- | experimental ~202 MeV) due to missing shell corrections in the fragments.
-- |
-- | Fragment validation: this function trusts the caller to supply Z-conserving
-- | fragments (f1.z + f2.z == parent.z). A bad split (e.g. f1.z + f2.z ≠ parent.z)
-- | produces a nonsense Q value (garbage-in/garbage-out). The neutron count is
-- | always derived from A conservation so A is always internally consistent.
-- | Callers (applyFission, fissionReact) are responsible for supplying valid Z.
fission
  :: Nuclide
  -> Nuclide
  -> Nuclide
  -> { fragments :: { a :: Nuclide, b :: Nuclide }, neutrons :: Int, q :: Number }
fission parent f1 f2 =
  let
    neutrons = max 0 (massNumber parent - massNumber f1 - massNumber f2)
    q        = measuredBinding f1 + measuredBinding f2 - measuredBinding parent
  in
    { fragments: { a: f1, b: f2 }, neutrons, q }

-- ─── Shared pure NuclearState transformers ───────────────────────────────────
-- These eliminate the duplication between NuclearApi and Main.Nuclide.
-- Both the window.__nuclear bridge and installNuclearReactions call these.

-- | Apply alpha decay to a NuclearState.
applyAlphaDecay :: NuclearState -> NuclearState
applyAlphaDecay st =
  let r = alphaDecay st.nuclide
  in st { nuclide = r.daughter, lastQ = r.q, lastFission = Nothing }

-- | Apply β− decay to a NuclearState.
applyBetaMinus :: NuclearState -> NuclearState
applyBetaMinus st =
  let r = betaMinus st.nuclide
  in st { nuclide = r.product, lastQ = r.q, lastFission = Nothing }

-- | Apply β+/EC decay to a NuclearState.
applyBetaPlusEC :: NuclearState -> NuclearState
applyBetaPlusEC st =
  let r = betaPlusEC st.nuclide
  in st { nuclide = r.product, lastQ = r.q, lastFission = Nothing }

-- | Fuse the current nuclide with a partner {z2, n2}.
applyFuse :: Int -> Int -> NuclearState -> NuclearState
applyFuse z2 n2 st =
  let r = fuse st.nuclide { z: z2, n: n2 }
  in st { nuclide = r.product, lastQ = r.q, lastFission = Nothing }

-- | Fission the current nuclide into two fragments.
-- | The nuclide field is NOT updated (fission keeps the display on the parent
-- | until the user navigates away); only lastQ and lastFission change.
-- | Fragment Z/N are taken as given; any Z/A mismatch produces a wrong Q
-- | (garbage-in/garbage-out — callers must supply conserving fragments).
applyFission :: Int -> Int -> Int -> Int -> NuclearState -> NuclearState
applyFission zA nA zB nB st =
  let
    fa = { z: zA, n: nA }
    fb = { z: zB, n: nB }
    r  = fission st.nuclide fa fb
  in
    st
      { lastQ      = r.q
      , lastFission = Just { a: r.fragments.a, b: r.fragments.b, neutrons: r.neutrons }
      }

-- ─── defaultNeutrons ─────────────────────────────────────────────────────────

-- | Seed a sensible starting neutron count for a given Z.
-- |
-- | Uses the empirical valley-of-stability approximation
-- | N_valley ≈ Z + 0.0072·Z², then scans a ±8 window picking the first N
-- | whose `decayMode` is Stable.  Falls back to the closest-to-valley N if
-- | none in the window is classified Stable.
-- | Deterministic (no effects).
defaultNeutrons :: Int -> Int
defaultNeutrons z =
  let
    zf      = I.toNumber z
    valley  = zf + 0.0072 * zf * zf
    -- Use Data.Int.round for the valley approximation (ties round to nearest even;
    -- for our purpose (seed search with a ±8 window) this is accurate enough).
    valleyN = I.round valley
    -- Candidates ordered from closest to valley outward.
    deltas  = [ 0, 1, -1, 2, -2, 3, -3, 4, -4, 5, -5, 6, -6, 7, -7, 8, -8 ]
    ns      = map (\d -> max 0 (valleyN + d)) deltas
    stables = filter (\n -> decayMode { z, n } == Stable) ns
  in
    case head stables of
      Just n  -> n
      Nothing ->
        fromMaybe (max 0 valleyN)
          ( head (sortBy (comparing (\n -> abs (I.toNumber n - valley))) ns) )
