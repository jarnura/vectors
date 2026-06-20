module Test.MoleculeChemSpec where

import Prelude

import Data.Array (all, any, find, index, length)
import Data.Foldable (sum)
import Data.Int (toNumber)
import Data.Maybe (fromMaybe, isJust)
import Data.Number (abs)
import Effect (Effect)
import Effect.Console (log)
import Atom as Atom
import Molecule (bondLength, moleculeOf, molecules, moleculeNucleons, sharedElectronPositions)
import Test.Util (approxEq, check)

moleculeChemSpec :: Effect Unit
moleculeChemSpec = do
  -- ───── Molecule model (H₂ slice) ────────────────────────────────────
  log "molecule model properties:"

  let
    h2 = moleculeOf 0
    halfLen = bondLength / 2.0
    -- Read each atom's center; default to origin so a missing atom fails loudly.
    centerAt i = (fromMaybe { element: 0, center: { x: 0.0, y: 0.0, z: 0.0 } } (index h2.atoms i)).center
    a0c = centerAt 0
    a1c = centerAt 1
    bond0 = fromMaybe { a: -1, b: -1, order: 0, shared: 0 } (index h2.bonds 0)
    sharedX f = map _.x (sharedElectronPositions h2 f)
    meanOf xs = sum xs / toNumber (max 1 (length xs))

  -- H₂ has two hydrogen atoms.
  check "moleculeOf 0 has 2 atoms" $ length h2.atoms == 2
  check "moleculeOf 0 atoms are both Hydrogen (element 1)" $
    all (\m -> m.element == 1) h2.atoms

  -- A single covalent bond between atom 0 and atom 1, sharing 2 electrons.
  check "moleculeOf 0 has 1 bond" $ length h2.bonds == 1
  check "moleculeOf 0 bond == {a:0,b:1,order:1,shared:2}" $
    bond0.a == 0 && bond0.b == 1 && bond0.order == 1 && bond0.shared == 2

  -- Atoms sit symmetric on the x axis at ±bondLength/2, with y=z=0.
  check "H₂ atom 0 center x ≈ -(bondLength/2)" $ approxEq a0c.x (-halfLen)
  check "H₂ atom 1 center x ≈ +(bondLength/2)" $ approxEq a1c.x halfLen
  check "H₂ atom centers y ≈ 0" $ approxEq a0c.y 0.0 && approxEq a1c.y 0.0
  check "H₂ atom centers z ≈ 0" $ approxEq a0c.z 0.0 && approxEq a1c.z 0.0

  -- Properties: non-empty, includes the formula row plus covalent/length/energy/
  -- shared rows (≥4), with at least one row about shared/bonding electrons.
  check "H₂ properties are non-empty" $ length h2.properties > 0
  check "H₂ properties contain {label:Formula, value:H₂}" $
    any (\p -> p.label == "Formula" && p.value == "H₂") h2.properties
  check "H₂ has the formula + covalent/length/energy/shared rows (≥4)" $
    length h2.properties >= 4
  check "H₂ properties mention shared electrons" $
    isJust (find (\p -> p.label == "Shared") h2.properties)

  -- Shared electrons sit in the internuclear overlap region, centred near x=0.
  check "H₂ shared electrons count == 2" $
    length (sharedElectronPositions h2 0.0) == 2
  check "H₂ shared electrons lie within [-half,+half] in x" $
    all (\p -> p.x >= -halfLen - 1.0e-6 && p.x <= halfLen + 1.0e-6)
      (sharedElectronPositions h2 0.0)
  check "H₂ shared electrons mean x ≈ 0" $
    abs (meanOf (sharedX 0.0)) < 1.0
  check "H₂ shared electrons are frame-animated" $
    sharedElectronPositions h2 0.0 /= sharedElectronPositions h2 60.0

  -- Nucleons: one proton per H, placed at each atom's center.
  let
    h2Nuc = moleculeNucleons h2
    nucXs = map (\n -> n.pos.x) h2Nuc
  check "H₂ has 2 nucleons" $ length h2Nuc == 2
  check "H₂ nucleons are all protons" $ all (\n -> n.kind == Atom.Proton) h2Nuc
  check "H₂ nucleon x's are ≈ ±bondLength/2" $
    any (\x -> approxEq x (-halfLen)) nucXs && any (\x -> approxEq x halfLen) nucXs

  -- Extension seam: registry is non-empty; moleculeOf is total/clamped.
  check "molecules registry is non-empty" $ length molecules >= 1
  check "moleculeOf (-1) clamps to first molecule" $
    length (moleculeOf (-1)).atoms >= 1
  check "moleculeOf 9999 clamps (atoms non-empty)" $
    length (moleculeOf 9999).atoms >= 1

  log "all molecule model properties hold."
