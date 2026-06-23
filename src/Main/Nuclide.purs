-- | Nuclide-scene helpers: nucleus entities and live info-panel rendering.
-- | Separated from Main to keep Main under the 800-line cap.
module Main.Nuclide
  ( nuclideNucleusEntities
  , renderNuclideInfo
  , installNuclearReactions
  ) where

import Prelude

import Atom (Nucleon(..), nucleons)
import Controls as Controls
import Data.Int (toNumber) as I
import Data.Maybe (Maybe(..))
import Data.Number.Format (fixed, toStringWith)
import Effect (Effect)
import Graphics.GL (SolidMesh)
import Math.Matrix as M
import FRP.Loop (installNuclearReactionControls)
import Nuclear (NuclearState, Mode(..), applyAlphaDecay, applyBetaMinus, applyBetaPlusEC, applyFuse, applyFission)
import Nuclear as Nuclear
import Scene.Entities (Entity, EntityMesh(..))
import Update (State)

-- Format a number to 2 decimal places.
fixed2 :: Number -> String
fixed2 = toStringWith (fixed 2)

-- Format a number to 3 decimal places.
fixed3 :: Number -> String
fixed3 = toStringWith (fixed 3)

-- | Build the nucleus Entity list for the Nuclide scene: Z protons + N neutrons
-- | placed by Atom.clusterPositions (Fibonacci sphere fill).
-- | Accepts the shared proton + neutron SolidMeshes from the GL init block.
nuclideNucleusEntities
  :: SolidMesh
  -> SolidMesh
  -> State
  -> Array Entity
nuclideNucleusEntities protonMesh neutronMesh s =
  let
    nuc = s.nuclear.nuclide
    z = max 1 nuc.z
    n = max 0 nuc.n
    -- electrons field required by the Element record shape but ignored by Atom.nucleons (nucleus-only render).
    el = { z, symbol: Nuclear.nuclearSymbol z, protons: z, neutrons: n, electrons: 0 }
  in
    map
      ( \p ->
          { mesh: Solid (if p.kind == Proton then protonMesh else neutronMesh)
          , modelMatrix: \_ -> M.translate p.pos.x p.pos.y p.pos.z
          }
      )
      (nucleons el)

-- | Render the live #nuclide-info panel for the current nuclear state.
-- | Called every frame when in the Nuclide scene (mutations trigger redraws).
-- | M3: adds "Last reaction" row with products summary and Q-value RELEASED/ABSORBED tag.
renderNuclideInfo :: State -> Effect Unit
renderNuclideInfo s = do
  let
    nuc = s.nuclear.nuclide
    a = Nuclear.massNumber nuc
    b = Nuclear.measuredBinding nuc
    ba = if a < 1 then 0.0 else b / I.toNumber a
    mode = Nuclear.decayMode nuc
    modeName = case mode of
      Stable     -> "Stable"
      BetaMinus  -> "β− (BetaMinus)"
      BetaPlusEC -> "β+/EC (BetaPlusEC)"
      Alpha      -> "α (Alpha)"
      Unbound    -> "Unbound (emits nucleon)"
    -- M3: last reaction Q-value and spontaneity annotation.
    q = s.nuclear.lastQ
    spontaneity = if q > 0.0 then "RELEASED" else "ABSORBED"
    qLabel =
      if q == 0.0 then "—"
      else fixed3 q <> " MeV " <> spontaneity
    -- Build the product summary for the last reaction.
    lastRxnValue = buildLastReactionValue s.nuclear
    infoRows =
      [ { label: "Element", value: Nuclear.nuclearSymbol nuc.z <> " (" <> Nuclear.nuclearName nuc.z <> ")" }
      , { label: "Z", value: show nuc.z }
      , { label: "A (mass number)", value: show a }
      , { label: "N (neutrons)", value: show nuc.n }
      , { label: "Binding (MeV)", value: fixed2 b }
      , { label: "B/A (MeV)", value: fixed2 ba }
      , { label: "Stability", value: modeName }
      , { label: "Last reaction", value: lastRxnValue }
      , { label: "Q-value", value: qLabel }
      ]
  Controls.renderInfoPanel "nuclide-info" infoRows

-- | Build the "last reaction" summary string for the info panel.
buildLastReactionValue :: NuclearState -> String
buildLastReactionValue ns =
  case ns.lastFission of
    -- Fission: show both fragments and neutron count.
    Just f ->
      let
        symA = Nuclear.nuclearSymbol f.a.z
        mA = Nuclear.massNumber f.a
        symB = Nuclear.nuclearSymbol f.b.z
        mB = Nuclear.massNumber f.b
      in
        symA <> "-" <> show mA <> " + " <> symB <> "-" <> show mB
          <> " + " <> show f.neutrons <> "n"
    Nothing ->
      -- For non-fission reactions, the current nuclide IS the product.
      if ns.lastQ == 0.0 then "—"
      else
        let
          sym = Nuclear.nuclearSymbol ns.nuclide.z
          m = Nuclear.massNumber ns.nuclide
        in
          sym <> "-" <> show m

-- | Wire the M3 named-reaction buttons (#react-alpha, #react-beta-minus,
-- | #react-beta-plus, #react-fuse, #react-fission) using the shared
-- | `nuclearMutate` helper from Main. Separated here to keep Main under 800 lines.
installNuclearReactions
  :: (( NuclearState -> NuclearState) -> Effect Unit)
  -> Effect Unit
installNuclearReactions nuclearMutate =
  installNuclearReactionControls
    (nuclearMutate applyAlphaDecay)
    (nuclearMutate applyBetaMinus)
    (nuclearMutate applyBetaPlusEC)
    (\z2 n2 -> nuclearMutate (applyFuse z2 n2))
    (\zA nA zB nB -> nuclearMutate (applyFission zA nA zB nB))
