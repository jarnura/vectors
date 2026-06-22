-- Data-driven anime.js #materials-cards gallery for the Materials scene.
-- DOM only — this module must never import or touch WebGL/GL. It renders one
-- card per `CuratedStructure` entry, wires each card's click to an `onSelect`
-- callback, and exposes `showMaterialsCards` to show/hide the gallery by scene.
-- Pattern mirrors Controls (anime.js stagger reveals) and Labels (show/hide by
-- scene). The gallery is fully data-driven: the number of cards == the number
-- of entries in the array passed to `renderMaterialsCards`, so adding a 3rd
-- material requires zero FFI/PureScript changes.
module MaterialsCards
  ( CardData
  , renderMaterialsCards
  , showMaterialsCards
  , highlightCard
  , showMaterialsPanel
  ) where

import Prelude (Unit)
import Effect (Effect)

-- Minimal data needed to render one card (name, formula, hybridization).
-- Sourced from `Lattice.structures` in Main — the FFI itself is generic/data-
-- driven and never hardcodes Diamond/Graphene.
type CardData =
  { name :: String
  , formula :: String
  , hybridization :: String
  }

-- Build `#materials-cards` children data-driven from the supplied array.
-- One `.material-card` per entry showing name + formula + hybridization;
-- each card's click calls `onSelect i` with the card's zero-based index.
-- Cards animate in with an anime.js stagger entrance. Clicking a card fires
-- a highlight animation (selected card) in addition to the onSelect callback.
-- No-op if `#materials-cards` is absent. DOM only — never WebGL.
foreign import renderMaterialsCards
  :: Array CardData -> (Int -> Effect Unit) -> Effect Unit

-- Show or hide `#materials-cards`. Pass `true` in the Materials scene, `false`
-- everywhere else. Mirrors how Labels.clearAtomLabels / Text.setVisible gate
-- other overlays to their owning scene. No-op if the container is absent.
foreign import showMaterialsCards :: Boolean -> Effect Unit

-- Highlight a single card by its zero-based index (the "selected" card).
-- Removes any previous highlight first, then animates a glow pulse onto the
-- selected card. No-op if absent or index is out of range. DOM only.
foreign import highlightCard :: Int -> Effect Unit

-- Show or hide `#materials-info`. Pass `true` in the Materials scene, `false`
-- everywhere else. Mirrors showMaterialsCards for the sibling info panel.
-- No-op if the container is absent. DOM only.
foreign import showMaterialsPanel :: Boolean -> Effect Unit
