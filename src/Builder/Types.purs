-- Core data types for the pure molecule-builder world model: a placed atom, an
-- undirected bond, and the whole builder state. The lowest module in the
-- Builder.* namespace — everything else imports these. Pure, no Effect/WebGL.
module Builder.Types
  ( PlacedAtom
  , BBond
  , BuilderState
  , emptyBuilder
  ) where

import Atom (V3)
import Data.Maybe (Maybe(..))

-- A placed atom: a stable id, its atomic number, and its world position.
type PlacedAtom = { id :: Int, z :: Int, pos :: V3 }

-- A single (undirected) bond between two atom ids, with a bond order.
-- order 1 = single bond (the only order produced by recomputeBonds in this step);
-- higher orders are reserved for future M2 steps.
type BBond = { a :: Int, b :: Int, order :: Int }

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
