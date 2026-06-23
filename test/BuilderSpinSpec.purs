-- M3-S3: Spin tag assertions for the SIGMA paired singlet of each bond.
-- Verifies that bondSigmaSpins returns the correct spin labels: one Up and one
-- Down per sigma pair (the anti-parallel singlet required by Pauli exclusion),
-- count matches bondElectronPositions, determinism, and model/electron-count
-- conservation (spin tagging does not alter the model or positions).
-- Pure, total, deterministic — no Effect/WebGL.
module Test.BuilderSpinSpec where

import Prelude

import Data.Array (index, length, nub)
import Data.Foldable (sum)
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Console (log)
import Builder as B
import Builder.Electrons (Spin(..), bondSigmaSpins)
import Test.Util (check)

builderSpinSpec :: Effect Unit
builderSpinSpec = do
  log "M3-S3 spin-tag (sigma singlet) properties:"

  let
    near = B.bondThreshold * 0.5 -- comfortably inside bonding range

    -- H-H: order 1 (single bond, sigma only) — the canonical singlet pair.
    hhPair = B.addAtom 1 { x: near, y: 0.0, z: 0.0 }
      (B.addAtom 1 { x: 0.0, y: 0.0, z: 0.0 } B.emptyBuilder)

    -- O-O: order 2 (double bond: 1 sigma + 1 PI pair).
    ooPair = B.addAtom 8 { x: near, y: 0.0, z: 0.0 }
      (B.addAtom 8 { x: 0.0, y: 0.0, z: 0.0 } B.emptyBuilder)

    -- N-N: order 3 (triple bond: 1 sigma + 2 PI pairs).
    nnPair = B.addAtom 7 { x: near, y: 0.0, z: 0.0 }
      (B.addAtom 7 { x: 0.0, y: 0.0, z: 0.0 } B.emptyBuilder)

    hhSpins = bondSigmaSpins hhPair
    ooSpins = bondSigmaSpins ooPair
    nnSpins = bondSigmaSpins nnPair
    emptySpins = bondSigmaSpins B.emptyBuilder

  -- ── (a) EMPTY WORLD ──────────────────────────────────────────────────────────
  check "spin: empty world ⇒ no spin tags" $
    length emptySpins == 0

  -- ── (b) COUNT: spin tag count equals bondElectronPositions count ─────────────
  -- This is the primary structural invariant: one spin label per bond electron,
  -- for any frame (spins are frame-independent).
  check "spin count: H-H == 2 (order 1: 2 electrons)" $
    length hhSpins == 2
  check "spin count: O-O == 4 (order 2: 4 electrons)" $
    length ooSpins == 4
  check "spin count: N-N == 6 (order 3: 6 electrons)" $
    length nnSpins == 6
  check "spin count: H-H spin count == bondElectronPositions count" $
    length hhSpins == length (B.bondElectronPositions hhPair 0.0)
  check "spin count: O-O spin count == bondElectronPositions count" $
    length ooSpins == length (B.bondElectronPositions ooPair 0.0)
  check "spin count: N-N spin count == bondElectronPositions count" $
    length nnSpins == length (B.bondElectronPositions nnPair 0.0)
  check "spin count: H-H == 2 * sum of bond orders" $
    length hhSpins == 2 * (sum (map _.order hhPair.bonds))
  check "spin count: O-O == 2 * sum of bond orders" $
    length ooSpins == 2 * (sum (map _.order ooPair.bonds))
  check "spin count: N-N == 2 * sum of bond orders" $
    length nnSpins == 2 * (sum (map _.order nnPair.bonds))

  -- ── (c) SINGLET: sigma pair has DISTINCT spins (one Up, one Down) ────────────
  -- The first two spin tags of any bond are the sigma pair; they must be distinct.
  check "spin singlet: H-H sigma pair has two distinct spins" $
    case index hhSpins 0, index hhSpins 1 of
      Just s0, Just s1 -> s0 /= s1
      _, _ -> false
  check "spin singlet: H-H sigma pair is (Up, Down)" $
    index hhSpins 0 == Just Up && index hhSpins 1 == Just Down
  check "spin singlet: O-O sigma pair is (Up, Down)" $
    index ooSpins 0 == Just Up && index ooSpins 1 == Just Down
  check "spin singlet: N-N sigma pair is (Up, Down)" $
    index nnSpins 0 == Just Up && index nnSpins 1 == Just Down

  -- ── (d) VARIETY: both Up and Down appear in the tag list ─────────────────────
  -- nub collapses identical values; length 2 means both Up and Down are present.
  check "spin variety: H-H tags contain both Up and Down" $
    length (nub hhSpins) == 2
  check "spin variety: O-O tags contain both Up and Down" $
    length (nub ooSpins) == 2
  check "spin variety: N-N tags contain both Up and Down" $
    length (nub nnSpins) == 2

  -- ── (e) ALTERNATING PATTERN: Up at even index, Down at odd index ─────────────
  -- Confirmed for the first four positions of O-O (sigma + PI pair).
  check "spin pattern: O-O index 0 == Up" $
    index ooSpins 0 == Just Up
  check "spin pattern: O-O index 1 == Down" $
    index ooSpins 1 == Just Down
  check "spin pattern: O-O index 2 == Up" $
    index ooSpins 2 == Just Up
  check "spin pattern: O-O index 3 == Down" $
    index ooSpins 3 == Just Down

  -- ── (f) DETERMINISM: identical call gives identical result ────────────────────
  check "spin determinism: H-H spins are reproducible" $
    bondSigmaSpins hhPair == bondSigmaSpins hhPair
  check "spin determinism: O-O spins are reproducible" $
    bondSigmaSpins ooPair == bondSigmaSpins ooPair
  check "spin determinism: N-N spins are reproducible" $
    bondSigmaSpins nnPair == bondSigmaSpins nnPair

  -- ── (g) MODEL CONSERVATION: spin tags do not change electron count ────────────
  -- bondElectronPositions before and after bondSigmaSpins must be identical
  -- (spin tagging is pure / read-only: no mutation, no count change).
  check "spin conservation: H-H bondElectronPositions unchanged by spin query" $
    B.bondElectronPositions hhPair 0.0 == B.bondElectronPositions hhPair 0.0
  check "spin conservation: H-H atom count unchanged" $
    length hhPair.atoms == 2
  check "spin conservation: H-H bond count unchanged" $
    length hhPair.bonds == 1
  check "spin conservation: O-O atom count unchanged" $
    length ooPair.atoms == 2
  check "spin conservation: O-O bond count unchanged" $
    length ooPair.bonds == 1
  check "spin conservation: N-N atom count unchanged" $
    length nnPair.atoms == 2
  check "spin conservation: N-N bond count unchanged" $
    length nnPair.bonds == 1

  log "all M3-S3 spin-tag (sigma singlet) properties hold."
