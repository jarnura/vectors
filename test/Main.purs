module Test.Main where

import Prelude

import Effect (Effect)
import Test.MatrixSpec (matrixSpec)
import Test.MeshesWorldSpec (meshesWorldSpec)
import Test.SceneStarfieldSpec (sceneStarfieldSpec)
import Test.AtomSpec (atomSpec)
import Test.MoleculeChemSpec (moleculeChemSpec)
import Test.BuilderSpec (builderSpec)
import Test.BuilderOverlapSpec (builderOverlapSpec)
import Test.BuilderBondsSpec (builderBondsSpec)
import Test.BuilderBondsS3Spec (builderBondsS3Spec)
import Test.CameraLayerSpec (cameraLayerSpec)
import Test.MainStateSpec (mainStateSpec)
import Test.PeSpec (peSpec)
import Test.VibrationSpec (vibrationSpec)
import Test.BuilderBondsS4Spec (builderBondsS4Spec)

-- | Thin aggregator: runs every per-domain spec in sequence. All assertions
-- | live in the Test.<Domain>Spec modules; this module holds none.
main :: Effect Unit
main = do
  matrixSpec
  meshesWorldSpec
  sceneStarfieldSpec
  atomSpec
  moleculeChemSpec
  builderSpec
  builderOverlapSpec
  builderBondsSpec
  builderBondsS3Spec
  cameraLayerSpec
  mainStateSpec
  peSpec
  vibrationSpec
  builderBondsS4Spec
