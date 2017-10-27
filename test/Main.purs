module Test.Main where

import Prelude

import Control.Monad.Eff (Eff)
import DOM (DOM)
import Test.Assert (ASSERT)
import Test.Element (assertDiffAlgorithmForElement)
import Test.TestHelper (enableJSDOM)
import Test.Text (assertDiffAlgorithmForText)



main :: forall e. Eff (dom :: DOM, assert :: ASSERT | e) Unit
main = do
  enableJSDOM
  assertDiffAlgorithmForText
  assertDiffAlgorithmForElement
