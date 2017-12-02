module Test.Main where

import Prelude

import ComplexStorageSpec (complexStorageSpec)
import Data.Maybe (Maybe(..))
import SimpleStorageSpec (simpleStorageSpec)
import MockERC20Spec (mockERC20Spec)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner (defaultConfig, run')

main = run' defaultConfig {timeout = Just (60 * 1000)} [consoleReporter] $ do
  simpleStorageSpec
  complexStorageSpec
  mockERC20Spec
