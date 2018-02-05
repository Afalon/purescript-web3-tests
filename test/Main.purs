module Test.Main where

import Prelude

import ComplexStorageSpec (complexStorageSpec)
import Data.Maybe (Maybe(..))
import SimpleStorageSpec (simpleStorageSpec, simpleStorageEventsSpec)
import MockERC20Spec (mockERC20Spec)
import SimpleErrorSpec (errorSpec)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner (defaultConfig, run')

main = run' defaultConfig {timeout = Just (60 * 1000)} [consoleReporter] $ do
-- simpleStorageSpec
-- simpleStorageEventsSpec
-- complexStorageSpec
-- mockERC20Spec
 errorSpec
