module Test.Main where

import Prelude

import Chanterelle.Test (buildTestConfig)
import Effect.Aff (launchAff)
import Effect (Effect)
import Effect.Class (liftEffect)
import Data.Maybe (Maybe(..))
import Main (deployScript)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner (run', defaultConfig)


import ComplexStorageSpec (complexStorageSpec)
import MockERC20Spec (mockERC20Spec)
import PayableSpec (payableTestSpec)
import SimpleErrorSpec (simpleErrorSpec)
import SimpleStorageSpec (simpleStorageSpec, simpleStorageEventsSpec)

-- | TODO: make the options for deploy config env vars
main :: Effect Unit
main = void <<< launchAff $ do
  testConfig <- buildTestConfig "http://localhost:8545" 60 deployScript
  liftEffect $ run' defaultConfig {timeout = Just (60 * 1000)} [consoleReporter] do
    simpleStorageSpec testConfig
    simpleStorageEventsSpec testConfig
    mockERC20Spec testConfig
    payableTestSpec testConfig
    simpleErrorSpec testConfig
    complexStorageSpec testConfig
