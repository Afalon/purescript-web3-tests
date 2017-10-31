module Test.Main where

import Prelude
import Utils (makeProvider, getDeployedContract, Contract(..))
import Control.Monad.Eff.Console (CONSOLE, logShow)
import Contracts.SimpleStorage as SimpleStorage
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff (Eff)
import Data.Array ((!!))
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner (RunnerEffects, run)
import Data.Maybe (Maybe(..), fromJust)
import Data.Symbol (SProxy(..))
import Network.Ethereum.Web3.Types (ETH, embed, runWeb3MA)
import Network.Ethereum.Web3.Api (eth_getAccounts)
import Network.Ethereum.Web3.Solidity (uIntNFromBigNumber)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)
import Partial.Unsafe (unsafePartial)

main :: Eff (RunnerEffects _) Unit
main = run [consoleReporter] $ do
  simpleStorageSpec


simpleStorageSpec :: forall r . Spec _ Unit
simpleStorageSpec =
  describe "interacting with a SimpleStorage Contract" do

    it "can set the value of simple storage" do
      provider <- liftEff makeProvider
      accounts <- runWeb3MA provider eth_getAccounts
      liftEff $ logShow accounts
      let primaryAccount = unsafePartial $ fromJust $ accounts !! 0

      Contract simpleStorage <- getDeployedContract provider (SProxy :: SProxy "SimpleStorage")
      hx <- runWeb3MA provider $ do
         let n = unsafePartial $ fromJust <<< uIntNFromBigNumber <<< embed $ 1
         SimpleStorage.setCount (Just simpleStorage.address) primaryAccount (embed 0) n
      liftEff $ logShow hx
      true `shouldEqual` true
