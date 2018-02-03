module SimpleErrorSpec where

import Network.Ethereum.Web3.Solidity
import Network.Ethereum.Web3.Types
import Prelude

import Contracts.SimpleErrorTest as SimpleErrorTest
import Data.Either (isLeft)
import Data.Lens.Setter ((.~))
import Data.Maybe (Maybe(..))
import Data.Newtype (wrap)
import Data.Symbol (SProxy(..))
import Debug.Trace (traceA)
import Network.Ethereum.Web3 (ChainCursor(..), CallError(..), _to, defaultTransactionOptions, runWeb3)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)
import Test.Spec.Runner (timeout)
import Utils (Contract(..), getDeployedContract, httpP)

errorSpec :: Spec _ Unit
errorSpec =
  describe "interacting with a SimpleErrorTest contract" do
    it "can raise a left for unset values" $ do
      Contract errorTest <- getDeployedContract (SProxy :: SProxy "SimpleErrorTest")
      let txOptions = defaultTransactionOptions # _to .~ Just errorTest.address
      resp <- runWeb3 httpP $ SimpleErrorTest.name txOptions Latest
      traceA $ show resp
      isLeft resp `shouldEqual` true
