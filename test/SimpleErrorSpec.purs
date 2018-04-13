module SimpleErrorSpec (simpleErrorSpec) where

import Prelude

import Chanterelle.Test (TestConfig)
import Contracts.SimpleErrorTest as SimpleErrorTest
import Data.Either (fromRight, isLeft)
import Data.Lens.Setter ((?~))
import Data.Maybe (fromJust)
import Network.Ethereum.Web3 (runWeb3, uIntNFromBigNumber, ChainCursor(..), _to, defaultTransactionOptions, Address)
import Partial.Unsafe (unsafePartial)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)


simpleErrorSpec
  :: forall r.
     TestConfig (simpleErrorTest :: Address | r)
  -> Spec _ Unit
simpleErrorSpec {provider, accounts, simpleErrorTest} =
  describe "interacting with a SimpleErrorTest contract" do
    it "can raise a left for unset values" $ do
      let txOptions = defaultTransactionOptions # _to ?~ simpleErrorTest
          n = unsafePartial fromJust <<< uIntNFromBigNumber $ one
      resp <- map (unsafePartial fromRight) <<< runWeb3 provider $ SimpleErrorTest.names txOptions Latest n
      isLeft resp `shouldEqual` true
