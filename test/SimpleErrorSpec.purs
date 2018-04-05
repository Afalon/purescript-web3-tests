module SimpleErrorSpec where

import Prelude

import Contracts.SimpleErrorTest as SimpleErrorTest
import Data.Either (fromRight, isLeft)
import Data.Lens.Setter ((.~))
import Data.Maybe (Maybe(..), fromJust)
import Data.Symbol (SProxy(..))
import Network.Ethereum.Web3 (runWeb3, uIntNFromBigNumber, ChainCursor(..), _to, defaultTransactionOptions)
import Partial.Unsafe (unsafePartial)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)
import Utils (Contract(..), getDeployedContract, makeProvider)
import Control.Monad.Eff.Class (liftEff)


errorSpec :: Spec _ Unit
errorSpec =
  describe "interacting with a SimpleErrorTest contract" do
    it "can raise a left for unset values" $ do
      httpP <- liftEff makeProvider
      Contract errorTest <- getDeployedContract (SProxy :: SProxy "SimpleErrorTest")
      let txOptions = defaultTransactionOptions # _to .~ Just errorTest.address
          n = unsafePartial fromJust <<< uIntNFromBigNumber $ one
      resp <- map (unsafePartial fromRight) <<< runWeb3 httpP $ SimpleErrorTest.names txOptions Latest n
      isLeft resp `shouldEqual` true
