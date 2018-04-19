module MockERC20Spec (mockERC20Spec) where

import Prelude

import Chanterelle.Test (TestConfig)
import Contracts.MockERC20 as MockERC20
import Control.Monad.Aff.AVar (AVAR, makeEmptyVar, putVar, takeVar)
import Control.Monad.Aff.Class (liftAff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Data.Array ((!!))
import Data.Lens.Setter ((.~))
import Data.Maybe (Maybe(..), fromJust)
import Network.Ethereum.Web3 (ETH,Address, ChainCursor(Latest), EventAction(TerminateEvent), _from, _fromBlock, _to, _toBlock, defaultTransactionOptions, embed, event, eventFilter, mkAddress, mkHexString, runWeb3, uIntNFromBigNumber)
import Network.Ethereum.Web3.Solidity.Sizes (s256)
import Partial.Unsafe (unsafePartial)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)
import Type.Proxy (Proxy(..))

mockERC20Spec
  :: forall r eff.
     TestConfig (mockERC20 :: Address | r)
  -> Spec (avar :: AVAR, eth :: ETH, console :: CONSOLE |eff)Unit
mockERC20Spec {accounts, provider, mockERC20} =
  describe "interacting with a ComplexStorage Contract" $ do
    it "can set the values of simple storage" $ do
      let primaryAccount = unsafePartial $ fromJust $ accounts !! 0
      var <- makeEmptyVar
      let amount = unsafePartial $ fromJust <<< uIntNFromBigNumber s256 <<< embed $ 1
          to = unsafePartial $ fromJust $ mkAddress =<< mkHexString "0000000000000000000000000000000000000000"
          txOptions = defaultTransactionOptions # _from .~ Just primaryAccount
                                                # _to .~ Just mockERC20
      hx <- runWeb3 provider $ MockERC20.transfer txOptions {to : to, amount : amount}
      liftEff $ log $ "setValues tx hash: " <> show hx

      let fltTransfer = eventFilter (Proxy :: Proxy MockERC20.Transfer) mockERC20
                          # _fromBlock .~ Latest -- (BN <<< wrap <<< embed $ 4732740)
                          # _toBlock   .~ Latest -- (BN <<< wrap <<< embed $ 4732754)

      _ <- liftAff $ runWeb3 provider $
        event fltTransfer $ \e@(MockERC20.Transfer tfr) -> do
          liftEff $ log $ "Received transfer event: " <> show e
          liftEff $ log $ "Value of `amount` field is: " <> show tfr.amount
          liftEff $ log $ "Value of `from` field is: " <> show tfr.from
          _ <- liftAff $ putVar e var
          pure TerminateEvent
      (MockERC20.Transfer tfr) <- takeVar var
      tfr.amount `shouldEqual` amount
