module MockERC20Spec (mockERC20Spec) where

import Network.Ethereum.Web3.Solidity
import Network.Ethereum.Web3.Types
import Prelude

import Contracts.MockERC20 as MockERC20
import Control.Monad.Aff.AVar (makeEmptyVar, putVar, takeVar)
import Control.Monad.Aff.Class (liftAff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console (log)
import Data.Array ((!!))
import Data.Lens.Setter ((.~))
import Data.Maybe (Maybe(..), fromJust)
import Data.Newtype (wrap)
import Data.Symbol (SProxy(..))
import Network.Ethereum.Web3 (ChainCursor(..), EventAction(..), _from, _fromBlock, _toBlock, defaultTransactionOptions, eventFilter)
import Network.Ethereum.Web3.Api (eth_getAccounts)
import Network.Ethereum.Web3.Contract (event)
import Network.Ethereum.Web3.Provider (runWeb3)
import Partial.Unsafe (unsafePartial)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)
import Type.Proxy (Proxy(..))
import Utils (getDeployedContract, Contract(..), httpP)

mockERC20Spec :: forall r . Spec _ Unit
mockERC20Spec =
  describe "interacting with a ComplexStorage Contract" $ do
    it "can set the values of simple storage" $ do
      accounts <- runWeb3 httpP eth_getAccounts
      let primaryAccount = unsafePartial $ fromJust $ accounts !! 0
      var <- makeEmptyVar
      Contract complexStorage <- getDeployedContract (SProxy :: SProxy "MockERC20")
      let amount = unsafePartial $ fromJust <<< uIntNFromBigNumber <<< embed $ 1
          to = unsafePartial $ fromJust $ mkAddress =<< mkHexString "0000000000000000000000000000000000000000"
          txOptions = defaultTransactionOptions # _from .~ Just primaryAccount
                                                # _to .~ Just complexStorage.address
      hx <- runWeb3 httpP $ MockERC20.transfer txOptions {to : to, amount : amount}
      liftEff $ log $ "setValues tx hash: " <> show hx

      let fltTransfer = eventFilter (Proxy :: Proxy MockERC20.Transfer) complexStorage.address
                          # _fromBlock .~ Latest -- (BN <<< wrap <<< embed $ 4732740)
                          # _toBlock   .~ Latest -- (BN <<< wrap <<< embed $ 4732754)

      _ <- liftAff $ runWeb3 httpP $
        event fltTransfer $ \e@(MockERC20.Transfer tfr) -> do
          liftEff $ log $ "Received transfer event: " <> show e
          liftEff $ log $ "Value of `amount` field is: " <> show tfr.amount
          liftEff $ log $ "Value of `from` field is: " <> show tfr.from
          _ <- liftAff $ putVar e var
          pure TerminateEvent
      (MockERC20.Transfer tfr) <- takeVar var
      tfr.amount `shouldEqual` amount
