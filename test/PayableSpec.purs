module PayableSpec where

import Prelude

import Contracts.PayableTest as PayableTest
import Control.Monad.Aff.AVar (makeEmptyVar, putVar, takeVar)
import Control.Monad.Aff.Class (liftAff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console (log)
import Data.Array ((!!))
import Data.Either (fromRight)
import Data.Lens.Setter ((.~))
import Data.Maybe (Maybe(..), fromJust)
import Data.Symbol (SProxy(..))
import Network.Ethereum.Web3 (Ether, EventAction(..), Shannon, Value, _from, _to, _value, convert, defaultTransactionOptions, event, eventFilter, mkValue, runWeb3, unUIntN)
import Network.Ethereum.Web3.Api (eth_getAccounts)
import Partial.Unsafe (unsafePartial)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)
import Type.Proxy (Proxy(..))
import Utils (Contract(..), getDeployedContract, makeProvider)


payableSpec :: Spec _ Unit
payableSpec =
  describe "interacting with a PayableTest contract" do
    it "can send the right amount of wei" $ do
      httpP <- liftEff makeProvider
      accounts <- unsafePartial fromRight <$> runWeb3 httpP eth_getAccounts
      let primaryAccount = unsafePartial $ fromJust $ accounts !! 0
      var <- makeEmptyVar
      Contract payableTest <- getDeployedContract (SProxy :: SProxy "PayableTest")
      let txOptions = defaultTransactionOptions # _to .~ Just payableTest.address
                                                # _value .~ Just (convert (mkValue one :: Value Ether))
                                                # _from .~ Just primaryAccount
      _ <- map (unsafePartial fromRight) <<< runWeb3 httpP $ PayableTest.seeContent txOptions
      let filterContent = eventFilter (Proxy :: Proxy PayableTest.Content) payableTest.address
      _ <- liftAff $ runWeb3 httpP $
        event filterContent $ \e@(PayableTest.Content c) -> do
          liftEff $ log $ "Received Event: " <> show e
          _ <- liftAff $ putVar c._paidContent var
          pure TerminateEvent
      val <- unUIntN <$> takeVar var
      Just val `shouldEqual` Just one
      let txOptions' = defaultTransactionOptions # _to .~ Just payableTest.address
                                                 # _value .~ Just (convert (mkValue one :: Value Shannon))
                                                 # _from .~ Just primaryAccount
      _ <- map (unsafePartial fromRight) <<< runWeb3 httpP $ PayableTest.seeContent txOptions'
      let filterContent' = eventFilter (Proxy :: Proxy PayableTest.Content) payableTest.address
      _ <- liftAff $ runWeb3 httpP $
        event filterContent' $ \e@(PayableTest.Content c) -> do
          liftEff $ log $ "Received Event: " <> show e
          _ <- liftAff $ putVar c._paidContent var
          pure TerminateEvent
      val' <- unUIntN <$> takeVar var
      Just val' `shouldEqual` Just zero
