module SimpleStorageSpec where

import Prelude

import Contracts.SimpleStorage as SimpleStorage
import Control.Monad.Aff (launchAff)
import Control.Monad.Aff.AVar (AVAR, makeEmptyVar, putVar, takeVar)
import Control.Monad.Aff.Class (liftAff)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console (CONSOLE, log, logShow)
import Control.Monad.Eff.Exception (EXCEPTION)
import Control.Monad.Reader (ReaderT)
import Data.Array ((!!))
import Data.Maybe (Maybe(..), fromJust)
import Data.Symbol (SProxy(..))
import Network.Ethereum.Web3.Api (eth_getAccounts)
import Network.Ethereum.Web3.Contract (EventAction(..), event)
import Network.Ethereum.Web3.Provider (forkWeb3, runWeb3)
import Network.Ethereum.Web3.Solidity (uIntNFromBigNumber)
import Network.Ethereum.Web3.Types (ETH, Web3(..), Value, Wei, embed)
import Node.FS.Aff (FS)
import Node.Process (PROCESS)
import Partial.Unsafe (unsafePartial)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)
import Test.Spec.Runner (timeout)
import Utils (makeProvider, getDeployedContract, Contract(..), HttpProvider)

simpleStorageSpec :: forall r . Spec _ Unit
simpleStorageSpec =
  describe "interacting with a SimpleStorage Contract" do

    it "can set the value of simple storage" $ do
      accounts <- runWeb3 (eth_getAccounts :: Web3 HttpProvider _ _)
      let primaryAccount = unsafePartial $ fromJust $ accounts !! 0
      var <- makeEmptyVar
      Contract simpleStorage <- getDeployedContract (SProxy :: SProxy "SimpleStorage")
      let n = unsafePartial $ fromJust <<< uIntNFromBigNumber <<< embed $ 1
      hx <- runWeb3 $ SimpleStorage.setCount (Just simpleStorage.address) primaryAccount (zero :: Value Wei) n :: Web3 HttpProvider _ _
      liftEff <<< log $ "setCount tx hash: " <> show hx
      _ <- liftAff $ runWeb3 $
        event simpleStorage.address $ \e@(SimpleStorage.CountSet _count) -> do
          liftEff $ log $ "Received Event: " <> show e
          _ <- liftAff $ putVar _count var
          pure TerminateEvent :: ReaderT _ (Web3 HttpProvider _) _
      val <- takeVar var
      Just val `shouldEqual` Just n
