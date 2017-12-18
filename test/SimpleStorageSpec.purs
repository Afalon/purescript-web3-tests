module SimpleStorageSpec where

import Prelude

import Contracts.SimpleStorage as SimpleStorage
import Control.Monad.Aff (Fiber, delay, launchAff)
import Control.Monad.Aff.AVar (AVAR, makeEmptyVar, putVar, takeVar)
import Control.Monad.Aff.Class (liftAff)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console (CONSOLE, log, logShow)
import Control.Monad.Eff.Exception (EXCEPTION)
import Control.Monad.Reader (ReaderT)
import Control.Monad.Trans.Class (lift)
import Data.Array (range, (!!))
import Data.Foldable (sum)
import Data.Lens.Setter ((.~))
import Data.List.Lazy (foldl, replicate)
import Data.Maybe (Maybe(..), fromJust)
import Data.Newtype (unwrap, wrap)
import Data.Symbol (SProxy(..))
import Data.Time.Duration (Milliseconds(..))
import Data.Traversable (traverse, sequence, sequence_, sum)
import Network.Ethereum.Web3 (BlockMode(..), UIntN, _fromBlock, _toBlock, embed, eventFilter)
import Network.Ethereum.Web3.Api (eth_newBlockFilter, eth_blockNumber, eth_getAccounts, eth_getBlockByNumber)
import Network.Ethereum.Web3.Contract (EventAction(..), event)
import Network.Ethereum.Web3.Provider (forkWeb3, runWeb3)
import Network.Ethereum.Web3.Solidity (uIntNFromBigNumber)
import Network.Ethereum.Web3.Solidity.UInt (UIntN, unUIntN)
import Network.Ethereum.Web3.Types (ETH, Web3(..), Value, Wei, embed, unsafeToInt)
import Node.FS.Aff (FS)
import Node.Process (PROCESS)
import Partial.Unsafe (unsafePartial)
import Pipes.Prelude (mapM_)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)
import Test.Spec.Runner (timeout)
import Type.Prelude (Proxy(..))
import Utils (makeProvider, getDeployedContract, Contract(..), HttpProvider, httpP)

toNum :: forall a . Semiring a => Int -> a 
toNum n = sum (replicate n one) 

simpleStorageSpec :: forall r . Spec _ Unit
simpleStorageSpec =
  describe "interacting with a SimpleStorage Contract" do

    it "can set the value of simple storage" $ do
      accounts <- runWeb3 httpP eth_getAccounts
      let primaryAccount = unsafePartial $ fromJust $ accounts !! 0
      var <- makeEmptyVar
      Contract simpleStorage <- getDeployedContract (SProxy :: SProxy "SimpleStorage")
      bn <- runWeb3 httpP $ eth_blockNumber
      liftEff <<< log $ "Current blockNumber is: " <> show bn
      let n = unsafePartial $ fromJust <<< uIntNFromBigNumber <<< embed $ (unsafeToInt <<< unwrap $ bn)
      hx <- runWeb3 httpP $ SimpleStorage.setCount (Just simpleStorage.address) primaryAccount n
      liftEff <<< log $ "setCount tx hash: " <> show hx

      let filterCountSet = eventFilter (Proxy :: Proxy SimpleStorage.CountSet) simpleStorage.address
      _ <- liftAff $ runWeb3 httpP $
        event filterCountSet $ \e@(SimpleStorage.CountSet cs) -> do
          liftEff $ log $ "Received Event: " <> show e
          _ <- liftAff $ putVar cs._count var
          pure TerminateEvent
      val <- takeVar var
      Just val `shouldEqual` Just n

simpleStorageEventsSpec :: forall r . Spec _ Unit
simpleStorageEventsSpec =
  describe "interacting with a SimpleStorage events for different block intervals" $ do

    it "can set the value of simple storage over multiple blocks" $ do
      bn <- runWeb3 httpP $ eth_blockNumber
      liftEff <<< log $ "Current blockNumber is: " <> show bn
      var <- makeEmptyVar
      Contract simpleStorage <- getDeployedContract (SProxy :: SProxy "SimpleStorage")
      accounts <- runWeb3 httpP eth_getAccounts
      let primaryAccount = unsafePartial $ fromJust $ accounts !! 0
      let r = range (unsafeToInt <<< unwrap $ bn) ((unsafeToInt <<< unwrap $ bn) + 2)
      let m = (\n -> unsafePartial $ fromJust <<< uIntNFromBigNumber <<< embed $ n) <$> r

      let filterCountSet = eventFilter (Proxy :: Proxy SimpleStorage.CountSet) simpleStorage.address
                         # _fromBlock .~ BN (wrap ((unwrap bn) - (toNum 0)))
                         # _toBlock   .~ BN (wrap ((unwrap bn) + (toNum 3)))

      liftEff <<< log $ "The filter is: " <> show filterCountSet
      _ <- traverse (setter simpleStorage.address primaryAccount) m

      _ <- liftAff $ runWeb3 httpP $
        event filterCountSet $ \e@(SimpleStorage.CountSet cs) -> do
          liftEff $ log $ "Received Event: " <> show e
          old <- liftAff $ takeVar var
          _ <- liftAff $ putVar ((unsafeToInt <<< unUIntN $ cs._count) + old) var
          pure TerminateEvent
      val <- takeVar var
      Just val `shouldEqual` Just (sum r)

    where
       setter address account n = do
         hx <- runWeb3 httpP $ SimpleStorage.setCount (Just address) account n
         liftEff <<< log $ "setCount: " <> show n <> ", tx hash: " <> show hx
         liftAff $ delay (Milliseconds 1000.0) -- we should probably use eth_newBlockFilter instead
