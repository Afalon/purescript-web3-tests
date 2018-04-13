module SimpleStorageSpec (simpleStorageSpec, simpleStorageEventsSpec) where

import Prelude

import Chanterelle.Test (TestConfig)
import Contracts.SimpleStorage as SimpleStorage
import Control.Monad.Aff (delay, joinFiber)
import Control.Monad.Aff.AVar (makeEmptyVar, putVar, takeVar)
import Control.Monad.Aff.Class (liftAff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console (log)
import Control.Monad.Reader (ask)
import Data.Array ((!!), (:))
import Data.Either (fromRight)
import Data.Lens.Setter ((.~))
import Data.List.Lazy (replicate)
import Data.Maybe (Maybe(..), fromJust)
import Data.Newtype (unwrap, wrap)
import Data.Set (fromFoldable)
import Data.Time.Duration (Milliseconds(..))
import Data.Traversable (traverse, sum)
import Network.Ethereum.Web3.Api (eth_blockNumber)
import Network.Ethereum.Web3 (unsafeToInt, unUIntN, forkWeb3, runWeb3, EventAction(..), event, ChainCursor(..), _fromBlock, _toBlock, embed, eventFilter, uIntNFromBigNumber, Change(..), _from, _to, defaultTransactionOptions, Address)
import Partial.Unsafe (unsafePartial)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)
import Type.Prelude (Proxy(..))

toNum :: forall a . Semiring a => Int -> a
toNum n = sum (replicate n one)

simpleStorageSpec
  :: forall r.
     TestConfig (simpleStorage :: Address | r)
  -> Spec _ Unit
simpleStorageSpec {provider, accounts, simpleStorage} =
  describe "interacting with a SimpleStorage Contract" do

    it "can set the value of simple storage" $ do
      let primaryAccount = unsafePartial fromJust $ accounts !! 0
      var <- makeEmptyVar
      bn <- unsafePartial fromRight <$> runWeb3 provider eth_blockNumber
      liftEff <<< log $ "Current blockNumber is: " <> show bn
      let n = unsafePartial $ fromJust <<< uIntNFromBigNumber <<< embed $ (unsafeToInt <<< unwrap $ bn)
          txOptions = defaultTransactionOptions # _from .~ Just primaryAccount
                                                # _to .~ Just simpleStorage
      hx <- runWeb3 provider $ SimpleStorage.setCount txOptions {_count: n}
      liftEff <<< log $ "setCount tx hash: " <> show hx

      let filterCountSet = eventFilter (Proxy :: Proxy SimpleStorage.CountSet) simpleStorage
      _ <- liftAff $ runWeb3 provider $
        event filterCountSet $ \e@(SimpleStorage.CountSet cs) -> do
          liftEff $ log $ "Received Event: " <> show e
          _ <- liftAff $ putVar cs._count var
          pure TerminateEvent
      val <- takeVar var
      Just val `shouldEqual` Just n

simpleStorageEventsSpec
  :: forall r.
     TestConfig (simpleStorage :: Address | r)
  -> Spec _ Unit
simpleStorageEventsSpec {provider, accounts, simpleStorage} =
  describe "interacting with a SimpleStorage events for different block intervals" $ do

    it "can stream events starting and ending in the past" $ do
      _ <- runWeb3 provider waitBlock
      -- set up
      var <- makeEmptyVar
      putVar [] var
      let primaryAccount = unsafePartial $ fromJust $ accounts !! 0

      -- actual test
      let values = map (unsafePartial fromJust <<< uIntNFromBigNumber <<< embed) [1,2,3]
      blockNumberV <- makeEmptyVar
      start <- unsafePartial fromRight <$> runWeb3 provider eth_blockNumber
      liftEff <<< log $ "Current blockNumber is: " <> show start
      _ <- forkWeb3 provider $ event (eventFilter (Proxy :: Proxy SimpleStorage.CountSet) simpleStorage)  \e@(SimpleStorage.CountSet cs) -> do
        liftEff <<< log $ "Received CountSet event: " <> show e
        if cs._count == (unsafePartial fromJust <<< uIntNFromBigNumber <<< embed $ 3)
          then do
            (Change c) <- ask
            liftAff $ putVar c.blockNumber blockNumberV
            pure TerminateEvent
          else pure ContinueEvent
      liftEff <<< log $ "About to traverse setters"
      _ <- traverse (setter simpleStorage primaryAccount) values
      liftEff <<< log $ "Done setting"
      end <- liftAff $ takeVar blockNumberV

      let filterCountSet = eventFilter (Proxy :: Proxy SimpleStorage.CountSet) simpleStorage
                         # _fromBlock .~ BN start
                         # _toBlock   .~ BN end
      liftEff <<< log $ "The filter is: " <> show filterCountSet
      -- set the count, sequentially, -> bn -> bn + 1 -> bn + 2
      -- register the filter
      delay (Milliseconds 5000.0)
      _ <- runWeb3 provider $
        event filterCountSet $ \e@(SimpleStorage.CountSet cs) -> do
          liftEff $ log $ "Received Event: " <> show e
          old <- liftAff $ takeVar var
          _ <- liftAff $ putVar (cs._count : old) var
          if cs._count == (unsafePartial fromJust <<< uIntNFromBigNumber <<< embed $ 3)
             then pure TerminateEvent
             else pure ContinueEvent
      val <- takeVar var
      fromFoldable [3,2,1] `shouldEqual` fromFoldable (map (unsafeToInt <<< unUIntN) val)

    it "can stream events starting in the past and ending in the future" $ do
      _ <- runWeb3 provider waitBlock
      -- set up
      var <- makeEmptyVar
      putVar [] var
      let primaryAccount = unsafePartial $ fromJust $ accounts !! 0

      -- actual test
      let firstValues = map (unsafePartial fromJust <<< uIntNFromBigNumber <<< embed) [1,2,3]
          secondValues = map (unsafePartial fromJust <<< uIntNFromBigNumber <<< embed) [4,5,6]
      start <- unsafePartial fromRight <$> runWeb3 provider eth_blockNumber
      liftEff <<< log $ "Current blockNumber is: " <> show start
      f1 <- forkWeb3 provider $ event (eventFilter (Proxy :: Proxy SimpleStorage.CountSet) simpleStorage)  \e@(SimpleStorage.CountSet cs) -> do
        if cs._count == (unsafePartial fromJust <<< uIntNFromBigNumber <<< embed $ 3)
          then pure TerminateEvent
          else pure ContinueEvent
      _ <- traverse (setter simpleStorage primaryAccount) firstValues
      _ <- joinFiber f1
      delay (Milliseconds 5000.0)
      let filterCountSet = eventFilter (Proxy :: Proxy SimpleStorage.CountSet) simpleStorage
                         # _fromBlock .~ BN start
                         # _toBlock   .~ Latest
      f2 <- forkWeb3 provider $
        event filterCountSet $ \e@(SimpleStorage.CountSet cs) -> do
          old <- liftAff $ takeVar var
          _ <- liftAff $ putVar (cs._count : old) var
          if cs._count == (unsafePartial fromJust <<< uIntNFromBigNumber <<< embed $ 6)
             then pure TerminateEvent
             else pure ContinueEvent
      _ <- traverse (setter simpleStorage primaryAccount) secondValues
      _ <- joinFiber f2
      val <- takeVar var
      fromFoldable [6,5,4,3,2,1] `shouldEqual` fromFoldable (map (unsafeToInt <<< unUIntN) val)


    it "can stream events starting and ending in the future, unbounded" $ do
      _ <- runWeb3 provider waitBlock
      -- set up
      var <- makeEmptyVar
      putVar [] var
      let primaryAccount = unsafePartial $ fromJust $ accounts !! 0

      -- actual test
      let values = map (unsafePartial fromJust <<< uIntNFromBigNumber <<< embed) [1,2,3]
      now <- unsafePartial fromRight <$> runWeb3 provider eth_blockNumber
      liftEff <<< log $ "Current blockNumber is: " <> show now
      let later = wrap (unwrap now + embed 3)
          filterCountSet = eventFilter (Proxy :: Proxy SimpleStorage.CountSet) simpleStorage
                         # _fromBlock .~ BN later
                         # _toBlock   .~ Latest
      liftEff <<< log $ "The filter is: " <> show filterCountSet
      -- set the count, sequentially, -> bn -> bn + 1 -> bn + 2
      -- register the filter
      f <- forkWeb3 provider $
        event filterCountSet $ \e@(SimpleStorage.CountSet cs) -> do
          liftEff $ log $ "Received Event: " <> show e
          old <- liftAff $ takeVar var
          _ <- liftAff $ putVar (cs._count : old) var
          if cs._count == (unsafePartial fromJust <<< uIntNFromBigNumber <<< embed $ 3)
             then pure TerminateEvent
             else pure ContinueEvent
      _ <- runWeb3 provider $ hangOutTillBlock later
      _ <- traverse (setter simpleStorage primaryAccount) values
      _ <- joinFiber f
      val <- takeVar var
      fromFoldable [3,2,1] `shouldEqual` fromFoldable (map (unsafeToInt <<< unUIntN) val)

    it "can stream events starting and ending in the future, bounded" $ do
      _ <- runWeb3 provider waitBlock
      -- set up
      var <- makeEmptyVar
      putVar [] var
      let primaryAccount = unsafePartial $ fromJust $ accounts !! 0

      -- actual test
      let values = map (unsafePartial fromJust <<< uIntNFromBigNumber <<< embed) [8,9,10]
      now <- unsafePartial fromRight <$> runWeb3 provider eth_blockNumber
      liftEff <<< log $ "Current blockNumber is: " <> show now
      let later = wrap $ unwrap now + embed 3
          latest = wrap $ unwrap now + embed 8
          filterCountSet = eventFilter (Proxy :: Proxy SimpleStorage.CountSet) simpleStorage
                         # _fromBlock .~ BN later
                         # _toBlock   .~ BN latest
      liftEff <<< log $ "The filter is: " <> show filterCountSet
      -- set the count, sequentially, -> bn -> bn + 1 -> bn + 2
      -- register the filter
      f <- forkWeb3 provider $
        event filterCountSet $ \e@(SimpleStorage.CountSet cs) -> do
          liftEff $ log $ "Received Event: " <> show e
          old <- liftAff $ takeVar var
          _ <- liftAff $ putVar (cs._count : old) var
          if cs._count == (unsafePartial fromJust <<< uIntNFromBigNumber <<< embed $ 3)
             then pure TerminateEvent
             else pure ContinueEvent
      _ <- runWeb3 provider $ hangOutTillBlock later
      _ <- traverse (setter simpleStorage primaryAccount) values
      _ <- joinFiber f
      val <- takeVar var
      fromFoldable [10,9,8] `shouldEqual` fromFoldable (map (unsafeToInt <<< unUIntN) val)


    where
       setter address account n = do
         let txOptions = defaultTransactionOptions # _from .~ Just account
                                                   # _to .~ Just address
         hx <- runWeb3 provider $ SimpleStorage.setCount txOptions {_count: n}
         liftEff <<< log $ "setCount: " <> show n <> ", tx hash: " <> show hx
         --liftAff $ runWeb3 provider $ hangOutTillTx hx
         liftAff $ delay (Milliseconds 500.0) -- we should probably use eth_newBlockFilter instead
       hangOutTillBlock bn = do
         bn' <- eth_blockNumber
         if bn' >= bn then pure unit else liftAff (delay (Milliseconds 1000.0)) *> hangOutTillBlock bn 
       waitBlock = do
         n <- eth_blockNumber
         let next = wrap $ embed 1 + unwrap n
         hangOutTillBlock next
       -- this causes a `(NonEmptyList (NonEmpty (ErrorAtIndex 0 (TypeMismatch "object" "object")) Nil))` for now
       --hangOutTillTx tx = do
       --  tx' <- eth_getTransactionReceipt tx
       --  liftEff <<< log $ "txReceipt: " <> show tx'
       --  if true then pure unit else liftAff (delay (Milliseconds 1000.0)) *> hangOutTillTx tx
