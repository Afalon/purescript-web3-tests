module ComplexStorageSpec where

import Prelude
import Contracts.ComplexStorage as ComplexStorage
import Control.Monad.Aff (joinFiber)
import Control.Monad.Aff.AVar (makeEmptyVar, putVar, takeVar)
import Control.Monad.Aff.Class (liftAff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console (log)
import Data.Array ((!!))
import Data.ByteString as BS
import Data.Either (fromRight)
import Data.Lens.Setter ((.~))
import Data.Maybe (Maybe(..), fromJust)
import Data.Symbol (SProxy(..))
import Network.Ethereum.Web3 (event, eventFilter, runWeb3, intNFromBigNumber, uIntNFromBigNumber, embed, (:<), nilVector, fromByteString, defaultTransactionOptions, _from, _to, _gas, parseBigNumber, hexadecimal, _fromBlock, ChainCursor(..), _toBlock, forkWeb3, EventAction(..))
import Network.Ethereum.Web3.Api (eth_blockNumber, eth_getAccounts)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)
import Type.Prelude (Proxy(..))
import Partial.Unsafe (unsafePartial)
import Utils (makeProvider, getDeployedContract, Contract(..))

complexStorageSpec :: forall r . Spec _ Unit
complexStorageSpec =
  describe "interacting with a ComplexStorage Contract" do
    it "can set the values of complex storage" $ do
      httpP <- liftEff makeProvider
      accounts <- unsafePartial fromRight <$> runWeb3 httpP eth_getAccounts
      let primaryAccount = unsafePartial $ fromJust $ accounts !! 0
      var <- makeEmptyVar
      Contract complexStorage <- getDeployedContract (SProxy :: SProxy "ComplexStorage")
      let uint = unsafePartial $ fromJust <<< uIntNFromBigNumber <<< embed $ 1
          int = unsafePartial $ fromJust <<< intNFromBigNumber <<< embed $ (negate 1)
          bool = true
          int224 = unsafePartial $ fromJust <<< intNFromBigNumber <<< embed $  221
          bools = true :< false :< nilVector
          ints = [unsafePartial $ fromJust <<< intNFromBigNumber <<< embed $ 1, unsafePartial $ fromJust <<< intNFromBigNumber <<< embed $ negate 1, unsafePartial $ fromJust <<< intNFromBigNumber <<< embed $  3]
          string = "hello"
          bytes16 = unsafePartial $ fromJust $ fromByteString =<< flip BS.fromString BS.Hex "12345678123456781234567812345678"
          elem = unsafePartial $ fromJust $ fromByteString =<< flip BS.fromString BS.Hex "1234"
          bytes2s = [elem :< elem :< elem :< elem :< nilVector, elem :< elem :< elem :< elem :< nilVector]
          txOptions = defaultTransactionOptions # _from .~ Just primaryAccount
                                                # _to .~ Just complexStorage.address
                                                # _gas .~ parseBigNumber hexadecimal "0x2dc2dc"
          arg = { _uintVal : uint
                , _intVal : int
                , _boolVal : bool
                , _int224Val : int224
                , _boolVectorVal : bools
                , _intListVal : ints
                , _stringVal : string
                , _bytes16Val : bytes16
                , _bytes2VectorListVal : bytes2s
                }
      let filterValsSet = eventFilter (Proxy :: Proxy ComplexStorage.ValsSet) complexStorage.address
                          # _fromBlock .~ Latest --(BN <<< wrap <<< embed $ 4732740)
                          # _toBlock   .~ Latest --(BN <<< wrap <<< embed $ 4732754)
      fiber <- forkWeb3 httpP $
        event filterValsSet $ \e@(ComplexStorage.ValsSet vs) -> do
          liftEff $ log $ "Received event: " <> show e
          liftEff $ log $ "Value of `i` field is: " <> show vs.i
          _ <- liftAff $ putVar e var
          pure TerminateEvent
      bn <- unsafePartial fromRight <$> runWeb3 httpP eth_blockNumber
      liftEff $ log $ "setting values"
      hx <- runWeb3 httpP $ ComplexStorage.setValues txOptions arg
      liftEff $ log $ "setValues tx hash: " <> show hx
      _ <- joinFiber fiber
      ev <- takeVar var
      ev `shouldEqual` ComplexStorage.ValsSet {a: uint, b: int, c: bool, d: int224, e: bools, f: ints, g: string, h: bytes16,  i:bytes2s}
