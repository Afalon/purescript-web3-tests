module ComplexStorageSpec (complexStorageSpec) where

import Prelude

import Chanterelle.Test (TestConfig)
import Contracts.ComplexStorage as ComplexStorage
import Control.Monad.Aff (joinFiber)
import Control.Monad.Aff.AVar (AVAR, makeEmptyVar, putVar, takeVar)
import Control.Monad.Aff.Class (liftAff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Data.Array ((!!))
import Data.ByteString as BS
import Data.Either (fromRight)
import Data.Lens.Setter ((.~))
import Data.Maybe (Maybe(..), fromJust)
import Network.Ethereum.Core.BigNumber (hexadecimal, parseBigNumber)
import Network.Ethereum.Web3 (ETH, Address, ChainCursor(Latest), EventAction(TerminateEvent), _from, _fromBlock, _gas, _to, _toBlock, defaultTransactionOptions, embed, event, eventFilter, forkWeb3, fromByteString, intNFromBigNumber, nilVector, runWeb3, uIntNFromBigNumber, (:<))
import Network.Ethereum.Web3.Api (eth_blockNumber)
import Network.Ethereum.Web3.Solidity.Sizes (s16, s2, s224, s256)
import Partial.Unsafe (unsafePartial)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)
import Type.Prelude (Proxy(..))

complexStorageSpec
  :: forall r eff.
     TestConfig (complexStorage :: Address | r)
  -> Spec (avar :: AVAR, eth :: ETH, console :: CONSOLE |eff) Unit
complexStorageSpec {provider, accounts, complexStorage} =
  describe "interacting with a ComplexStorage Contract" do
    it "can set the values of complex storage" $ do
      let primaryAccount = unsafePartial $ fromJust $ accounts !! 0
      var <- makeEmptyVar
      let uint = unsafePartial $ fromJust <<< uIntNFromBigNumber s256 <<< embed $ 1
          int = unsafePartial $ fromJust <<< intNFromBigNumber s256 <<< embed $ (negate 1)
          bool = true
          int224 = unsafePartial $ fromJust <<< intNFromBigNumber s224 <<< embed $  221
          bools = true :< false :< nilVector
          ints = 
            [ unsafePartial $ fromJust <<< intNFromBigNumber s256 <<< embed $ 1
            , unsafePartial $ fromJust <<< intNFromBigNumber s256 <<< embed $ negate 1
            , unsafePartial $ fromJust <<< intNFromBigNumber s256 <<< embed $ 3
            ]
          string = "hello"
          bytes16 = unsafePartial $ fromJust $ fromByteString s16 =<< flip BS.fromString BS.Hex "12345678123456781234567812345678"
          elem = unsafePartial $ fromJust $ fromByteString s2 =<< flip BS.fromString BS.Hex "1234"
          bytes2s = [elem :< elem :< elem :< elem :< nilVector, elem :< elem :< elem :< elem :< nilVector]
          txOptions = defaultTransactionOptions # _from .~ Just primaryAccount
                                                # _to .~ Just complexStorage
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
      let filterValsSet = eventFilter (Proxy :: Proxy ComplexStorage.ValsSet) complexStorage
                          # _fromBlock .~ Latest --(BN <<< wrap <<< embed $ 4732740)
                          # _toBlock   .~ Latest --(BN <<< wrap <<< embed $ 4732754)
      fiber <- forkWeb3 provider $
        event filterValsSet $ \e@(ComplexStorage.ValsSet vs) -> do
          liftEff $ log $ "Received event: " <> show e
          liftEff $ log $ "Value of `i` field is: " <> show vs.i
          _ <- liftAff $ putVar e var
          pure TerminateEvent
      bn <- unsafePartial fromRight <$> runWeb3 provider eth_blockNumber
      liftEff $ log $ "setting values"
      hx <- runWeb3 provider $ ComplexStorage.setValues txOptions arg
      liftEff $ log $ "setValues tx hash: " <> show hx
      _ <- joinFiber fiber
      ev <- takeVar var
      ev `shouldEqual` ComplexStorage.ValsSet {a: uint, b: int, c: bool, d: int224, e: bools, f: ints, g: string, h: bytes16,  i:bytes2s}
