module Contracts.SimpleStorage where

import Prelude
import Data.Lens ((.~))
import Data.Maybe (Maybe(..))
import Network.Ethereum.Web3.Types (HexString(..), CallMode, Web3MA, BigNumber, _address, _topics, _fromBlock, _toBlock, defaultFilter)
import Network.Ethereum.Web3.Contract (class EventFilter, callAsync, sendTxAsync)
import Network.Ethereum.Web3.Solidity
import Partial.Unsafe (unsafeCrashWith)
--------------------------------------------------------------------------------
-- | CountFn
--------------------------------------------------------------------------------

data CountFn = CountFn 

instance abiEncodingCountFn :: ABIEncoding CountFn where
	toDataBuilder CountFn = HexString "06661abd"
	fromDataParser = unsafeCrashWith "Function type has no parser."

count :: forall e . Address -> Maybe Address -> CallMode -> Web3MA e (UIntN (D2 :& D5 :& D6))
count x0 x1 x2 = unSingleton <$> callAsync x0 x1 x2 CountFn

--------------------------------------------------------------------------------
-- | SetCountFn
--------------------------------------------------------------------------------

data SetCountFn = SetCountFn (UIntN (D2 :& D5 :& D6))

instance abiEncodingSetCountFn :: ABIEncoding SetCountFn where
	toDataBuilder (SetCountFn x0) = HexString "d14e62b8" <> toDataBuilder (Singleton x0)
	fromDataParser = unsafeCrashWith "Function type has no parser."

setCount :: forall e . Maybe Address -> Address -> BigNumber -> (UIntN (D2 :& D5 :& D6)) -> Web3MA e HexString
setCount x0 x1 x2 x3 = sendTxAsync x0 x1 x2 (SetCountFn x3)

--------------------------------------------------------------------------------
-- | CountSet
--------------------------------------------------------------------------------

data CountSet = CountSet (UIntN (D2 :& D5 :& D6))

instance abiEncodingCountSet :: ABIEncoding CountSet where
	toDataBuilder = unsafeCrashWith "Event type has no builder."
	fromDataParser = uncurry1 CountSet <$> fromDataParser

instance eventFilterCountSet :: EventFilter CountSet where
	eventFilter _ addr = defaultFilter
		# _address .~ Just addr
		# _topics .~ Just [Just (HexString "a32bc18230dd172221ac5c4821a5f1f1a831f27b1396d244cdd891c58f132435")]
		# _fromBlock .~ Nothing
		# _toBlock .~ Nothing