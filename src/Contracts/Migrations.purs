module Contracts.Migrations where

import Prelude
import Data.Lens ((.~))
import Data.Maybe (Maybe(..))
import Network.Ethereum.Web3.Types (HexString(..), CallMode, Web3MA, BigNumber, _address, _topics, _fromBlock, _toBlock, defaultFilter)
import Network.Ethereum.Web3.Contract (class EventFilter, callAsync, sendTxAsync)
import Network.Ethereum.Web3.Solidity
import Partial.Unsafe (unsafeCrashWith)
--------------------------------------------------------------------------------
-- | UpgradeFn
--------------------------------------------------------------------------------

data UpgradeFn = UpgradeFn Address

instance abiEncodingUpgradeFn :: ABIEncoding UpgradeFn where
	toDataBuilder (UpgradeFn x0) = HexString "0900f010" <> toDataBuilder (Singleton x0)
	fromDataParser = unsafeCrashWith "Function type has no parser."

upgrade :: forall e . Maybe Address -> Address -> BigNumber -> Address -> Web3MA e HexString
upgrade x0 x1 x2 x3 = sendTxAsync x0 x1 x2 (UpgradeFn x3)

--------------------------------------------------------------------------------
-- | Last_completed_migrationFn
--------------------------------------------------------------------------------

data Last_completed_migrationFn = Last_completed_migrationFn 

instance abiEncodingLast_completed_migrationFn :: ABIEncoding Last_completed_migrationFn where
	toDataBuilder Last_completed_migrationFn = HexString "445df0ac"
	fromDataParser = unsafeCrashWith "Function type has no parser."

last_completed_migration :: forall e . Address -> Maybe Address -> CallMode -> Web3MA e (UIntN (D2 :& D5 :& D6))
last_completed_migration x0 x1 x2 = unSingleton <$> callAsync x0 x1 x2 Last_completed_migrationFn

--------------------------------------------------------------------------------
-- | OwnerFn
--------------------------------------------------------------------------------

data OwnerFn = OwnerFn 

instance abiEncodingOwnerFn :: ABIEncoding OwnerFn where
	toDataBuilder OwnerFn = HexString "8da5cb5b"
	fromDataParser = unsafeCrashWith "Function type has no parser."

owner :: forall e . Address -> Maybe Address -> CallMode -> Web3MA e Address
owner x0 x1 x2 = unSingleton <$> callAsync x0 x1 x2 OwnerFn

--------------------------------------------------------------------------------
-- | SetCompletedFn
--------------------------------------------------------------------------------

data SetCompletedFn = SetCompletedFn (UIntN (D2 :& D5 :& D6))

instance abiEncodingSetCompletedFn :: ABIEncoding SetCompletedFn where
	toDataBuilder (SetCompletedFn x0) = HexString "fdacd576" <> toDataBuilder (Singleton x0)
	fromDataParser = unsafeCrashWith "Function type has no parser."

setCompleted :: forall e . Maybe Address -> Address -> BigNumber -> (UIntN (D2 :& D5 :& D6)) -> Web3MA e HexString
setCompleted x0 x1 x2 x3 = sendTxAsync x0 x1 x2 (SetCompletedFn x3)

