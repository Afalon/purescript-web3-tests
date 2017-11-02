module Contracts.Migrations where

import Prelude
import Data.Monoid (mempty)
import Data.Lens ((.~))
import Text.Parsing.Parser (fail)
import Data.Maybe (Maybe(..))
import Network.Ethereum.Web3.Types (HexString(..), CallMode, Web3, BigNumber, _address, _topics, _fromBlock, _toBlock, defaultFilter)
import Network.Ethereum.Web3.Provider (class IsAsyncProvider)
import Network.Ethereum.Web3.Contract (class EventFilter, call, sendTx)
import Network.Ethereum.Web3.Solidity
--------------------------------------------------------------------------------
-- | EthupgradeFn
--------------------------------------------------------------------------------

data EthupgradeFn = EthupgradeFn Address

instance abiEncodingEthupgradeFn :: ABIEncoding EthupgradeFn where
	toDataBuilder (EthupgradeFn x0) = HexString "0900f010" <> toDataBuilder (Singleton x0)
	fromDataParser = fail "Function type has no parser."

ethupgrade :: forall p e . IsAsyncProvider p => Maybe Address -> Address -> BigNumber -> Address -> Web3 p e HexString
ethupgrade x0 x1 x2 x3 = sendTx x0 x1 x2 (EthupgradeFn x3)

--------------------------------------------------------------------------------
-- | Ethlast_completed_migrationFn
--------------------------------------------------------------------------------

data Ethlast_completed_migrationFn = Ethlast_completed_migrationFn 

instance abiEncodingEthlast_completed_migrationFn :: ABIEncoding Ethlast_completed_migrationFn where
	toDataBuilder Ethlast_completed_migrationFn = HexString "445df0ac"
	fromDataParser = fail "Function type has no parser."

ethlast_completed_migration :: forall p e . IsAsyncProvider p => Address -> Maybe Address -> CallMode -> Web3 p e (UIntN (D2 :& D5 :& D6))
ethlast_completed_migration x0 x1 x2 = unSingleton <$> call x0 x1 x2 Ethlast_completed_migrationFn

--------------------------------------------------------------------------------
-- | EthownerFn
--------------------------------------------------------------------------------

data EthownerFn = EthownerFn 

instance abiEncodingEthownerFn :: ABIEncoding EthownerFn where
	toDataBuilder EthownerFn = HexString "8da5cb5b"
	fromDataParser = fail "Function type has no parser."

ethowner :: forall p e . IsAsyncProvider p => Address -> Maybe Address -> CallMode -> Web3 p e Address
ethowner x0 x1 x2 = unSingleton <$> call x0 x1 x2 EthownerFn

--------------------------------------------------------------------------------
-- | EthsetCompletedFn
--------------------------------------------------------------------------------

data EthsetCompletedFn = EthsetCompletedFn (UIntN (D2 :& D5 :& D6))

instance abiEncodingEthsetCompletedFn :: ABIEncoding EthsetCompletedFn where
	toDataBuilder (EthsetCompletedFn x0) = HexString "fdacd576" <> toDataBuilder (Singleton x0)
	fromDataParser = fail "Function type has no parser."

ethsetCompleted :: forall p e . IsAsyncProvider p => Maybe Address -> Address -> BigNumber -> (UIntN (D2 :& D5 :& D6)) -> Web3 p e HexString
ethsetCompleted x0 x1 x2 x3 = sendTx x0 x1 x2 (EthsetCompletedFn x3)

