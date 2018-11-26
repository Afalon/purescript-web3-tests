module Main where

import Prelude

import Chanterelle (deployMain)
import Chanterelle.Deploy (deployContract)
import Chanterelle.Internal.Types (DeployM, DeployConfig(..))
import ContractConfig (simpleStorageConfig, mockERC20Config, payableTestConfig, simpleErrorTestConfig, complexStorageConfig)
import Effect (Effect)
import Control.Monad.Reader.Class (ask)
import Data.Lens ((?~))
import Data.Maybe (fromJust)
import Network.Ethereum.Web3 (Address, _from, _gas, defaultTransactionOptions)
import Network.Ethereum.Core.BigNumber (parseBigNumber, decimal)
import Partial.Unsafe (unsafePartial)

main :: Effect Unit
main = deployMain deployScript


type DeployResults =
  ( simpleStorage :: Address
  , mockERC20 :: Address
  , payableTest :: Address
  , simpleErrorTest :: Address
  , complexStorage :: Address
  )

deployScript :: DeployM (Record DeployResults)
deployScript = do
  deployCfg@(DeployConfig {primaryAccount}) <- ask
  let bigGasLimit = unsafePartial fromJust $ parseBigNumber decimal "4712388"
      txOpts = defaultTransactionOptions # _from ?~ primaryAccount
                                         # _gas ?~ bigGasLimit
  simpleStorage <- deployContract txOpts simpleStorageConfig
  mockERC20 <- deployContract txOpts mockERC20Config
  payableTest <- deployContract txOpts payableTestConfig
  simpleErrorTest <- deployContract txOpts simpleErrorTestConfig
  complexStorage <- deployContract txOpts complexStorageConfig
  pure { simpleStorage: simpleStorage.deployAddress
       , mockERC20: mockERC20.deployAddress
       , payableTest: payableTest.deployAddress
       , simpleErrorTest: simpleErrorTest.deployAddress
       , complexStorage: complexStorage.deployAddress
       }
