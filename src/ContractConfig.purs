module ContractConfig
  ( simpleStorageConfig
  , mockERC20Config
  , payableTestConfig
  , simpleErrorTestConfig
  , complexStorageConfig
  ) where

import Chanterelle.Internal.Types (ContractConfig, NoArgs, noArgs, constructorNoArgs)

--------------------------------------------------------------------------------
-- | SimpleStorage
--------------------------------------------------------------------------------

simpleStorageConfig
  :: ContractConfig NoArgs
simpleStorageConfig =
    { filepath : "./build/contracts/SimpleStorage.json"
    , name : "SimpleStorage"
    , constructor : constructorNoArgs
    , unvalidatedArgs : noArgs
    }

--------------------------------------------------------------------------------
-- | MockERC20
--------------------------------------------------------------------------------

mockERC20Config
  :: ContractConfig NoArgs
mockERC20Config =
  { filepath : "./build/contracts/MockERC20.json"
  , name : "MockERC20"
  , constructor : constructorNoArgs
  , unvalidatedArgs : noArgs
  }

--------------------------------------------------------------------------------
-- | PayableTest
--------------------------------------------------------------------------------

payableTestConfig
  :: ContractConfig NoArgs
payableTestConfig =
  { filepath : "./build/contracts/PayableTest.json"
  , name : "PayableTest"
  , constructor : constructorNoArgs
  , unvalidatedArgs : noArgs
  }
--------------------------------------------------------------------------------
-- | SimpleErrorTest
--------------------------------------------------------------------------------

simpleErrorTestConfig
  :: ContractConfig NoArgs
simpleErrorTestConfig =
  { filepath : "./build/contracts/SimpleErrorTest.json"
  , name : "SimpleErrorTest"
  , constructor : constructorNoArgs
  , unvalidatedArgs : noArgs
  }

--------------------------------------------------------------------------------
-- | ComplexStorage
--------------------------------------------------------------------------------

complexStorageConfig
  :: ContractConfig NoArgs
complexStorageConfig =
  { filepath : "./build/contracts/ComplexStorage.json"
  , name : "ComplexStorage"
  , constructor : constructorNoArgs
  , unvalidatedArgs : noArgs
  }
