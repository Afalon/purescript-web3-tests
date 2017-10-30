module Utils where

import Prelude

import Control.Error.Util (note)
import Control.Monad.Aff (Aff)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Exception (EXCEPTION, throw)
import Control.Monad.Except (runExcept)
import Data.Argonaut.Parser (jsonParser)
import Data.Argonaut.Prisms (_Object, _String)
import Data.Either (Either)
import Data.EitherR (fmapL)
import Data.Foreign (renderForeignError)
import Data.Foreign.Class (decode)
import Data.Foreign.JSON (parseJSON)
import Data.Lens ((^?))
import Data.Lens.Index (ix)
import Data.Maybe (maybe)
import Network.Ethereum.Web3.Api (net_version)
import Network.Ethereum.Web3.Provider (httpProvider)
import Network.Ethereum.Web3.Types (Address, ETH, Provider, fromHexString, runWeb3MA)
import Node.Encoding (Encoding(UTF8))
import Node.FS.Aff (FS, readTextFile)
import Node.Process (PROCESS, lookupEnv)


makeProvider :: forall eff . Eff (process :: PROCESS, eth :: ETH, exception :: EXCEPTION | eff) Provider
makeProvider = do
  murl <- lookupEnv "NODE_URL"
  url <- maybe (throw "Must provide node url") pure murl
  httpProvider url


getDeployedAddress :: forall eff . Provider -> String -> Aff (fs :: FS, eth :: ETH | eff) (Either String Address)
getDeployedAddress p contractName = do
  let fname = "./build/contracts/" <> contractName <> ".json"
  nodeId <- fromHexString <$> runWeb3MA p net_version
  ejson <- jsonParser <$> readTextFile UTF8 fname
  pure $ do
    contractJson <- ejson
    networks <- note "artifact missing networks key" $ contractJson ^? _Object <<< ix "networks"
    addr <- note "artifact missing address" $ networks ^? _Object <<< ix "address" <<< _String
    fmapL (show <<< map renderForeignError) <<< runExcept $ decode =<< parseJSON addr

