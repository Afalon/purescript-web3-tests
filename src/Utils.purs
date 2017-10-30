module Utils where

import Data.Symbol
import Prelude

import Control.Error.Util (note)
import Control.Monad.Aff (Aff)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Exception (EXCEPTION, throwException, error, throw)
import Control.Monad.Except (runExcept)
import Data.Argonaut.Parser (jsonParser)
import Data.Argonaut.Prisms (_Object, _String)
import Data.Either (Either)
import Data.Either (either)
import Data.EitherR (fmapL)
import Data.Foreign (renderForeignError)
import Data.Foreign.Class (decode, encode)
import Data.Lens ((^?))
import Data.Lens.Index (ix)
import Data.Maybe (maybe)
import Network.Ethereum.Web3.Api (net_version)
import Network.Ethereum.Web3.Provider (httpProvider)
import Network.Ethereum.Web3.Types (Address(..), ETH, Provider, runWeb3MA)
import Node.Encoding (Encoding(UTF8))
import Node.FS.Aff (FS, readTextFile)
import Node.Process (PROCESS, lookupEnv)
--import Control.Monad.Eff.Console (logShow)

makeProvider :: forall eff . Eff (process :: PROCESS, eth :: ETH, exception :: EXCEPTION | eff) Provider
makeProvider = do
  murl <- lookupEnv "NODE_URL"
  url <- maybe (throw "Must provide node url") pure murl
  httpProvider url

newtype Contract (name :: Symbol) =
  Contract { address :: Address
           }

getDeployedContract :: forall eff name .
                       IsSymbol name
                    => SProxy name
                    -> Provider
                    -> Aff (fs :: FS, eth :: ETH, exception :: EXCEPTION | eff) (Contract name)
getDeployedContract sproxy p = do
  let fname = "./build/contracts/" <> reflectSymbol sproxy <> ".json"
  nodeId <- runWeb3MA p net_version
  ejson <- jsonParser <$> readTextFile UTF8 fname
  addr <- liftEff $ either throw pure $ do
    contractJson <- ejson
    networks <- note "artifact missing networks key" $ contractJson ^? _Object <<< ix "networks"
    net <- note ("artifact missing network: " <> show nodeId)  $ networks ^? _Object <<< ix (show nodeId)
    addr <- note "artifact has no address" $ net ^? _Object <<< ix "address" <<< _String
    fmapL (show <<< map renderForeignError) <<< runExcept <<< decode <<< encode $ addr
  pure $ Contract { address: addr
                  }

{-
foo = void <<< launchAff $ do
  p <- liftEff makeProvider
  r <- getDeployedAddress p "SimpleStorage"
  liftEff $ logShow r
  pure r
-}
