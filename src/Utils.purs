module Utils where

import Prelude

import Control.Error.Util (note)
import Control.Monad.Aff (Aff, liftEff')
import Control.Monad.Aff.Class (liftAff)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Exception (EXCEPTION, throw)
import Control.Monad.Eff.Unsafe (unsafeCoerceEff)
import Control.Monad.Except (runExcept)
import Data.Argonaut.Parser (jsonParser)
import Data.Argonaut.Prisms (_Object, _String)
import Data.Either (Either(..), either, fromRight)
import Data.EitherR (fmapL)
import Data.Foreign (renderForeignError)
import Data.Foreign.Class (decode, encode)
import Data.Lens ((^?))
import Data.Lens.Index (ix)
import Data.Maybe (maybe)
import Data.Symbol (class IsSymbol, SProxy, reflectSymbol)
import Network.Ethereum.Web3.Api (net_version)
import Network.Ethereum.Web3.Provider (class IsAsyncProvider, Provider, httpProvider, runWeb3)
import Network.Ethereum.Web3.Types (Address, ETH, Web3(..))
import Node.Encoding (Encoding(UTF8))
import Node.FS.Aff (FS, readTextFile)
import Node.Process (lookupEnv)
import Type.Proxy(Proxy(..))
import Partial.Unsafe (unsafePartial)
makeProvider :: forall eff . Eff (eth :: ETH, exception :: EXCEPTION | eff) Provider
makeProvider = unsafeCoerceEff $ do
  murl <- lookupEnv "NODE_URL"
  url <- maybe (pure "http://localhost:8545") pure murl
  httpProvider url

data HttpProvider

httpP :: Proxy HttpProvider
httpP = Proxy

instance providerHttp :: IsAsyncProvider HttpProvider where
  getAsyncProvider = liftAff <<< liftEff' $ makeProvider

newtype Contract (name :: Symbol) =
  Contract { address :: Address
           }

getDeployedContract :: forall eff name .
                       IsSymbol name
                    => SProxy name
                    -> Aff (fs :: FS, eth :: ETH, exception :: EXCEPTION | eff) (Contract name)
getDeployedContract sproxy = do
  let fname = "./build/contracts/" <> reflectSymbol sproxy <> ".json"
  enodeId <- runWeb3 httpP net_version
  case enodeId of
    Left err -> liftEff <<< throw <<< show $ err
    Right nodeId -> do
      ejson <- jsonParser <$> readTextFile UTF8 fname
      addr <- liftEff $ either throw pure $ do
        contractJson <- ejson
        networks <- note "artifact missing networks key" $ contractJson ^? _Object <<< ix "networks"
        net <- note ("artifact missing network: " <> show nodeId)  $ networks ^? _Object <<< ix (show nodeId)
        addr <- note "artifact has no address" $ net ^? _Object <<< ix "address" <<< _String
        fmapL (show <<< map renderForeignError) <<< runExcept <<< decode <<< encode $ addr
      pure $ Contract { address: addr
                      }

