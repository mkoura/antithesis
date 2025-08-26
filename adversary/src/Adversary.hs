module Adversary (adversary, Message (..), toString) where

import Adversary.ChainSync (clientChainSync)
import Data.Aeson (FromJSON, ToJSON)
import Data.Aeson qualified as Aeson
import Data.Text.Lazy qualified as Text
import Data.Text.Lazy.Encoding qualified as Text
import GHC.Generics (Generic)
import Ouroboros.Network.Magic (NetworkMagic(..))

newtype Message = Startup {arguments :: [String]}
  deriving (Eq, Show, Generic, FromJSON, ToJSON)

adversary :: [String] -> IO Message
adversary args@(magicArg : host : port : limitArg : _) = do
  let magic = NetworkMagic { unNetworkMagic = read magicArg }
  clientChainSync magic host (read port) (read limitArg)
  pure Startup {arguments = args}
adversary _ = error "Expected network-magic, host, port and sync-length arguments"

toString :: Message -> String
toString = Text.unpack . Text.decodeUtf8 . Aeson.encode
