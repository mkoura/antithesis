module Adversary (adversary, Message (..), toString)
where

import Adversary.ChainSync (clientChainSync)
import Data.Aeson (FromJSON, ToJSON)
import Data.Aeson qualified as Aeson
import Data.Text.Lazy qualified as Text
import Data.Text.Lazy.Encoding qualified as Text
import GHC.Generics (Generic)
import Ouroboros.Consensus.Cardano (ShelleyGenesis(ShelleyGenesis), ProtVer (ProtVer))

newtype Message = Startup {arguments :: [String]}
    deriving (Eq, Show, Generic, FromJSON, ToJSON)

adversary :: [String] -> IO Message
adversary args@(host : port : genesis : _) = do
    Just shelleyGenesis <- Aeson.decodeFileStrict @ShelleyGenesis genesis
    clientChainSync host (read port) protVer shelleyGenesis
    pure Startup{arguments = args}
  where
    protVer = ProtVer maxBound 0

adversary _ = error "Expected host and port arguments"

toString :: Message -> String
toString = Text.unpack . Text.decodeUtf8 . Aeson.encode
