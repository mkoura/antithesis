{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module Adversary where

import Adversary.ChainSync (clientChainSync)
import Data.Aeson (FromJSON, ToJSON)
import qualified Data.Aeson as Aeson
import qualified Data.Text.Lazy as Text
import qualified Data.Text.Lazy.Encoding as Text
import GHC.Generics (Generic)

data Message = Startup {arguments :: [String]}
  deriving (Eq, Show, Generic, FromJSON, ToJSON)

adversary :: [String] -> IO Message
adversary args@(host : port : _) = do
  clientChainSync host (read port)
  pure Startup {arguments = args}

toString :: Message -> String
toString = Text.unpack . Text.decodeUtf8 . Aeson.encode
