{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
module Adversary where
import GHC.Generics (Generic)
import Data.Aeson (FromJSON, ToJSON)
import qualified Data.Aeson as Aeson
import qualified Data.Text.Lazy.Encoding as Text
import qualified Data.Text.Lazy as Text

data Message = Startup {arguments :: [String]}
  deriving (Eq, Show, Generic, FromJSON, ToJSON)

adversary :: [String] -> IO Message
adversary args = pure Startup {arguments = args}

toString :: Message -> String
toString = Text.unpack . Text.decodeUtf8 . Aeson.encode
