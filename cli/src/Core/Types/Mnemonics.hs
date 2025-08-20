module Core.Types.Mnemonics
    ( Mnemonics (..)
    ) where

import Data.Aeson
    ( ToJSON (..)
    , object
    , (.=)
    )
import Data.Text (Text)

data Mnemonics where
    ClearText :: Text -> Mnemonics
    Decryptable :: Text -> Text -> Mnemonics

instance ToJSON Mnemonics where
    toJSON (ClearText mnemonics) =
        object ["mnemonics" .= mnemonics]
    toJSON (Decryptable mnemonics _) =
        object ["encryptedMnemonics" .= mnemonics]
