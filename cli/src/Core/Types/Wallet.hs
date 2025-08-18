{-# LANGUAGE StrictData #-}

module Core.Types.Wallet
    ( Wallet (..)
    , Mnemonics (..)
    ) where

import Core.Types.Basic
import Core.Types.Tx
import Data.Aeson
    ( FromJSON (..)
    , ToJSON (..)
    , object
    , withObject
    , (.:)
    , (.=)
    )
import Data.Text (Text)

data Wallet = Wallet
    { address :: Address
    , owner :: Owner
    , sign :: UnsignedTx -> Either SignTxError SignedTx
    }

instance Show Wallet where
    show (Wallet addr owner _) =
        "Wallet { address: " ++ show addr ++ ", owner: " ++ show owner ++ " }"

instance Eq Wallet where
    (Wallet addr1 owner1 _) == (Wallet addr2 owner2 _) =
        addr1 == addr2 && owner1 == owner2

data Mnemonics where
    ClearText :: Text -> Mnemonics
    deriving (Show, Eq)

instance ToJSON Mnemonics where
    toJSON (ClearText mnemonics) =
        object ["mnemonics" .= mnemonics]

instance FromJSON Mnemonics where
    parseJSON = withObject "Mnemonics" $ \v -> do
        mnemonics <- v .: "mnemonics"
        pure $ ClearText mnemonics
