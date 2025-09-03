module Core.Types.Mnemonics
    ( Mnemonics (..)
    , MnemonicsPhase (..)
    , readDecryptedMnemonicFile
    , readEncryptedMnemonicFile
    ) where

import Control.Exception (try)
import Data.Aeson
    ( FromJSON (..)
    , ToJSON (..)
    , object
    , withObject
    , (.:?)
    , (.=)
    )
import Data.Aeson.Types
    ( Parser
    )
import Data.Text (Text)

import Data.Aeson qualified as Aeson
import Data.ByteString.Lazy qualified as BL

data MnemonicsPhase = EncryptedS | DecryptedS

data Mnemonics phase where
    ClearText :: Text -> Mnemonics 'DecryptedS
    Encrypted :: Text -> Mnemonics 'EncryptedS

instance ToJSON (Mnemonics phase) where
    toJSON (ClearText mnemonics) =
        object ["mnemonics" .= mnemonics]
    toJSON (Encrypted mnemonics) =
        object ["encryptedMnemonics" .= mnemonics]

instance FromJSON (Mnemonics 'DecryptedS) where
    parseJSON = withObject "Mnemonics 'DecryptedS" $ \o -> do
        mDecrypted <- o .:? "mnemonics" :: Parser (Maybe Text)
        case mDecrypted of
            Just mnemonics -> pure $ ClearText mnemonics
            _ -> error "expecting 'mnemonics' field"

instance FromJSON (Mnemonics 'EncryptedS) where
    parseJSON = withObject "Mnemonics 'EncryptedS" $ \o -> do
        mEncrypted <- o .:? "encryptedMnemonics" :: Parser (Maybe Text)
        case mEncrypted of
            Just txt -> pure $ Encrypted txt
            _ -> error "expecting 'encryptedMnemonics' field"

readDecryptedMnemonicFile
    :: FilePath -> IO (Either String ((Mnemonics 'DecryptedS)))
readDecryptedMnemonicFile fp = do
    econtent <- try $ BL.readFile fp
    case econtent of
        Left (e :: IOError) -> return $ Left $ show e
        Right content -> return $ Aeson.eitherDecode content

readEncryptedMnemonicFile
    :: FilePath -> IO (Either String ((Mnemonics 'EncryptedS)))
readEncryptedMnemonicFile fp = do
    econtent <- try $ BL.readFile fp
    case econtent of
        Left (e :: IOError) -> return $ Left $ show e
        Right content -> return $ Aeson.eitherDecode content
