module Core.Types.Mnemonics
    ( Mnemonics (..)
    , mnemonicsParser
    ) where

import Data.Aeson
    ( Object
    , ToJSON (..)
    , object
    , (.=)
    )
import Data.Aeson qualified as Aeson
import Data.ByteString.Lazy qualified as BL
import Data.Text (Text)
import OptEnvConf
    ( Alternative ((<|>))
    , Parser
    , checkMapIO
    , conf
    , env
    , help
    , metavar
    , reader
    , setting
    , str
    , withConfig
    )

data Mnemonics where
    ClearText :: Text -> Mnemonics
    Decryptable :: Text -> Text -> Mnemonics

instance ToJSON Mnemonics where
    toJSON (ClearText mnemonics) =
        object ["mnemonics" .= mnemonics]
    toJSON (Decryptable mnemonics _) =
        object ["encryptedMnemonics" .= mnemonics]

mnemonicsClearTextOption :: Parser Text
mnemonicsClearTextOption =
    setting
        [ help "The mnemonics for the wallet in clear text"
        , conf "mnemonics"
        , metavar "MNEMONICS"
        ]

mnemonicsEncryptedOption :: Parser Text
mnemonicsEncryptedOption =
    setting
        [ help "The encrypted mnemonics for the wallet"
        , conf "encryptedMnemonics"
        , metavar "ENCRYPTED_MNEMONICS"
        ]

walletPassphraseOption
    :: Parser Text
walletPassphraseOption =
    setting
        [ env "ANTI_WALLET_PASSPHRASE"
        , metavar "PASSPHRASE"
        , help "The passphrase for the encrypted mnemonics"
        , reader str
        ]

walletFileOption :: Parser FilePath
walletFileOption =
    setting
        [ env "ANTI_WALLET_FILE"
        , metavar "FILEPATH"
        , help "The file path to the wallet secrets"
        , reader str
        ]

coreMnemonicsParser :: Parser Mnemonics
coreMnemonicsParser =
    ClearText <$> mnemonicsClearTextOption
        <|> Decryptable <$> mnemonicsEncryptedOption <*> walletPassphraseOption

mnemonicsObject :: Parser Object
mnemonicsObject =
    checkMapIO
        (fmap Aeson.eitherDecode . BL.readFile)
        walletFileOption

mnemonicsParser :: Parser Mnemonics
mnemonicsParser = withConfig (Just <$> mnemonicsObject) coreMnemonicsParser
