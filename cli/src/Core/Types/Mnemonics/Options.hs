module Core.Types.Mnemonics.Options
    ( mnemonicsParser
    , walletPassphraseCommon
    , walletFileOption
    ) where

import Core.Encryption (decryptText)
import Core.Types.Mnemonics (Mnemonics (..), MnemonicsPhase (..))
import Data.Aeson
    ( Object
    )
import Data.Aeson qualified as Aeson
import Data.ByteString.Lazy qualified as BL
import Data.Text (Text)
import OptEnvConf
    ( Alternative ((<|>))
    , Builder
    , Parser
    , checkMapIO
    , conf
    , env
    , help
    , mapIO
    , metavar
    , reader
    , setting
    , str
    , withConfig
    )

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
walletPassphraseCommon :: [Builder a]
walletPassphraseCommon =
    [ env "ANTI_WALLET_PASSPHRASE"
    , metavar "PASSPHRASE"
    , help "The passphrase for the encrypted mnemonics"
    ]
walletPassphraseOption
    :: Parser (Text -> IO Text)
walletPassphraseOption =
    fmap decryptText
        $ setting
        $ reader str : walletPassphraseCommon

walletFileOption :: Parser FilePath
walletFileOption =
    setting
        [ env "ANTI_WALLET_FILE"
        , metavar "FILEPATH"
        , help "The file path to the wallet secrets mnemonics"
        , reader str
        ]

coreMnemonicsParser :: Parser (Mnemonics 'DecryptedS)
coreMnemonicsParser =
    fmap ClearText
        $ mnemonicsClearTextOption
        <|> mapIO id (walletPassphraseOption <*> mnemonicsEncryptedOption)

mnemonicsObject :: Parser Object
mnemonicsObject =
    checkMapIO
        (fmap Aeson.eitherDecode . BL.readFile)
        walletFileOption

mnemonicsParser :: Parser (Mnemonics 'DecryptedS)
mnemonicsParser = withConfig (Just <$> mnemonicsObject) coreMnemonicsParser
