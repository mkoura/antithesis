{-# LANGUAGE DerivingStrategies #-}

module Core.Options
    ( platformOption
    , repositoryOption
    , commitOption
    , testDirectoryOption
    , usernameOption
    , pubkeyhashOption
    , outputReferenceParser
    , durationOption
    , tryOption
    , tokenIdOption
    , readyMnemonicsOption
    , walletOption
    , downloadAssetsDirectoryOption
    )
where

import Control.Arrow (left)
import Core.Types.Basic
    ( Commit (..)
    , Directory (..)
    , Duration (..)
    , Platform (..)
    , PublicKeyHash (..)
    , Repository (..)
    , RequestRefId (..)
    , TokenId (..)
    , Try (..)
    , Username (..)
    )
import Core.Types.Wallet
    ( Mnemonics (..)
    , Wallet
    )
import Data.Aeson (Object)
import Data.Aeson qualified as Aeson
import Data.ByteString.Lazy qualified as BL
import Data.Text (Text)
import Data.Text qualified as T
import OptEnvConf
    ( Parser
    , auto
    , checkEither
    , checkMapIO
    , conf
    , env
    , help
    , long
    , maybeReader
    , metavar
    , option
    , reader
    , setting
    , short
    , str
    , strOption
    , withConfig
    )
import OptEnvConf.Reader (Reader (..))
import Submitting (readWallet)

platformOption :: Parser Platform
platformOption =
    Platform
        <$> strOption
            [ long "platform"
            , short 'p'
            , metavar "PLATFORM"
            , help "The platform to use"
            , option
            ]

parseRepository :: String -> Maybe Repository
parseRepository repoStr = case break (== '/') repoStr of
    (org, '/' : proj) -> Just $ Repository org proj
    _ -> Nothing

repositoryOption :: Parser Repository
repositoryOption =
    setting
        [ long "repository"
        , short 'r'
        , metavar "ORGANIZATION/PROJECT"
        , help "The repository in the format 'organization/project'"
        , reader (maybeReader parseRepository)
        , option
        ]

commitOption :: Parser Commit
commitOption =
    Commit
        <$> strOption
            [ long "commit"
            , short 'c'
            , metavar "COMMIT"
            , help "The commit hash or reference"
            ]

testDirectoryOption :: Parser Directory
testDirectoryOption =
    Directory
        <$> strOption
            [ long "directory"
            , short 'd'
            , metavar "DIRECTORY"
            , help "The repository directory where test assets are located"
            , option
            ]

downloadAssetsDirectoryOption :: Parser Directory
downloadAssetsDirectoryOption =
    Directory
        <$> strOption
            [ long "download-directory"
            , short 'D'
            , metavar "DIRECTORY"
            , help "The directory where assets will be downloaded to"
            , option
            ]

usernameOption :: Parser Username
usernameOption =
    Username
        <$> strOption
            [ long "username"
            , short 'u'
            , metavar "USERNAME"
            , help "The username to register"
            , option
            ]

pubkeyhashOption :: Parser PublicKeyHash
pubkeyhashOption =
    PublicKeyHash
        <$> strOption
            [ long "pubkeyhash"
            , short 'k'
            , metavar "PUBKEYHASH"
            , help "The public key hash for the user"
            , option
            ]

outputReferenceParser :: Parser RequestRefId
outputReferenceParser =
    setting
        [ long "outref"
        , short 'o'
        , metavar "OUTPUT_REF"
        , help "The transaction hash and index for the output reference"
        , reader parseOutputReference
        , option
        ]

parseOutputReference :: Reader RequestRefId
parseOutputReference = Reader $ \s -> do
    case break (== '-') s of
        (_txHash, '-' : indexStr) -> do
            _index :: Int <- case reads indexStr of
                [(i, "")] -> pure i
                _ ->
                    Left
                        "Invalid index format. Use 'txHash-index' where index is an integer."
            pure
                $ RequestRefId
                $ T.pack s
        _ -> Left "Invalid output reference format. Use 'txHash-index'"

durationOption :: Parser Duration
durationOption =
    Duration
        <$> setting
            [ long "duration"
            , short 't'
            , metavar "DURATION"
            , help "The duration in hours for the test-run"
            , reader auto
            , option
            ]

tryOption :: Parser Try
tryOption =
    Try
        <$> setting
            [ long "try"
            , short 'y'
            , metavar "TRY"
            , help "The current attempt number for this commit"
            , reader auto
            , option
            ]

-- If the token is not passed as the function argument, try to get it from the options
tokenIdOption :: Parser TokenId
tokenIdOption =
    TokenId
        <$> setting
            [ env "ANTI_TOKEN_ID"
            , metavar "TOKEN_ID"
            , help "The token ID of the antithesis token"
            , reader str
            ]

mnemonicsClearTextOption :: Parser Text
mnemonicsClearTextOption =
    setting
        [ help "The mnemonics for the wallet in clear text"
        , conf "mnemonics"
        , metavar "MNEMONICS"
        ]

readyMnemonicsOption :: Parser Mnemonics
readyMnemonicsOption = ClearText <$> mnemonicsClearTextOption

walletFileOption :: Parser FilePath
walletFileOption =
    setting
        [ env "ANTI_WALLET_FILE"
        , metavar "FILEPATH"
        , help "The file path to the wallet secrets"
        , reader str
        ]

mnemonicsObject :: Parser Object
mnemonicsObject = checkMapIO f walletFileOption
  where
    f = fmap Aeson.eitherDecode . BL.readFile

walletOption :: Parser Wallet
walletOption =
    withConfig (Just <$> mnemonicsObject)
        $ checkEither (left show <$> readWallet) readyMnemonicsOption
