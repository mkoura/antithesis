{-# LANGUAGE DerivingStrategies #-}

module Core.Options
    ( platformOption
    , repositoryOption
    , commitOption
    , directoryOption
    , usernameOption
    , pubkeyhashOption
    , outputReferenceParser
    , durationOption
    , tryOption
    , tokenIdOption
    )
where

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
import Data.Text qualified as T
import OptEnvConf
    ( Parser
    , auto
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
    , value
    )
import OptEnvConf.Reader (Reader (..))

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

directoryOption :: Parser Directory
directoryOption =
    Directory
        <$> strOption
            [ long "directory"
            , short 'd'
            , metavar "DIRECTORY"
            , value "."
            , help "The directory to run in (defaults to \".\")"
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
