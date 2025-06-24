{-# LANGUAGE DerivingStrategies #-}

module Options (parseArgs, Options (..)) where

import Cli (Command (..))
import Data.Text qualified as T
import Options.Applicative
    ( Alternative (..)
    , Parser
    , ReadM
    , command
    , defaultPrefs
    , execParserPure
    , fullDesc
    , handleParseResult
    , header
    , help
    , helper
    , hsubparser
    , info
    , long
    , maybeReader
    , metavar
    , option
    , progDesc
    , short
    , strOption
    , value
    , (<**>)
    )
import Options.Applicative.Types (readerAsk)
import Oracle.Cli (OracleCommand (..))
import Oracle.Token.Cli (TokenCommand (..))
import Types
    ( Directory (..)
    , Platform (..)
    , PublicKeyHash (..)
    , Repository (..)
    , RequestRefId (..)
    , Role (..)
    , SHA1 (..)
    , Username (..)
    )
import User.Cli (UserCommand (..))
import User.Requester.Cli (RequesterCommand (..))

newtype Options = Options
    { optionsCommand :: Command
    }
    deriving (Eq, Show)

platformOption :: Parser Platform
platformOption =
    Platform
        <$> strOption
            ( long "platform"
                <> short 'p'
                <> metavar "PLATFORM"
                <> help "The platform to use"
            )

parseRepository :: String -> Maybe Repository
parseRepository repoStr = case break (== '/') repoStr of
    (org, '/' : proj) -> Just $ Repository org proj
    _ -> Nothing

repositoryOption :: Parser Repository
repositoryOption =
    option
        (maybeReader parseRepository)
        ( long "repository"
            <> short 'r'
            <> metavar "ORGANIZATION/PROJECT"
            <> help "The repository in the format 'organization/project'"
        )

commitOption :: Parser SHA1
commitOption =
    SHA1
        <$> strOption
            ( long "commit"
                <> short 'c'
                <> metavar "COMMIT"
                <> help "The commit hash or reference"
            )

directoryOption :: Parser Directory
directoryOption =
    Directory
        <$> strOption
            ( long "directory"
                <> short 'd'
                <> metavar "DIRECTORY"
                <> value "."
                <> help "The directory to run in (defaults to \".\")"
            )

requestTestOptions :: Parser RequesterCommand
requestTestOptions =
    RequestTest
        <$> platformOption
        <*> repositoryOption
        <*> commitOption
        <*> directoryOption
        <*> usernameOption

usernameOption :: Parser Username
usernameOption =
    Username
        <$> strOption
            ( long "username"
                <> short 'u'
                <> metavar "USERNAME"
                <> help "The username to register"
            )

pubkeyhashOption :: Parser PublicKeyHash
pubkeyhashOption =
    PublicKeyHash
        <$> strOption
            ( long "pubkeyhash"
                <> short 'k'
                <> metavar "PUBKEYHASH"
                <> help "The public key hash for the user"
            )

addPublicKeyOptions :: Parser RequesterCommand
addPublicKeyOptions =
    RegisterPublicKey
        <$> platformOption
        <*> usernameOption
        <*> pubkeyhashOption

removePublicKeyOptions :: Parser RequesterCommand
removePublicKeyOptions =
    UnregisterPublicKey
        <$> platformOption
        <*> usernameOption
        <*> pubkeyhashOption

roleOption :: Parser Role
roleOption =
    Role
        <$> strOption
            ( long "role"
                <> short 'r'
                <> metavar "ROLE"
                <> help "The role to assign to the user (e.g., maintainer, contributor)"
            )

addRoleOptions :: Parser RequesterCommand
addRoleOptions =
    RegisterRole
        <$> platformOption
        <*> repositoryOption
        <*> roleOption
        <*> usernameOption

removeRoleOptions :: Parser RequesterCommand
removeRoleOptions =
    UnregisterRole
        <$> platformOption
        <*> repositoryOption
        <*> roleOption
        <*> usernameOption

retractRequestOptions :: Parser UserCommand
retractRequestOptions =
    RetractRequest
        <$> outputReferenceParser

requesterCommandParser :: Parser RequesterCommand
requesterCommandParser =
    hsubparser
        ( command
            "test"
            ( info
                requestTestOptions
                (progDesc "Request a test on a specific platform")
            )
            <> command
                "register-public-key"
                ( info
                    addPublicKeyOptions
                    (progDesc "Register a user public key")
                )
            <> command
                "unregister-public-key"
                ( info
                    removePublicKeyOptions
                    (progDesc "Unregister a user public key")
                )
            <> command
                "register-role"
                ( info
                    addRoleOptions
                    (progDesc "Add a user to a repository")
                )
            <> command
                "unregister-role"
                ( info
                    removeRoleOptions
                    (progDesc "Remove a user from a repository")
                )
        )

outputReferenceParser :: Parser RequestRefId
outputReferenceParser =
    option parseOutputReference
        $ short 'o'
            <> long "outref"
            <> metavar "OUTPUT_REF"
            <> help "The transaction hash and index for the output reference"

parseOutputReference :: ReadM RequestRefId
parseOutputReference = do
    s <- readerAsk
    case break (== '-') s of
        (_txHash, '-' : indexStr) -> do
            _index :: Int <- case reads indexStr of
                [(i, "")] -> pure i
                _ ->
                    fail
                        "Invalid index format. Use 'txHash-index' where index is an integer."
            pure
                $ RequestRefId
                $ T.pack s
        _ -> fail "Invalid output reference format. Use 'txHash-index'"

tokenCommandParser :: Parser TokenCommand
tokenCommandParser =
    hsubparser
        ( command
            "get"
            ( info
                (pure GetToken <**> helper)
                (progDesc "Get a token")
            )
            <> command
                "update"
                ( info
                    ( UpdateToken
                        <$> many outputReferenceParser
                        <**> helper
                    )
                    (progDesc "Update a token")
                )
        )

commandParser :: Parser Command
commandParser =
    hsubparser
        ( command
            "oracle"
            ( info
                (OracleCommand <$> oracleCommandParser <**> helper)
                (progDesc "Oracle services")
            )
            <> command
                "user"
                ( info
                    (UserCommand <$> userCommandParser <**> helper)
                    (progDesc "Manage user requests")
                )
        )

oracleCommandParser :: Parser OracleCommand
oracleCommandParser =
    hsubparser
        ( command
            "token"
            ( info
                (OracleTokenCommand <$> tokenCommandParser <**> helper)
                (progDesc "Manage tokens")
            )
        )

userCommandParser :: Parser UserCommand
userCommandParser =
    hsubparser
        ( command
            "request"
            ( info
                (UserRequesterCommand <$> requesterCommandParser <**> helper)
                (progDesc "Allow users to send requests")
            )
            <> command
                "retract"
                ( info
                    retractRequestOptions
                    (progDesc "Retract a request")
                )
            <> command
                "get-facts"
                ( info
                    (pure GetFacts)
                    (progDesc "Get token facts")
                )
        )

optionsParser :: Parser Options
optionsParser = Options <$> commandParser

parseArgs :: [String] -> IO Options
parseArgs args = handleParseResult $ execParserPure defaultPrefs opts args
  where
    opts =
        info
            (optionsParser <**> helper)
            ( fullDesc
                <> progDesc "Antithesis CLI"
                <> header "anti - A tool for managing Antithesis test runs"
            )
