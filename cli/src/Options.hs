{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}

module Options (parseArgs) where

import Options.Applicative
    ( Alternative (..)
    , Parser
    , ReadM
    , auto
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
import Types
    ( Command (..)
    , Directory (..)
    , Host (..)
    , Options (Options)
    , OracleCommand (..)
    , OutputReference (..)
    , Platform (..)
    , Port (..)
    , PublicKeyHash (..)
    , Repository (Repository)
    , RequesterCommand (..)
    , Role (..)
    , SHA1 (..)
    , TokenCommand (..)
    , TokenId (..)
    , UserCommand (..)
    , Username (..)
    )

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
        <*> tokenIdOption

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
        <*> tokenIdOption

removePublicKeyOptions :: Parser RequesterCommand
removePublicKeyOptions =
    UnregisterPublicKey
        <$> platformOption
        <*> usernameOption
        <*> pubkeyhashOption
        <*> tokenIdOption

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
        <*> tokenIdOption

removeRoleOptions :: Parser RequesterCommand
removeRoleOptions =
    UnregisterRole
        <$> platformOption
        <*> repositoryOption
        <*> roleOption
        <*> usernameOption
        <*> tokenIdOption

tokenIdOption :: Parser TokenId
tokenIdOption =
    TokenId
        <$> strOption
            ( long "token-id"
                <> short 't'
                <> metavar "TOKEN_ID"
                <> help "The token ID"
            )

retractRequestOptions :: Parser RequesterCommand
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
                "retract"
                ( info
                    retractRequestOptions
                    (progDesc "Retract a request")
                )
            <> command
                "get-facts"
                ( info
                    (GetFacts <$> tokenIdOption)
                    (progDesc "Get token facts")
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

outputReferenceParser :: Parser OutputReference
outputReferenceParser =
    option parseOutputReference
        $ short 'o'
            <> long "outref"
            <> metavar "OUTPUT_REF"
            <> help "The transaction hash and index for the output reference"

parseOutputReference :: ReadM OutputReference
parseOutputReference = do
    s <- readerAsk
    case break (== '-') s of
        (txHash, '-' : indexStr) -> do
            index <- case reads indexStr of
                [(i, "")] -> pure i
                _ ->
                    fail
                        "Invalid index format. Use 'txHash-index' where index is an integer."
            pure
                $ OutputReference
                    { outputReferenceTx = txHash
                    , outputReferenceIndex = index
                    }
        _ -> fail "Invalid output reference format. Use 'txHash-index'"

tokenCommandParser :: Parser TokenCommand
tokenCommandParser =
    hsubparser
        ( command
            "create"
            ( info
                (pure CreateToken <**> helper)
                (progDesc "Create a new token")
            )
            <> command
                "delete"
                ( info
                    (deleteTokenOptions <**> helper)
                    (progDesc "Delete a token")
                )
            <> command
                "get"
                ( info
                    (GetToken <$> tokenIdOption <**> helper)
                    (progDesc "Get a token")
                )
            <> command
                "update"
                ( info
                    ( UpdateToken
                        <$> tokenIdOption
                        <*> many outputReferenceParser
                        <**> helper
                    )
                    (progDesc "Update a token")
                )
        )

deleteTokenOptions :: Parser TokenCommand
deleteTokenOptions =
    DeleteToken
        <$> tokenIdOption

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
        )

hostOption :: Parser Host
hostOption =
    Host
        <$> strOption
            ( long "host"
                <> short 'h'
                <> metavar "HOST"
                <> value "localhost"
                <> help "The host to connect to (defaults to \"localhost\")"
            )

portOption :: Parser Port
portOption =
    Port
        <$> option
            auto
            ( long "port"
                <> short 'p'
                <> metavar "PORT"
                <> value 3000
                <> help "The port to connect to (defaults to 8080)"
            )

optionsParser :: Parser Options
optionsParser =
    Options
        <$> hostOption
        <*> portOption
        <*> commandParser

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
