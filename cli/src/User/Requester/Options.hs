{-# LANGUAGE DerivingStrategies #-}

module User.Requester.Options
    ( requesterCommandParser
    , addPublicKeyOptions
    , addRoleOptions
    , sshClientOption
    , sshClientOptionWithoutSelector
    ) where

import Core.Options
    ( commitOption
    , downloadAssetsDirectoryOption
    , durationOption
    , platformOption
    , pubkeyhashOption
    , repositoryOption
    , testDirectoryOption
    , tokenIdOption
    , tryOption
    , usernameOption
    , walletOption
    )
import Core.Types.Tx (TxHash, WithTxHash)
import Lib.Box (Box (..))
import Lib.Options.Secrets (secretsParser)
import Lib.SSH.Private (SSHClient (..), WithSelector (..))
import OptEnvConf
    ( Parser
    , command
    , commands
    , env
    , help
    , metavar
    , reader
    , setting
    , str
    )
import Oracle.Validate.Requests.RegisterRole
    ( RegisterRoleFailure
    , UnregisterRoleFailure
    )
import Oracle.Validate.Requests.RegisterUser
    ( RegisterUserFailure
    , UnregisterUserFailure
    )
import Oracle.Validate.Requests.TestRun.Create (CreateTestRunFailure)
import Oracle.Validate.Types (AValidationResult)
import User.Requester.Cli (NewTestRunCreated, RequesterCommand (..))
import User.Types
    ( RegisterRoleKey (..)
    , RegisterUserKey (..)
    , TestRun (..)
    )

addPublicKeyOptions
    :: Parser
        (RequesterCommand (AValidationResult RegisterUserFailure TxHash))
addPublicKeyOptions =
    RegisterUser
        <$> tokenIdOption
        <*> walletOption
        <*> ( RegisterUserKey
                <$> platformOption
                <*> usernameOption
                <*> pubkeyhashOption
            )

removePublicKeyOptions
    :: Parser
        (RequesterCommand (AValidationResult UnregisterUserFailure TxHash))
removePublicKeyOptions =
    UnregisterUser
        <$> tokenIdOption
        <*> walletOption
        <*> ( RegisterUserKey
                <$> platformOption
                <*> usernameOption
                <*> pubkeyhashOption
            )

addRoleOptions
    :: Parser
        (RequesterCommand (AValidationResult RegisterRoleFailure TxHash))
addRoleOptions =
    RegisterRole
        <$> tokenIdOption
        <*> walletOption
        <*> ( RegisterRoleKey
                <$> platformOption
                <*> repositoryOption
                <*> usernameOption
            )

removeRoleOptions
    :: Parser
        (RequesterCommand (AValidationResult UnregisterRoleFailure TxHash))
removeRoleOptions =
    UnregisterRole
        <$> tokenIdOption
        <*> walletOption
        <*> ( RegisterRoleKey
                <$> platformOption
                <*> repositoryOption
                <*> usernameOption
            )

requesterCommandParser
    :: Parser (Box RequesterCommand)
requesterCommandParser =
    commands
        [ command "create-test" "Request an antithesis test run"
            $ Box <$> requestTestOptions
        , command "register-user" "Register a user public key"
            $ Box <$> addPublicKeyOptions
        , command "unregister-user" "Unregister a user public key"
            $ Box <$> removePublicKeyOptions
        , command "register-role" "Add a user to a repository"
            $ Box <$> addRoleOptions
        , command "unregister-role" "Remove a user from a repository"
            $ Box <$> removeRoleOptions
        , command "generate-assets" "Generate assets for a test run"
            $ Box . GenerateAssets <$> downloadAssetsDirectoryOption
        ]

sshClientOption
    :: Parser (SSHClient 'WithSelector)
sshClientOption =
    SSHClient
        <$> keySelectorOption
        <*> keyFileOption
        <*> keyPasswordOption

sshClientOptionWithoutSelector
    :: Parser (SSHClient 'WithoutSelector)
sshClientOptionWithoutSelector =
    SSHClient ()
        <$> keyFileOption
        <*> keyPasswordOption

keySelectorOption :: Parser String
keySelectorOption =
    setting
        [ env "ANTI_SSH_KEY_SELECTOR"
        , help "Which key selector to use from the SSH file"
        , metavar "STRING"
        , reader str
        ]

keyFileOption :: Parser FilePath
keyFileOption =
    setting
        [ env "ANTI_SSH_FILE"
        , help "Path to the SSH private key file"
        , metavar "FILEPATH"
        , reader str
        ]

keyPasswordOption :: Parser String
keyPasswordOption =
    secretsParser
        "Enter the passphrase for the SSH private key"
        "The passphrase for the SSH private key"
        "ANTI_SSH_PASSWORD"
        "PASSWORD"
        "ask-ssh-passphrase"

requestTestOptions
    :: Parser
        ( RequesterCommand
            ( AValidationResult
                CreateTestRunFailure
                (WithTxHash NewTestRunCreated)
            )
        )
requestTestOptions =
    RequestTest
        <$> tokenIdOption
        <*> walletOption
        <*> sshClientOption
        <*> ( TestRun
                <$> platformOption
                <*> repositoryOption
                <*> testDirectoryOption
                <*> commitOption
                <*> tryOption
                <*> usernameOption
            )
        <*> durationOption
