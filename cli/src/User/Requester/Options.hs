{-# LANGUAGE DerivingStrategies #-}

module User.Requester.Options
    ( requesterCommandParser
    , addPublicKeyOptions
    , addRoleOptions
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
import Lib.SSH.Private (SSHClient (..))
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
import User.Requester.Cli (RequesterCommand (..))
import User.Types
    ( Phase (PendingT)
    , RegisterRoleKey (..)
    , RegisterUserKey (..)
    , TestRun (..)
    , TestRunState
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
        , command "generate-asssets" "Generate assets for a test run"
            $ Box . GenerateAssets <$> downloadAssetsDirectoryOption
        ]

sshClientOption
    :: Parser SSHClient
sshClientOption =
    SSHClient
        <$> keySelectorOption
        <*> keyFileOption
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
    setting
        [ env "ANTI_SSH_PASSWORD"
        , help "Password to the decrypt the SSH private key"
        , metavar "STRING"
        , reader str
        ]

requestTestOptions
    :: Parser
        ( RequesterCommand
            ( AValidationResult
                CreateTestRunFailure
                (WithTxHash (TestRunState PendingT))
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
