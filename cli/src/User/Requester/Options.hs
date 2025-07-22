{-# LANGUAGE DerivingStrategies #-}

module User.Requester.Options
    ( requesterCommandParser
    , addPublicKeyOptions
    , addRoleOptions
    ) where

import Core.Options
    ( commitOption
    , directoryOption
    , durationOption
    , platformOption
    , pubkeyhashOption
    , repositoryOption
    , tryOption
    , usernameOption
    )
import Core.Types.Tx (TxHash, WithTxHash)
import Lib.Box (Box (..))
import Options.Applicative
    ( Parser
    , command
    , hsubparser
    , info
    , progDesc
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
        <$> ( RegisterUserKey
                <$> platformOption
                <*> usernameOption
                <*> pubkeyhashOption
            )

removePublicKeyOptions
    :: Parser
        (RequesterCommand (AValidationResult UnregisterUserFailure TxHash))
removePublicKeyOptions =
    UnregisterUser
        <$> ( RegisterUserKey
                <$> platformOption
                <*> usernameOption
                <*> pubkeyhashOption
            )

addRoleOptions
    :: Parser
        (RequesterCommand (AValidationResult RegisterRoleFailure TxHash))
addRoleOptions =
    RegisterRole
        <$> ( RegisterRoleKey
                <$> platformOption
                <*> repositoryOption
                <*> usernameOption
            )

removeRoleOptions
    :: Parser
        (RequesterCommand (AValidationResult UnregisterRoleFailure TxHash))
removeRoleOptions =
    UnregisterRole
        <$> ( RegisterRoleKey
                <$> platformOption
                <*> repositoryOption
                <*> usernameOption
            )

requesterCommandParser :: Parser (Box RequesterCommand)
requesterCommandParser =
    hsubparser
        ( command
            "create-test"
            ( info
                (Box <$> requestTestOptions)
                (progDesc "Request an antithesis test run")
            )
            <> command
                "register-user"
                ( info
                    (Box <$> addPublicKeyOptions)
                    (progDesc "Register a user public key")
                )
            <> command
                "unregister-user"
                ( info
                    (Box <$> removePublicKeyOptions)
                    (progDesc "Unregister a user public key")
                )
            <> command
                "register-role"
                ( info
                    (Box <$> addRoleOptions)
                    (progDesc "Add a user to a repository")
                )
            <> command
                "unregister-role"
                ( info
                    (Box <$> removeRoleOptions)
                    (progDesc "Remove a user from a repository")
                )
        )

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
        <$> ( TestRun
                <$> platformOption
                <*> repositoryOption
                <*> directoryOption
                <*> commitOption
                <*> tryOption
                <*> usernameOption
            )
        <*> durationOption
