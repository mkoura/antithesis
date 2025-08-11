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
    , tokenIdOption
    , tryOption
    , usernameOption
    )
import Core.Types.Basic (TokenId)
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
    :: Maybe TokenId
    -> Parser
        (RequesterCommand (AValidationResult RegisterUserFailure TxHash))
addPublicKeyOptions ptk =
    RegisterUser
        <$> tokenIdOption ptk
        <*> ( RegisterUserKey
                <$> platformOption
                <*> usernameOption
                <*> pubkeyhashOption
            )

removePublicKeyOptions
    :: Maybe TokenId
    -> Parser
        (RequesterCommand (AValidationResult UnregisterUserFailure TxHash))
removePublicKeyOptions ptk =
    UnregisterUser
        <$> tokenIdOption ptk
        <*> ( RegisterUserKey
                <$> platformOption
                <*> usernameOption
                <*> pubkeyhashOption
            )

addRoleOptions
    :: Maybe TokenId
    -> Parser
        (RequesterCommand (AValidationResult RegisterRoleFailure TxHash))
addRoleOptions ptk =
    RegisterRole
        <$> tokenIdOption ptk
        <*> ( RegisterRoleKey
                <$> platformOption
                <*> repositoryOption
                <*> usernameOption
            )

removeRoleOptions
    :: Maybe TokenId
    -> Parser
        (RequesterCommand (AValidationResult UnregisterRoleFailure TxHash))
removeRoleOptions ptk =
    UnregisterRole
        <$> tokenIdOption ptk
        <*> ( RegisterRoleKey
                <$> platformOption
                <*> repositoryOption
                <*> usernameOption
            )

requesterCommandParser
    :: Maybe TokenId
    -> Parser (Box RequesterCommand)
requesterCommandParser ptk =
    hsubparser
        ( command
            "create-test"
            ( info
                (Box <$> requestTestOptions ptk)
                (progDesc "Request an antithesis test run")
            )
            <> command
                "register-user"
                ( info
                    (Box <$> addPublicKeyOptions ptk)
                    (progDesc "Register a user public key")
                )
            <> command
                "unregister-user"
                ( info
                    (Box <$> removePublicKeyOptions ptk)
                    (progDesc "Unregister a user public key")
                )
            <> command
                "register-role"
                ( info
                    (Box <$> addRoleOptions ptk)
                    (progDesc "Add a user to a repository")
                )
            <> command
                "unregister-role"
                ( info
                    (Box <$> removeRoleOptions ptk)
                    (progDesc "Remove a user from a repository")
                )
        )

requestTestOptions
    :: Maybe TokenId
    -> Parser
        ( RequesterCommand
            ( AValidationResult
                CreateTestRunFailure
                (WithTxHash (TestRunState PendingT))
            )
        )
requestTestOptions ptk =
    RequestTest
        <$> tokenIdOption ptk
        <*> ( TestRun
                <$> platformOption
                <*> repositoryOption
                <*> directoryOption
                <*> commitOption
                <*> tryOption
                <*> usernameOption
            )
        <*> durationOption
