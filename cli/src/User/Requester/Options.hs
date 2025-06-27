{-# LANGUAGE DerivingStrategies #-}

module User.Requester.Options
    ( requesterCommandParser
    , addPublicKeyOptions
    , addRoleOptions
    , retractRequestOptions
    ) where

import Core.Options
    ( commitOption
    , directoryOption
    , outputReferenceParser
    , platformOption
    , pubkeyhashOption
    , repositoryOption
    , usernameOption
    )
import Options.Applicative
    ( Parser
    , command
    , hsubparser
    , info
    , progDesc
    )
import User.Cli (UserCommand (..))
import User.Requester.Cli (RequesterCommand (..))
import User.Types
    ( Direction (..)
    , Duration (..)
    , RegisterPublicKey (..)
    , RegisterRoleKey (..)
    , TestRun (..)
    )

addPublicKeyOptions :: Parser RequesterCommand
addPublicKeyOptions =
    RegisterUser
        <$> ( RegisterPublicKey
                <$> platformOption
                <*> usernameOption
                <*> pubkeyhashOption
                <*> pure Insert
            )

removePublicKeyOptions :: Parser RequesterCommand
removePublicKeyOptions =
    RegisterUser
        <$> ( RegisterPublicKey
                <$> platformOption
                <*> usernameOption
                <*> pubkeyhashOption
                <*> pure Delete
            )

addRoleOptions :: Parser RequesterCommand
addRoleOptions =
    RegisterRole
        <$> ( RegisterRoleKey
                <$> platformOption
                <*> repositoryOption
                <*> usernameOption
                <*> pure Insert
            )

removeRoleOptions :: Parser RequesterCommand
removeRoleOptions =
    RegisterRole
        <$> ( RegisterRoleKey
                <$> platformOption
                <*> repositoryOption
                <*> usernameOption
                <*> pure Delete
            )

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

requestTestOptions :: Parser RequesterCommand
requestTestOptions =
    RequestTest
        <$> ( TestRun
                <$> platformOption
                <*> repositoryOption
                <*> directoryOption
                <*> commitOption
                <*> pure 1
                <*> usernameOption
            )
        <*> pure (Duration 3)
