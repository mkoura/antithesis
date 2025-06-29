{-# LANGUAGE DerivingStrategies #-}

module User.Requester.Options
    ( requesterCommandParser
    , addPublicKeyOptions
    , addRoleOptions
    ) where

import Core.Options
    ( commitOption
    , directoryOption
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
import User.Requester.Cli (RequesterCommand (..))
import User.Types
    ( Direction (..)
    , Duration (..)
    , RegisterRoleKey (..)
    , RegisterUserKey (..)
    , TestRun (..)
    )

addPublicKeyOptions :: Parser RequesterCommand
addPublicKeyOptions =
    RegisterUser
        <$> ( RegisterUserKey
                <$> platformOption
                <*> usernameOption
                <*> pubkeyhashOption
                <*> pure Insert
            )

removePublicKeyOptions :: Parser RequesterCommand
removePublicKeyOptions =
    RegisterUser
        <$> ( RegisterUserKey
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

requesterCommandParser :: Parser RequesterCommand
requesterCommandParser =
    hsubparser
        ( command
            "create-test"
            ( info
                requestTestOptions
                (progDesc "Request an antithesis test run")
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
