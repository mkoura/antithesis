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
import Core.Types (TxHash, WithTxHash)
import Options.Applicative
    ( Parser
    , command
    , hsubparser
    , info
    , progDesc
    )
import Oracle.Token.Options (Box (..))
import User.Requester.Cli (RequesterCommand (..))
import User.Types
    ( Duration (..)
    , Phase (PendingT)
    , RegisterRoleKey (..)
    , RegisterUserKey (..)
    , TestRun (..)
    , TestRunState
    )

addPublicKeyOptions :: Parser (RequesterCommand TxHash)
addPublicKeyOptions =
    RegisterUser
        <$> ( RegisterUserKey
                <$> platformOption
                <*> usernameOption
                <*> pubkeyhashOption
            )

removePublicKeyOptions :: Parser (RequesterCommand TxHash)
removePublicKeyOptions =
    UnregisterUser
        <$> ( RegisterUserKey
                <$> platformOption
                <*> usernameOption
                <*> pubkeyhashOption
            )

addRoleOptions :: Parser (RequesterCommand TxHash)
addRoleOptions =
    RegisterRole
        <$> ( RegisterRoleKey
                <$> platformOption
                <*> repositoryOption
                <*> usernameOption
            )

removeRoleOptions :: Parser (RequesterCommand TxHash)
removeRoleOptions =
    UnregisterRole
        <$> ( RegisterRoleKey
                <$> platformOption
                <*> repositoryOption
                <*> usernameOption
            )

requesterCommandParser :: (Box RequesterCommand -> b) -> Parser b
requesterCommandParser constructor =
    hsubparser
        ( command
            "create-test"
            ( info
                (constructor . Box <$> requestTestOptions)
                (progDesc "Request an antithesis test run")
            )
            <> command
                "register-public-key"
                ( info
                    (constructor . Box <$> addPublicKeyOptions)
                    (progDesc "Register a user public key")
                )
            <> command
                "unregister-public-key"
                ( info
                    (constructor . Box <$> removePublicKeyOptions)
                    (progDesc "Unregister a user public key")
                )
            <> command
                "register-role"
                ( info
                    (constructor . Box <$> addRoleOptions)
                    (progDesc "Add a user to a repository")
                )
            <> command
                "unregister-role"
                ( info
                    (constructor . Box <$> removeRoleOptions)
                    (progDesc "Remove a user from a repository")
                )
        )

requestTestOptions
    :: Parser (RequesterCommand (WithTxHash (TestRunState PendingT)))
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
