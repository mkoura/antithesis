{-# LANGUAGE DerivingStrategies #-}

module User.Requester.Options
    ( requesterCommandParser
    , addPublicKeyOptions
    , removePublicKeyOptions
    , addRoleOptions
    , removeRoleOptions
    , retractRequestOptions
    ) where

import Core.Options
    ( commitOption
    , directoryOption
    , outputReferenceParser
    , platformOption
    , pubkeyhashOption
    , repositoryOption
    , roleOption
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
import User.Types (RegisterPublicKey (..))

addPublicKeyOptions :: Parser RequesterCommand
addPublicKeyOptions =
    RegisterUser
        <$> ( RegisterPublicKey
                <$> platformOption
                <*> usernameOption
                <*> pubkeyhashOption
            )

removePublicKeyOptions :: Parser RequesterCommand
removePublicKeyOptions =
    UnregisterUser
        <$> ( RegisterPublicKey
                <$> platformOption
                <*> usernameOption
                <*> pubkeyhashOption
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

requestTestOptions :: Parser RequesterCommand
requestTestOptions =
    RequestTest
        <$> platformOption
        <*> repositoryOption
        <*> commitOption
        <*> directoryOption
        <*> usernameOption
