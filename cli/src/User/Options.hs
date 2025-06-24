module User.Options
    ( UserCommand (..)
    , userCommandParser
    ) where

import Options.Applicative
    ( Parser
    , command
    , helper
    , hsubparser
    , info
    , progDesc
    , (<**>)
    )
import User.Cli (UserCommand (..))
import User.Requester.Options
    ( requesterCommandParser
    , retractRequestOptions
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
