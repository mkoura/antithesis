module Oracle.Token.Options
    ( TokenCommand (..)
    , tokenCommandParser
    )
where

import Core.Options (outputReferenceParser)
import Options.Applicative
    ( Alternative (many)
    , Parser
    , command
    , helper
    , hsubparser
    , info
    , progDesc
    , (<**>)
    )
import Oracle.Token.Cli (TokenCommand (..))

tokenCommandParser :: Parser TokenCommand
tokenCommandParser =
    hsubparser
        ( command
            "get"
            ( info
                (pure GetToken <**> helper)
                (progDesc "Get a token")
            )
            <> command
                "update"
                ( info
                    ( UpdateToken
                        <$> many outputReferenceParser
                        <**> helper
                    )
                    (progDesc "Update a token")
                )
        )
