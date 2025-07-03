module Oracle.Token.Options
    ( TokenCommand
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
import Oracle.Token.Cli
    ( TokenCommand
    , bootTokenCmd
    , getTokenCmd
    , updateTokenCmd
    )

tokenCommandParser :: Parser TokenCommand
tokenCommandParser =
    hsubparser
        ( command
            "get"
            ( info
                (pure getTokenCmd <**> helper)
                (progDesc "Get a token")
            )
            <> command
                "update"
                ( info
                    ( updateTokenCmd
                        <$> many outputReferenceParser
                    )
                    (progDesc "Update a token")
                )
            <> command
                "boot"
                ( info
                    (pure bootTokenCmd <**> helper)
                    (progDesc "Boot a token")
                )
        )
