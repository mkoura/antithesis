module Oracle.Options
    ( OracleCommand (..)
    , oracleCommandParser
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
import Oracle.Cli (OracleCommand (..))
import Oracle.Token.Options (tokenCommandParser)

oracleCommandParser :: Parser OracleCommand
oracleCommandParser =
    hsubparser
        ( command
            "token"
            ( info
                (OracleTokenCommand <$> tokenCommandParser <**> helper)
                (progDesc "Manage tokens")
            )
        )
