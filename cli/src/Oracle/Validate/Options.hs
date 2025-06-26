module Oracle.Validate.Options
    ( ValidateCommand (..)
    , validateCommandParser
    )
where

import Options.Applicative
    ( Parser
    , command
    , helper
    , hsubparser
    , info
    , progDesc
    , (<**>)
    )
import Oracle.Validate.Cli (ValidateCommand (..))

validateCommandParser :: Parser ValidateCommand
validateCommandParser =
    hsubparser
        ( command
            "validate requests"
            ( info
                (pure ValidateRequests <**> helper)
                (progDesc "Validate all requests")
            )
        )
