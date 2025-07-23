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
import Oracle.Validate.Cli
    ( OracleValidateFailure
    , RequestValidation
    , ValidateCommand (..)
    )
import Oracle.Validate.Types (AValidationResult)

validateCommandParser
    :: Parser
        ( ValidateCommand
            (AValidationResult OracleValidateFailure [RequestValidation])
        )
validateCommandParser =
    hsubparser
        ( command
            "validate"
            ( info
                (pure ValidateRequests <**> helper)
                (progDesc "Validate all requests")
            )
        )
