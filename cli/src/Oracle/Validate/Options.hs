module Oracle.Validate.Options
    ( ValidateCommand (..)
    , validateCommandParser
    )
where

import OptEnvConf
    ( Parser
    , command
    , commands
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
    commands
        [ command "validate" "Validate all requests"
            $ pure ValidateRequests
        ]
