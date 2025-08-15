module Oracle.Options
    ( OracleCommand (..)
    , oracleCommandParser
    ) where

import Core.Options (tokenIdOption)
import Lib.Box (Box (..), fmapBox)
import OptEnvConf
    ( Parser
    , command
    , commands
    )
import Oracle.Cli (OracleCommand (..))
import Oracle.Config.Options (configCommandParser)
import Oracle.Token.Options (tokenCommandParser)
import Oracle.Validate.Options (validateCommandParser)

oracleCommandParser
    :: Parser (Box OracleCommand)
oracleCommandParser =
    commands
        [ command "token" "Manage tokens"
            $ fmapBox OracleTokenCommand <$> tokenCommandParser
        , command "requests" "Manage requests"
            $ fmap Box . OracleValidateCommand
                <$> tokenIdOption
                <*> validateCommandParser
        , command "config" "Manage configuration"
            $ fmapBox OracleSetConfigCommand <$> configCommandParser
        ]
