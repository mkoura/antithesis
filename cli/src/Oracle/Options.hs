module Oracle.Options
    ( OracleCommand (..)
    , oracleCommandParser
    ) where

import Core.Options (tokenIdOption)
import Core.Types.Basic (TokenId)
import Lib.Box (Box (..), fmapBox)
import Options.Applicative
    ( Parser
    , command
    , hsubparser
    , info
    , progDesc
    )
import Oracle.Cli (OracleCommand (..))
import Oracle.Config.Options (configCommandParser)
import Oracle.Token.Options (tokenCommandParser)
import Oracle.Validate.Options (validateCommandParser)

oracleCommandParser
    :: Maybe TokenId
    -> Parser (Box OracleCommand)
oracleCommandParser ptk =
    hsubparser
        ( command
            "token"
            ( info
                ( fmapBox OracleTokenCommand
                    <$> tokenCommandParser ptk
                )
                (progDesc "Manage tokens")
            )
            <> command
                "requests"
                ( info
                    ( fmap Box . OracleValidateCommand
                        <$> tokenIdOption ptk
                        <*> validateCommandParser
                    )
                    (progDesc "Manage requests")
                )
            <> command
                "config"
                ( info
                    ( fmapBox OracleSetConfigCommand
                        <$> configCommandParser ptk
                    )
                    (progDesc "Manage configuration")
                )
        )
