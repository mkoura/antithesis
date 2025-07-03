{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Redundant <$>" #-}
module Oracle.Options
    ( OracleCommand (..)
    , oracleCommandParser
    ) where

import Lib.Box (Box, fmapBox)
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

oracleCommandParser :: (Box OracleCommand -> b) -> Parser b
oracleCommandParser constructor =
    hsubparser
        ( command
            "token"
            ( info
                ( constructor
                    <$> tokenCommandParser (fmapBox OracleTokenCommand)
                    <**> helper
                )
                (progDesc "Manage tokens")
            )
        )
