{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Oracle.Token.Options
    ( tokenCommandParser
    , Box (..)
    )
where

import Core.Options (outputReferenceParser)
import Lib.Box (Box (..))
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
    ( TokenCommand (..)
    )

tokenCommandParser :: (Box TokenCommand -> b) -> Parser b
tokenCommandParser constructor =
    hsubparser
        ( command
            "get"
            ( info
                (pure (constructor $ Box GetToken) <**> helper)
                (progDesc "Get a token")
            )
            <> command
                "update"
                ( info
                    ( constructor . Box . UpdateToken
                        <$> many outputReferenceParser
                    )
                    (progDesc "Update a token")
                )
            <> command
                "boot"
                ( info
                    (pure (constructor $ Box BootToken) <**> helper)
                    (progDesc "Boot a token")
                )
        )
