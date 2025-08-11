{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Oracle.Token.Options
    ( tokenCommandParser
    , Box (..)
    )
where

import Core.Options (outputReferenceParser, tokenIdOption)
import Core.Types.Basic (TokenId)
import Lib.Box (Box (..))
import Options.Applicative
    ( Alternative (many)
    , Parser
    , command
    , hsubparser
    , info
    , progDesc
    )
import Oracle.Token.Cli
    ( TokenCommand (..)
    )

tokenCommandParser
    :: Maybe TokenId
    -> Parser (Box TokenCommand)
tokenCommandParser ptk =
    hsubparser
        ( command
            "get"
            ( info
                (Box . GetToken <$> tokenIdOption ptk)
                (progDesc "Get a token")
            )
            <> command
                "update"
                ( info
                    ( fmap Box . UpdateToken
                        <$> tokenIdOption ptk
                        <*> many outputReferenceParser
                    )
                    (progDesc "Update a token")
                )
            <> command
                "boot"
                ( info
                    (pure (Box BootToken))
                    (progDesc "Boot a token")
                )
            <> command
                "end"
                ( info
                    (Box . EndToken <$> tokenIdOption ptk)
                    (progDesc "End a token")
                )
        )
