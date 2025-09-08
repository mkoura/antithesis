module Lib.Options.Secrets
    ( secretsParser
    ) where

import Data.Functor (($>))
import OptEnvConf
    ( Alternative ((<|>))
    , Parser
    , env
    , help
    , long
    , mapIO
    , metavar
    , reader
    , setting
    , str
    , switch
    )
import System.Console.Haskeline
    ( defaultSettings
    , getPassword
    , runInputT
    )

-- | A parser for secrets that can be provided via environment variables,
-- command line flags, or interactively via console prompt.
secretsParser
    :: String
    -- ^ Prompt
    -> String
    -- ^ Help string
    -> String
    -- ^ Environment variable name
    -> String
    -- ^ Metavar string
    -> String
    -- ^ Long option name
    -> Parser String
secretsParser prompt helpString envString metaString longString =
    mapIO id
        $ setting
            [ help helpString
            , env "ANTI_INTERACTIVE_SECRETS"
            , metavar "NONE"
            , reader (str @String $> queryConsole prompt)
            ]
        <|> setting
            [ help helpString
            , metavar "NONE"
            , long longString
            , switch $ queryConsole prompt
            ]
        <|> setting
            [ env envString
            , metavar metaString
            , help prompt
            , reader $ fmap pure str
            ]

queryConsole :: String -> IO String
queryConsole prompt = runInputT defaultSettings $ do
    pw <- getPassword (Just '*') (prompt <> ": ")
    case pw of
        Nothing -> pure ""
        Just pw' -> pure pw'
