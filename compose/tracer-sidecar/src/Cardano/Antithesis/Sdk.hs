{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
module Cardano.Antithesis.Sdk where

import qualified Data.ByteString.Lazy as BL

import Data.Aeson
    ( Value
    , encode
    )
import Data.Aeson.QQ
    ( aesonQQ
    )
import Data.Maybe
    ( fromMaybe
    )
import Data.Text
    ( Text
    )
import System.Environment
    ( lookupEnv
    )
import System.IO
    ( IOMode (AppendMode)
    , withFile
    )


-- | Append a JSON Value as one line to $ANTITHESIS_OUTPUT_DIR/sdk.jsonl
writeSdkJsonl :: Value -> IO ()
writeSdkJsonl v = do
  dir <- fromMaybe "/tmp"<$> lookupEnv "ANTITHESIS_OUTPUT_DIR"
  let outFile = dir ++ "/sdk.jsonl"
  -- open in AppendMode and write the JSON + newline
  withFile outFile AppendMode $ \h ->
    BL.hPutStr h (encode v <> "\n")


-- Hard code values for now

sometimesTracesDeclaration :: Text -> Value
sometimesTracesDeclaration label = [aesonQQ|
    {
      "antithesis_assert": {
        "id":           #{label},
        "message":      #{label},
        "condition":    false,
        "display_type": "Sometimes",
        "hit":          false,
        "must_hit":     true,
        "assert_type":  "sometimes",
        "location": #{dummyLocation},
        "details": null
      }
    }
    |]

sometimesTracesReached :: Text -> Value
sometimesTracesReached label = [aesonQQ|
    {
      "antithesis_assert": {
        "id":           #{label},
        "message":      #{label},
        "condition":    true,
        "display_type": "AlwaysOrUnreachable",
        "hit":          true,
        "must_hit":     true,
        "assert_type":  "always",
        "location": #{dummyLocation},
        "details": null
      }
    }
    |]

dummyLocation :: Value
dummyLocation = [aesonQQ|
    {
          "file":         "",
          "function":     "",
          "class":        "",
          "begin_line":   0,
          "begin_column": 0
    } |]
