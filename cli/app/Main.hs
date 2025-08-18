module Main (main) where

import App (Result (..))
import App qualified as Anti
import Data.ByteString.Lazy.Char8 qualified as BL
import Lib.JSON.Canonical.Extra (object, (.=))
import Text.JSON.Canonical (renderCanonicalJSON)

main :: IO ()
main = do
    clientResult <- Anti.client
    case clientResult of
        Success _ mpsHost e -> do
            output <- do
                let fs =
                        [ "mpfsHost" .= mpsHost
                        ]
                            <> case e of
                                Left err -> ["error" .= show err]
                                Right result -> ["result" .= result]
                object fs
            BL.putStrLn $ renderCanonicalJSON output
        Failure ex -> do
            output <- object ["error" .= show ex]
            BL.putStrLn $ renderCanonicalJSON output
        Help -> pure ()
