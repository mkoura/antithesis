module Main (main) where

import App qualified as Anti
import Data.ByteString.Lazy.Char8 qualified as BL
import Text.JSON.Canonical (renderCanonicalJSON)

main :: IO ()
main = do
    (_, e) <- Anti.server
    case e of
        Left err -> print err
        Right result -> BL.putStrLn $ renderCanonicalJSON result
