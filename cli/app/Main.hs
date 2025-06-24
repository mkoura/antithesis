module Main (main) where

import Data.Aeson (encode)

import App qualified as Anti
import Data.ByteString.Lazy.Char8 qualified as BL

main :: IO ()
main = do
    (_, e) <- Anti.server
    case e of
        Left err -> BL.putStrLn $ encode $ show err
        Right result -> BL.putStrLn $ encode result
