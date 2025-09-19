{-# LANGUAGE TemplateHaskell #-}

module Lib.GitHubSpec (spec) where

import Control.Monad (void)
import Core.Types.Basic (Commit (..), Repository (..))
import Data.ByteString.Char8 qualified as BC
import GitHub (Auth (OAuth))
import Lib.GitHub (copyGithubDirectory)
import Path
    ( Dir,
      Path,
      parseAbsDir,
      toFilePath,
      (</>),
      mkRelDir,
      mkRelFile,
      parseRelFile,
      File,
      Rel )
import System.Directory (doesFileExist)
import System.Environment (getEnv)
import System.IO.Temp (withSystemTempDirectory)
import Test.Hspec
    ( Spec
    , beforeAll
    , describe
    , it
    , shouldReturn
    )

boot :: IO Auth
boot = OAuth . BC.pack <$> getEnv "ANTI_GITHUB_PAT"

repo :: Repository
repo =
    Repository
        { organization = "cardano-foundation"
        , project = "hal-fixture-sin"
        }

commit :: Maybe Commit
commit = Just $ Commit "a2572c4c9c37c3aa5f21e3ac8ce7ea9b96d833e5"

srcPath :: Path Rel Dir
srcPath = $(mkRelDir "antithesis-test")

readmePath :: Path Rel File
readmePath = $(mkRelFile "README.md")

dockerComposePath :: Path Rel File
dockerComposePath = $(mkRelFile "docker-compose.yaml")

testnetPath :: Path Rel File
testnetPath = $(mkRelFile "testnet.yaml")

spec :: Spec
spec = do
    describe "Path lib" $ do
        it "parses relative directory" $ do
            void $ parseRelFile @IO "antithesis-test/somefile.txt"
    beforeAll boot $ do
        describe "Lib.GitHub" $ do
            it "downloads a directory" $ \pat ->
                withSystemTempDirectory "github-test" $ \targetPath -> do
                    targetDir <- parseAbsDir targetPath
                    copyGithubDirectory pat repo commit srcPath targetDir
                        `shouldReturn` Right ()
                    let readmeAbsPath = targetDir </> srcPath </> readmePath
                    print $ "Checking for file: " ++ toFilePath readmeAbsPath
                    doesFileExist (toFilePath readmeAbsPath) `shouldReturn` True
                    let dockerComposeAbsPath =
                            targetDir
                                </> srcPath
                                </> dockerComposePath
                    doesFileExist (toFilePath dockerComposeAbsPath)
                        `shouldReturn` True
                    let testnetAbsPath = targetDir </> srcPath </> testnetPath
                    doesFileExist (toFilePath testnetAbsPath) `shouldReturn` True
