{-# LANGUAGE TemplateHaskell #-}

module Lib.GitHubSpec (githubSpec) where

import Core.Types.Basic (Commit (..), Repository (..))
import GitHub (Auth)
import Lib.GitHub (copyGithubDirectory)
import Path
    ( Dir
    , File
    , Path
    , Rel
    , mkRelDir
    , mkRelFile
    , parseAbsDir
    , toFilePath
    , (</>)
    )
import System.Directory (doesFileExist)
import System.IO.Temp (withSystemTempDirectory)
import Test.Hspec
    ( SpecWith
    , describe
    , it
    , shouldReturn
    )

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

githubSpec :: SpecWith Auth
githubSpec = describe "Lib.GitHub" $ do
    it "downloads a directory" $ \pat ->
        withSystemTempDirectory "github-test" $ \targetPath -> do
            targetDir <- parseAbsDir targetPath
            copyGithubDirectory pat repo commit srcPath targetDir
                `shouldReturn` Right ()
            let readmeAbsPath = targetDir </> srcPath </> readmePath
            doesFileExist (toFilePath readmeAbsPath) `shouldReturn` True
            let dockerComposeAbsPath =
                    targetDir
                        </> srcPath
                        </> dockerComposePath
            doesFileExist (toFilePath dockerComposeAbsPath)
                `shouldReturn` True
            let testnetAbsPath = targetDir </> srcPath </> testnetPath
            doesFileExist (toFilePath testnetAbsPath) `shouldReturn` True
