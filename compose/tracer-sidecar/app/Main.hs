{-# LANGUAGE OverloadedStrings #-}


{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NamedFieldPuns #-}

module Main where

import LogMessage
import Antithesis

import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Char8 as B8

import System.Directory
    ( listDirectory, doesDirectoryExist, doesFileExist, listDirectory )
import System.FilePath
    ( (</>), takeExtension, (</>), takeExtension )
import System.IO (withFile, BufferMode (NoBuffering), hSetBuffering, hIsEOF, IOMode(ReadMode), hSetBuffering, BufferMode(LineBuffering), stdout)
import Control.Monad (forM, forM_, filterM, unless, forever)
import System.Environment (getArgs)
import Control.Concurrent (forkIO, threadDelay, modifyMVar_, newMVar)
import Data.Aeson (eitherDecode)

-- main ------------------------------------------------------------------------

-- | Main: <program> <directory>
--  Processes existing .jsonl files and tails them for new entries.
main :: IO ()
main = do
  hSetBuffering stdout LineBuffering
  putStrLn "starting tracer-sidecar..."
  args <- getArgs
  dir <- case args of
           [d] -> return d
           _   -> error "Usage: <executable name> <directory>"

  mvar <- newMVar =<< initialState

  threadDelay 2000000 -- allow log files to be created
  files <- jsonFiles dir

  putStrLn $ "Observing .jsonl files: " <> show files

  forM_ files $ \file ->
    forkIO $ tailJsonLines file (modifyMVar_ mvar . processMessage)
  forever $ threadDelay maxBound

-- State -----------------------------------------------------------------------

newtype State = State
  { hasSeenAFork :: Bool -- whether or not any node has seen a fork
  }

initialState :: IO State
initialState = do
  writeSdkJsonl sometimesForksDeclaration
  return $ State False

processMessage :: LogMessage -> State -> IO State
processMessage LogMessage{datum} state = case (datum, state) of
  (SwitchedToAFork{}, s@(State False)) -> do
    writeSdkJsonl sometimesForksReached
    return $ s { hasSeenAFork = True }
  (_, s) -> return s

-- utils -----------------------------------------------------------------------

-- | Recursively find .json files in a directory
jsonFiles :: FilePath -> IO [FilePath]
jsonFiles dir = do
  entries <- listDirectory dir
  let paths = map (dir </>) entries

  files <- filterM doesFileExist paths
  dirs  <- filterM doesDirectoryExist paths

  let jsonHere = filter ((== ".json") . takeExtension) files
  jsonInSubDirs <- concat <$> forM dirs jsonFiles

  pure (jsonHere ++ jsonInSubDirs)

tailJsonLines :: FilePath -> (LogMessage -> IO ()) -> IO ()
tailJsonLines path action = tailLines path $ \bs ->
  case eitherDecode $ BL.fromStrict bs of
      Right msg -> action msg
      Left _e -> pure () -- putStrLn $ "warning: unrecognized line: " <> B8.unpack bs <> " " <> show e

tailLines :: FilePath -> (B8.ByteString -> IO ()) -> IO ()
tailLines path callback = withFile path ReadMode $ \h -> do
    -- read up to current EOF without closing the handle
    let drain = do
          eof <- hIsEOF h
          unless eof $ B8.hGetLine h >>= callback >> drain
    drain

    -- switch to unbuffered mode and follow new data
    hSetBuffering h NoBuffering
    forever $ do
      eof <- hIsEOF h
      if eof
         then threadDelay 100000
         else B8.hGetLine h >>= callback

