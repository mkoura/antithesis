{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Boilerplate to parse (some) node logs
module Cardano.Antithesis.LogMessage
  ( LogMessage (..)
  , LogMessageData (..)
  , Severity (..)
  ) where

import qualified Data.Aeson.KeyMap as KeyMap

import Data.Aeson
    ( FromJSON (..)
    , Value (..)
    , withObject
    , withText
    , (.:)
    , (.:?)
    )
import Data.Text
    ( Text
    )
import Data.Time
    ( UTCTime
    )
import GHC.Generics
    ( Generic
    )

type Node = Text

-- details ---------------------------------------------------------------------

-- | The inner payload of a log message, dispatched on the
--   @"kind"@ field in the JSON.
data LogMessageData
  = AddedToCurrentChain
      { newTipSelectView :: NewTipSelectView
      , newtip           :: Text
      }
  | OtherLogMessageData
      { originalObject :: Value
      }
  deriving (Show, Generic)

-- | Details of the new tip selection view.
data NewTipSelectView = NewTipSelectView
  { chainLength :: Int
  , issueNo     :: Int
  , issuerHash  :: Text
  , kind        :: Text
  , slotNo      :: Int
  , tieBreakVRF :: Text
  }
  deriving (Show, Generic, FromJSON)

instance FromJSON LogMessageData where
  parseJSON = withObject "LogMessageData" $ \o -> do
    (tag :: Maybe Text) <- o .:? "kind"
    case tag of
      Just "AddedToCurrentChain" ->
        AddedToCurrentChain
          <$> o .: "newTipSelectView"
          <*> o .: "newtip"
      -- Fallback: capture the raw object for unknown tags
      _ ->
        pure $ OtherLogMessageData $ Object o

-- LogMessage ------------------------------------------------------------------

data LogMessage = LogMessage
  { at      :: UTCTime
  , ns      :: Text
  , details :: LogMessageData -- renamed from 'data'
  , sev     :: Severity
  , thread  :: Text
  , host    :: Node
  , kind    :: Text
  , json    :: Value
  } deriving (Show, Generic)

instance FromJSON LogMessage where
  parseJSON = withObject "LogMessage" $ \o -> do
    at      <- o .:  "at"
    ns      <- o .:  "ns"
    details <- o .:  "data"
    sev     <- o .:  "sev"
    thread  <- o .:  "thread"
    host    <- o .:  "host"


    detailsJson <- o .:  "data"
    kind <- case detailsJson of
      Object hm -> case KeyMap.lookup "kind" hm of
                     Just v  -> parseJSON v
                     Nothing -> pure "" -- for simplicity
      _ -> fail "\"data\" was not a JSON object"

    return LogMessage
      { at      = at
      , ns      = ns
      , details = details
      , sev     = sev
      , thread  = thread
      , host    = host
      , kind    = kind
      , json    = Object o
      }

-- | Severity levels in your logs
data Severity = Debug | Info | Notice | Warning | SevError | Critical
  deriving (Show, Generic, Eq, Ord)

instance FromJSON Severity where
  parseJSON = withText "Severity" $ \t -> case t of
    "Debug"    -> pure Debug
    "Info"     -> pure Info
    "Notice"   -> pure Notice
    "Warning"  -> pure Warning
    "Error"    -> pure SevError
    "Critical" -> pure Critical
    _          -> fail $ "Unknown severity: " <> show t
