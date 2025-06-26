{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneDeriving #-}

module User.Types
    ( TestRun (..)
    , TestRunState (..)
    , Duration (..)
    , Reason (..)
    , Phase (..)
    , URL (..)
    )
where

import Core.Types
    ( Directory (..)
    , Platform (..)
    , Repository (..)
    , SHA1 (..)
    , Username (..)
    )
import Lib.JSON
    ( getField
    , getIntegralField
    , getListField
    , getStringField
    , getStringMapField
    , intJSON
    , object
    , stringJSON
    )
import Text.JSON.Canonical
    ( FromJSON (..)
    , JSValue (..)
    , ReportSchemaErrors
    , ToJSON (..)
    , expectedButGotValue
    , fromJSString
    )

data TestRun = TestRun
    { platform :: Platform
    , repository :: Repository
    , directory :: Directory
    , commitId :: SHA1
    , testRunIndex :: Int
    , requester :: Username
    }
    deriving (Eq, Show)

instance Monad m => ToJSON m TestRun where
    toJSON
        ( TestRun
                (Platform platform)
                (Repository owner repo)
                (Directory directory)
                (SHA1 commitId)
                testRunIndex
                (Username requester)
            ) =
            object
                [ ("platform", stringJSON platform)
                ,
                    ( "repository"
                    , object
                        [ ("owner", stringJSON owner)
                        , ("repo", stringJSON repo)
                        ]
                    )
                , ("directory", stringJSON directory)
                , ("commitId", stringJSON commitId)
                , ("round", intJSON testRunIndex)
                , ("requester", stringJSON requester)
                ]

instance (Monad m, ReportSchemaErrors m) => FromJSON m TestRun where
    fromJSON obj@(JSObject _) = do
        mapping <- fromJSON obj
        platform <- getStringField "platform" mapping
        repository <- do
            repoMapping <- getStringMapField "repository" mapping
            owner <- getStringField "owner" repoMapping
            repo <- getStringField "repo" repoMapping
            pure $ Repository{organization = owner, project = repo}
        directory <- getStringField "directory" mapping
        commitId <- getStringField "commitId" mapping
        testRunIndex <- getIntegralField "round" mapping
        requester <- getStringField "requester" mapping
        pure
            $ TestRun
                { platform = Platform platform
                , repository = repository
                , directory = Directory directory
                , commitId = SHA1 commitId
                , testRunIndex = testRunIndex
                , requester = Username requester
                }
    fromJSON r =
        expectedButGotValue
            "an object representing a test run"
            r

newtype Duration = Duration Int
    deriving (Eq, Show)

data Phase = PendingT | DoneT | RunningT

newtype URL = URL String
    deriving (Show, Eq)

data Reason
    = UnacceptableDuration
    | UnacceptablePlatform
    | UnacceptableRepository
    | UnacceptableCommit
    | UnacceptableRound
    | UnacceptableRequester
    deriving (Eq, Show)

toJSONReason :: Monad m => Reason -> m JSValue
toJSONReason UnacceptableDuration = stringJSON "unacceptable duration"
toJSONReason UnacceptablePlatform = stringJSON "unacceptable platform"
toJSONReason UnacceptableRepository = stringJSON "unacceptable repository"
toJSONReason UnacceptableCommit = stringJSON "unacceptable commit"
toJSONReason UnacceptableRound = stringJSON "unacceptable round"
toJSONReason UnacceptableRequester = stringJSON "unacceptable requester"

fromJSONReason :: (ReportSchemaErrors m) => JSValue -> m Reason
fromJSONReason (JSString jsString) = do
    let reason = fromJSString jsString
    case reason of
        "unacceptable duration" -> pure UnacceptableDuration
        "unacceptable platform" -> pure UnacceptablePlatform
        "unacceptable repository" -> pure UnacceptableRepository
        "unacceptable commit" -> pure UnacceptableCommit
        "unacceptable round" -> pure UnacceptableRound
        "unacceptable requester" -> pure UnacceptableRequester
        _ -> expectedButGotValue "a valid reason" (JSString jsString)
fromJSONReason other =
    expectedButGotValue "a string representing a reason" other

data TestRunState a where
    Pending :: Duration -> TestRunState PendingT
    Rejected :: TestRunState PendingT -> [Reason] -> TestRunState DoneT
    Accepted :: TestRunState PendingT -> TestRunState RunningT
    Finished
        :: TestRunState RunningT -> Duration -> URL -> TestRunState DoneT

deriving instance Eq (TestRunState a)
deriving instance Show (TestRunState a)
instance Monad m => ToJSON m (TestRunState a) where
    toJSON (Pending (Duration d)) =
        object
            [ ("phase", stringJSON "pending")
            , ("duration", intJSON d)
            ]
    toJSON (Rejected pending reasons) =
        object
            [ ("phase", stringJSON "rejected")
            , ("pending", toJSON pending)
            , ("reasons", traverse toJSONReason reasons >>= toJSON)
            ]
    toJSON (Accepted pending) =
        object
            [ ("phase", stringJSON "accepted")
            , ("pending", toJSON pending)
            ]
    toJSON (Finished running duration url) =
        object
            [ ("phase", stringJSON "finished")
            , ("running", toJSON running)
            , ("duration", intJSON $ case duration of Duration d -> d)
            , ("url", stringJSON $ case url of URL u -> u)
            ]

instance (Monad m, ReportSchemaErrors m) => FromJSON m (TestRunState PendingT) where
    fromJSON obj@(JSObject _) = do
        mapping <- fromJSON obj
        phase <- getStringField "phase" mapping
        case phase of
            "pending" -> do
                duration <- getIntegralField "duration" mapping
                pure $ Pending (Duration duration)
            _ ->
                expectedButGotValue
                    "a pending phase"
                    obj
    fromJSON other =
        expectedButGotValue
            "an object representing a pending phase"
            other

instance (Monad m, ReportSchemaErrors m) => FromJSON m (TestRunState DoneT) where
    fromJSON obj@(JSObject _) = do
        mapping <- fromJSON obj
        phase <- getStringField "phase" mapping
        case phase of
            "rejected" -> do
                pending <- getField "pending" mapping >>= fromJSON
                reasons <- getListField "reasons" mapping
                reasonList <- traverse fromJSONReason reasons
                pure $ Rejected pending reasonList
            "finished" -> do
                running <- getField "running" mapping >>= fromJSON
                duration <- getIntegralField "duration" mapping
                url <- getStringField "url" mapping
                pure $ Finished running (Duration duration) (URL url)
            _ ->
                expectedButGotValue
                    "a rejected phase"
                    obj
    fromJSON other =
        expectedButGotValue
            "an object representing a rejected phase"
            other

instance (Monad m, ReportSchemaErrors m) => FromJSON m (TestRunState RunningT) where
    fromJSON obj@(JSObject _) = do
        mapping <- fromJSON obj
        phase <- getStringField "phase" mapping
        case phase of
            "accepted" -> do
                pending <- getField "pending" mapping >>= fromJSON
                pure $ Accepted pending
            _ ->
                expectedButGotValue
                    "an accepted phase"
                    obj
    fromJSON other =
        expectedButGotValue
            "an object representing an accepted phase"
            other
