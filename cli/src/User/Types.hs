{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module User.Types
    ( TestRun (..)
    )
where

import Core.Types
    ( Directory (..)
    , Platform (..)
    , Repository (..)
    , SHA1 (..)
    , Username (..)
    )
import Data.Map (Map)
import Data.Map qualified as Map
import Text.JSON.Canonical
    ( FromJSON (..)
    , Int54
    , JSValue (..)
    , ReportSchemaErrors (..)
    , ToJSON (..)
    , fromJSString
    , toJSString
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
            ) = do
            mround <- toJSON $ (fromIntegral :: Int -> Int54) testRunIndex
            mrepository <-
                toJSON
                    $ Map.fromList
                        [ ("owner" :: String, JSString $ toJSString owner)
                        , ("repo", JSString $ toJSString repo)
                        ]
            toJSON $ mapping mround mrepository
          where
            mapping mround mrepository =
                Map.fromList
                    [ ("platform" :: String, JSString $ toJSString platform)
                    , ("repository", mrepository)
                    , ("directory", JSString $ toJSString directory)
                    , ("commitId", JSString $ toJSString commitId)
                    , ("round", mround)
                    , ("requester", JSString $ toJSString requester)
                    ]

getField
    :: ReportSchemaErrors m => String -> Map String JSValue -> m JSValue
getField key mapping = case Map.lookup key mapping of
    Nothing -> expected (key <> " key") Nothing
    Just value -> pure value

getStringField
    :: ReportSchemaErrors m
    => String
    -> Map String JSValue
    -> m String
getStringField key mapping = do
    value <- getField key mapping
    case value of
        JSString jsString -> pure $ fromJSString jsString
        _ -> expected ("a stringy " <> key) $ Just "got something else"

getIntegralField
    :: (ReportSchemaErrors m, Num a)
    => String
    -> Map String JSValue
    -> m a
getIntegralField key mapping = do
    value <- getField key mapping
    case value of
        JSNum n -> pure $ fromIntegral n
        _ -> expected ("an integer " <> key) $ Just "got something else"

getStringMapField
    :: ReportSchemaErrors m
    => String
    -> Map String JSValue
    -> m (Map String JSValue)
getStringMapField key mapping = do
    value <- getField key mapping
    case value of
        JSObject _ -> do
            objMapping :: Map String JSValue <- fromJSON value
            pure objMapping
        _ -> expected ("a map " <> key) $ Just "got something else"

instance (Monad m, ReportSchemaErrors m) => FromJSON m TestRun where
    fromJSON obj@(JSObject _) = do
        mapping :: Map String JSValue <- fromJSON obj
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
    fromJSON _ = expected "object" $ Just "something else"
