{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE StrictData #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Oracle.Validate.Logic
    ( ValidationResult (..)
    , validateRequest
    ) where

import Control.Monad.IO.Class (liftIO)
import Core.Types
    ( Change (..)
    , Key (..)
    , Operation (..)
    , Platform (..)
    , Repository (..)
    , RequestRefId
    , Role (..)
    , Username (..)
    , parseFacts
    )
import Data.List (find)
import Lib.JSON
    ( stringJSON
    )
import Oracle.Github.GetRepoRole qualified as Github
import Oracle.Github.ListPublicKeys qualified as Github
import Oracle.Types (Request (..), RequestZoo (..))
import Oracle.Validate.Requests.TestRun.Config
    ( TestRunValidationConfig
    )
import Oracle.Validate.Requests.TestRun.Create (validateCreateTestRun)
import Servant.Client (ClientM)
import Text.JSON.Canonical.Class (ToJSON (..))
import User.Types
    ( RegisterRoleKey (..)
    , RegisterUserKey (..)
    )
import Validation (Validation (..))

data ValidationResult
    = Validated
    | NotValidated String
    | CannotValidate String
    | NotEvaluated
    deriving (Eq, Show)

instance Monad m => ToJSON m ValidationResult where
    toJSON = \case
        Validated -> stringJSON "validated"
        NotValidated reason -> stringJSON $ "not validated: " <> reason
        CannotValidate reason -> stringJSON $ "cannot validate: " <> reason
        NotEvaluated -> stringJSON "not evaluated"

instance MonadFail ClientM where
    fail = error

validateRequest
    :: TestRunValidationConfig
    -> Validation ClientM
    -> RequestZoo
    -> ClientM (RequestRefId, ValidationResult)
validateRequest _ _validation (RegisterUserRequest (Request refId _owner (Change k _v))) = do
    res <- case k of
        Key (RegisterUserKey{platform, username, pubkeyhash}) ->
            case platform of
                Platform "github" -> do
                    validationRes <- liftIO $ Github.inspectPublicKey username pubkeyhash
                    if validationRes == Github.PublicKeyValidated
                        then
                            pure Validated
                        else
                            pure $ NotValidated (Github.emitPublicKeyMsg validationRes)
                Platform _other ->
                    pure
                        $ CannotValidate
                            "expecting github platform as we are validating only this at this moment"
    pure (refId, res)
validateRequest
    _
    Validation{mpfsGetFacts}
    (UnregisterUserRequest (Request refId _owner (Change (Key k) _v))) = do
        facts <- mpfsGetFacts
        let registration = find (\(k', ()) -> k' == k) $ parseFacts facts
        if null registration
            then
                pure
                    ( refId
                    , NotValidated
                        $ "no registration for platform '"
                            <> show k.platform
                            <> "' and user '"
                            <> show k.username
                            <> "' and public key hash '"
                            <> show k.pubkeyhash
                            <> "' found"
                    )
            else
                pure (refId, Validated)
validateRequest
    _
    Validation{mpfsGetFacts}
    (RegisterRoleRequest (Request refId _owner (Change k _v))) = do
        facts <- mpfsGetFacts
        let Key
                ( RegisterRoleKey
                        platform
                        repository
                        username
                    ) = k
        let registration = flip find (parseFacts facts)
                $ \(RegisterUserKey platform' username' _, ()) ->
                    platform' == platform
                        && username' == username

        if null registration
            then
                let (Platform p) = platform
                    (Repository o r) = repository
                    (Username u) = username
                in  pure
                        ( refId
                        , NotValidated
                            $ "no registration for platform '"
                                <> show p
                                <> "' and repository '"
                                <> show r
                                <> "' of owner '"
                                <> show o
                                <> "' and user '"
                                <> show u
                                <> "' found"
                        )
            else do
                validationRes <-
                    liftIO
                        $ Github.inspectRepoRoleForUser
                            username
                            repository
                            (Role "antithesis")
                if validationRes == Github.RepoRoleValidated
                    then
                        pure (refId, Validated)
                    else
                        pure (refId, NotValidated (Github.emitRepoRoleMsg validationRes))
validateRequest
    _
    Validation{mpfsGetFacts}
    (UnregisterRoleRequest (Request refId _owner (Change (Key k) _v))) = do
        facts <- mpfsGetFacts
        let registration = find (\(k', ()) -> k' == k) $ parseFacts facts
        if null registration
            then
                pure
                    ( refId
                    , NotValidated
                        $ "no registration of the 'antithesis' role for '"
                            <> show k.platform
                            <> "' platform and '"
                            <> show k.repository
                            <> "' repository found"
                    )
            else
                pure (refId, Validated)
validateRequest
    testRunConfig
    validation
    ( CreateTestRequest
            (Request refId _owner (Change (Key testRun) operation))
        ) =
        (,) refId <$> do
            case operation of
                Insert state -> do
                    result <-
                        validateCreateTestRun
                            testRunConfig
                            validation
                            testRun
                            state
                    case result of
                        Nothing -> pure Validated
                        Just rejections ->
                            pure
                                $ CannotValidate
                                $ "test run validation failed for the following reasons: "
                                    <> unwords (fmap show rejections)
                _ ->
                    pure
                        $ CannotValidate
                            "only insert operation is supported for test runs"
validateRequest _ _validation (RejectRequest (Request refId _owner _change)) =
    pure (refId, NotEvaluated)
validateRequest _ _validation (AcceptRequest (Request refId _owner _change)) =
    pure (refId, NotEvaluated)
validateRequest _ _validation (FinishedRequest (Request refId _owner _change)) =
    pure (refId, NotEvaluated)
