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
    , Platform (..)
    , PublicKeyHash (..)
    , RequestRefId
    , TokenId
    , Username (..)
    )
import Lib.JSON
    ( stringJSON
    )
import MPFS.API
    ( getTokenFacts
    )
import Oracle.Types (Request (..), RequestZoo (..))
import Servant.Client (ClientM)
import Text.JSON.Canonical (JSValue (..), toJSString)
import Text.JSON.Canonical.Class (ToJSON (..))
import User.Types (RegisterUserKey (..))

import Oracle.Github.ListPublicKeys qualified as Github

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
    :: TokenId
    -> RequestZoo
    -> ClientM (RequestRefId, ValidationResult)
validateRequest _tk (RegisterUserRequest (Request refId _owner (Change k _v))) = do
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
validateRequest tk (UnregisterUserRequest (Request refId _owner (Change k _v))) = do
    JSObject facts <- getTokenFacts tk
    let Key
            ( RegisterUserKey
                    (Platform platform)
                    (Username username)
                    (PublicKeyHash pubkeyhash)
                ) = k
    let expEntry =
            ( "key"
            , JSObject
                [ ("platform", JSString $ toJSString platform)
                , ("publickeyhash", JSString $ toJSString pubkeyhash)
                , ("type", JSString $ toJSString "register-user")
                , ("user", JSString $ toJSString username)
                ]
            )
    let findRes = filter (== expEntry) facts
    if null findRes
        then
            pure (refId, NotValidated "no mirror user registration fact found")
        else
            pure (refId, Validated)
validateRequest _tk (RegisterRoleRequest (Request refId _owner _change)) =
    pure (refId, NotEvaluated)
validateRequest _tk (UnregisterRoleRequest (Request refId _owner _change)) =
    pure (refId, NotEvaluated)
validateRequest _tk (CreateTestRequest (Request refId _owner _change)) =
    pure (refId, NotEvaluated)
validateRequest _tk (RejectRequest (Request refId _owner _change)) =
    pure (refId, NotEvaluated)
validateRequest _tk (AcceptRequest (Request refId _owner _change)) =
    pure (refId, NotEvaluated)
validateRequest _tk (FinishedRequest (Request refId _owner _change)) =
    pure (refId, NotEvaluated)
