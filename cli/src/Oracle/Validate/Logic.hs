{-# LANGUAGE StrictData #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Oracle.Validate.Logic
    ( ValidationResult (..)
    , validateRequest
    ) where

import Control.Monad.IO.Class (liftIO)
import Core.Types (Change (..), Key (..), Platform (..), RequestRefId)
import Lib.JSON
    ( stringJSON
    )
import Oracle.Types (Request (..), RequestZoo (..))
import Servant.Client (ClientM)
import Text.JSON.Canonical.Class (ToJSON (..))
import User.Types (RegisterUserKey (..))

import qualified Oracle.Github.ListPublicKeys as Github

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

validateRequest
    :: RequestZoo
    -> ClientM (RequestRefId, ValidationResult)
validateRequest (RegisterUserRequest (Request refId _owner (Change k _v))) = do
    res <- case k of
        Key (RegisterUserKey {platform,username,pubkeyhash}) ->
            case platform of
                Platform "github" -> do
                    validationRes <- liftIO $ Github.inspectPublicKey username pubkeyhash
                    if validationRes == Github.PublicKeyValidated then
                        pure Validated
                    else
                        pure $ NotValidated (Github.emitPublicKeyMsg validationRes)
                Platform other ->
                    pure $ CannotValidate "expecting github platform as we are validating only this at this moment"
        _ ->
            pure $ CannotValidate "expecting RegisterUserKey"
    pure (refId, res)
validateRequest (UnregisterUserRequest (Request refId _owner _change)) =
    pure (refId, NotEvaluated)
validateRequest (RegisterRoleRequest (Request refId _owner _change)) =
    pure (refId, NotEvaluated)
validateRequest (UnregisterRoleRequest (Request refId _owner _change)) =
    pure (refId, NotEvaluated)
validateRequest (CreateTestRequest (Request refId _owner _change)) =
    pure (refId, NotEvaluated)
validateRequest (RejectRequest (Request refId _owner _change)) =
    pure (refId, NotEvaluated)
validateRequest (AcceptRequest (Request refId _owner _change)) =
    pure (refId, NotEvaluated)
validateRequest (FinishedRequest (Request refId _owner _change)) =
    pure (refId, NotEvaluated)
