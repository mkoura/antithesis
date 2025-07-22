module Oracle.Validate.Requests.RegisterUser
    ( validateRegisterUser
    , validateUnregisterUser
    ) where

import Core.Types.Basic
    ( Platform (..)
    )
import Core.Types.Change (Change (..), Key (..))
import Core.Types.Fact (Fact (..))
import Core.Types.Operation (Op (..))
import Data.List (find)
import Oracle.Validate.Types (ValidationResult (..))
import User.Types
    ( RegisterUserKey (..)
    )
import Validation
    ( Validation (..)
    , deleteValidation
    , insertValidation
    )
import Validation.RegisterUser qualified as Github

validateRegisterUser
    :: Monad m
    => Validation m
    -> Change RegisterUserKey (OpI ())
    -> m ValidationResult
validateRegisterUser
    validation@Validation{githubUserPublicKeys}
    change@(Change k _v) = do
        insertionValidation <- insertValidation validation change
        if insertionValidation /= Validated
            then pure insertionValidation
            else case k of
                Key (RegisterUserKey{platform, username, pubkeyhash}) ->
                    case platform of
                        Platform "github" -> do
                            validationRes <- githubUserPublicKeys username pubkeyhash
                            if validationRes == Github.PublicKeyValidated
                                then
                                    pure Validated
                                else
                                    pure $ NotValidated (Github.emitPublicKeyMsg validationRes)
                        Platform _other ->
                            pure
                                $ CannotValidate
                                    "expecting github platform as we are validating only this at this moment"

validateUnregisterUser
    :: Monad m
    => Validation m
    -> Change RegisterUserKey (OpD ())
    -> m ValidationResult
validateUnregisterUser
    validation@Validation{mpfsGetFacts}
    change@(Change (Key k) _v) = do
        deletionValidation <- deleteValidation validation change
        if deletionValidation /= Validated
            then pure deletionValidation
            else do
                facts <- mpfsGetFacts
                let registration = find (\(Fact k' ()) -> k' == k) facts
                if null registration
                    then
                        pure
                            $ NotValidated
                            $ "no registration for platform '"
                                <> show (platform k)
                                <> "' and user '"
                                <> show (username k)
                                <> "' and public key hash '"
                                <> show (pubkeyhash k)
                                <> "' found"
                    else
                        pure Validated
