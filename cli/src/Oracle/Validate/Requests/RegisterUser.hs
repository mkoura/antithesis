module Oracle.Validate.Requests.RegisterUser
    ( validateRegisterUser
    , validateUnregisterUser
    ) where

import Control.Monad (unless, when)
import Control.Monad.Trans.Class (lift)
import Core.Types.Basic
    ( Platform (..)
    )
import Core.Types.Change (Change (..), Key (..))
import Core.Types.Fact (Fact (..))
import Core.Types.Operation (Op (..))
import Data.List (find)
import Oracle.Validate.Types
    ( ValidationResult
    , cannotValidate
    , mapFailure
    , notValidated
    , runValidate
    )
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
    -> m (ValidationResult String)
validateRegisterUser
    validation@Validation{githubUserPublicKeys}
    change@(Change k _v) = runValidate $ do
        mapFailure show $ insertValidation validation change
        case k of
            Key (RegisterUserKey{platform, username, pubkeyhash}) ->
                case platform of
                    Platform "github" -> do
                        validationRes <- lift $ githubUserPublicKeys username pubkeyhash
                        unless (validationRes == Github.PublicKeyValidated)
                            $ notValidated (Github.emitPublicKeyMsg validationRes)
                    Platform _other ->
                        cannotValidate
                            "expecting github platform as we are validating only this at this moment"

validateUnregisterUser
    :: Monad m
    => Validation m
    -> Change RegisterUserKey (OpD ())
    -> m (ValidationResult String)
validateUnregisterUser
    validation@Validation{mpfsGetFacts}
    change@(Change (Key k) _v) = runValidate $ do
        mapFailure show $ deleteValidation validation change
        facts <- lift mpfsGetFacts
        let registration = find (\(Fact k' ()) -> k' == k) facts
        when (null registration)
            $ notValidated
            $ "no registration for platform '"
                <> show (platform k)
                <> "' and user '"
                <> show (username k)
                <> "' and public key hash '"
                <> show (pubkeyhash k)
                <> "' found"
