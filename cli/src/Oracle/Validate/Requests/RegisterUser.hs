module Oracle.Validate.Requests.RegisterUser
    ( validateRegisterUser
    , validateUnregisterUser
    , RegisterUserFailure (..)
    ) where

import Control.Monad (when)
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
    , mapFailure
    , notValidated
    , runValidate
    , throwJusts
    )
import User.Types
    ( RegisterUserKey (..)
    )
import Validation
    ( KeyFailure
    , Validation (..)
    , deleteValidation
    , insertValidation
    )
import Validation.RegisterUser qualified as Github

data RegisterUserFailure
    = PublicKeyValidationFailure Github.PublicKeyFailure
    | RegisterUserPlatformNotSupported String
    | RegisterUserKeyFailure KeyFailure
    deriving (Show, Eq)

validateRegisterUser
    :: Monad m
    => Validation m
    -> Change RegisterUserKey (OpI ())
    -> m (ValidationResult RegisterUserFailure)
validateRegisterUser
    validation@Validation{githubUserPublicKeys}
    change@(Change (Key (RegisterUserKey{platform, username, pubkeyhash})) _) =
        runValidate $ do
            mapFailure RegisterUserKeyFailure $ insertValidation validation change
            case platform of
                Platform "github" -> do
                    validationRes <- lift $ githubUserPublicKeys username pubkeyhash
                    mapFailure PublicKeyValidationFailure $ throwJusts validationRes
                Platform other -> notValidated $ RegisterUserPlatformNotSupported other

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
