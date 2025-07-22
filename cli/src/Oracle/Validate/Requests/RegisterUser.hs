module Oracle.Validate.Requests.RegisterUser
    ( validateRegisterUser
    , validateUnregisterUser
    , RegisterUserFailure (..)
    , UnregisterUserFailure (..)
    ) where

import Control.Monad.Trans.Class (lift)
import Core.Types.Basic
    ( Platform (..)
    )
import Core.Types.Change (Change (..), Key (..))
import Core.Types.Operation (Op (..))
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

data UnregisterUserFailure
    = UnregisterUserPlatformNotSupported String
    | UnregisterUserKeyFailure KeyFailure
    | PublicKeyIsPresentOnPlatform -- issue 19300550b3b776dde1b08059780f617e182f067f
    deriving (Show, Eq)

validateUnregisterUser
    :: Monad m
    => Validation m
    -> Change RegisterUserKey (OpD ())
    -> m (ValidationResult UnregisterUserFailure)
validateUnregisterUser
    validation
    change@(Change (Key (RegisterUserKey{platform})) _v) =
        runValidate $ do
            mapFailure UnregisterUserKeyFailure
                $ deleteValidation validation change
            case platform of
                Platform "github" -> pure () -- issue 19300550b3b776dde1b08059780f617e182f067f
                Platform other -> notValidated $ UnregisterUserPlatformNotSupported other
