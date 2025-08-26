module Oracle.Validate.Requests.RegisterUser
    ( validateRegisterUser
    , validateUnregisterUser
    , RegisterUserFailure (..)
    , UnregisterUserFailure (..)
    ) where

import Control.Monad (void, when)
import Control.Monad.Trans.Class (lift)
import Core.Types.Basic
    ( Platform (..)
    )
import Core.Types.Change (Change (..), Key (..))
import Core.Types.Operation (Op (..))
import Data.List (find)
import Lib.JSON.Canonical.Extra (object, (.=))
import Oracle.Types (requestZooGetRegisterUserKey)
import Oracle.Validate.Types
    ( ForRole
    , Validate
    , Validated (..)
    , forUser
    , mapFailure
    , notValidated
    , throwJusts
    )
import Text.JSON.Canonical (ToJSON (..))
import User.Types
    ( RegisterUserKey (..)
    )
import Validation
    ( KeyFailure
    , Validation (..)
    , deleteValidation
    , insertValidation
    )
import Validation.RegisterUser
    ( PublicKeyFailure
    )

data RegisterUserFailure
    = PublicKeyValidationFailure PublicKeyFailure
    | RegisterUserPlatformNotSupported String
    | RegisterUserKeyFailure KeyFailure
    | RegisterUserKeyChangeAlreadyPending RegisterUserKey
    deriving (Show, Eq)

instance Monad m => ToJSON m RegisterUserFailure where
    toJSON = \case
        PublicKeyValidationFailure reason ->
            object ["publicKeyValidationFailure" .= reason]
        RegisterUserPlatformNotSupported platform ->
            object ["registerUserPlatformNotSupported" .= platform]
        RegisterUserKeyFailure keyFailure ->
            object ["registerUserKeyFailure" .= keyFailure]
        RegisterUserKeyChangeAlreadyPending key ->
            object ["registerUserKeyChangeAlreadyPending" .= key]

validateRegisterUser
    :: Monad m
    => Validation m
    -> ForRole
    -> Change RegisterUserKey (OpI ())
    -> Validate RegisterUserFailure m Validated
validateRegisterUser
    validation@Validation{githubUserPublicKeys, mpfsGetTokenRequests}
    forRole
    change@(Change (Key key@(RegisterUserKey{platform, username, pubkeyhash})) _) = do
        rqs <- lift mpfsGetTokenRequests
        when (forUser forRole)
            $ void
            $ throwJusts
                ( RegisterUserKeyChangeAlreadyPending key
                    <$ find (\r -> requestZooGetRegisterUserKey r == Just key) rqs
                )
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

instance Monad m => ToJSON m UnregisterUserFailure where
    toJSON = \case
        UnregisterUserPlatformNotSupported platform ->
            object ["unregisterUserPlatformNotSupported" .= platform]
        UnregisterUserKeyFailure keyFailure ->
            object ["unregisterUserKeyFailure" .= keyFailure]
        PublicKeyIsPresentOnPlatform ->
            object ["publicKeyIsPresentOnPlatform" .= ()]

validateUnregisterUser
    :: Monad m
    => Validation m
    -> Change RegisterUserKey (OpD ())
    -> Validate UnregisterUserFailure m Validated
validateUnregisterUser
    validation
    change@(Change (Key (RegisterUserKey{platform})) _v) = do
        void
            $ mapFailure UnregisterUserKeyFailure
            $ deleteValidation validation change
        case platform of
            Platform "github" -> pure Validated -- issue 19300550b3b776dde1b08059780f617e182f067f
            Platform other -> notValidated $ UnregisterUserPlatformNotSupported other
