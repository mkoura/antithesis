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
import Lib.Github.ListPublicKeys qualified as Github
import Oracle.Validate.Types (ValidationResult (..))
import Servant.Client (ClientM)
import User.Types
    ( RegisterUserKey (..)
    )
import Validation (Validation (..))

validateRegisterUser
    :: Validation ClientM
    -> Change RegisterUserKey (OpI ())
    -> ClientM ValidationResult
validateRegisterUser Validation{githubUserPublicKeys} (Change k _v) = case k of
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
    :: Validation ClientM
    -> Change RegisterUserKey (OpD ())
    -> ClientM ValidationResult
validateUnregisterUser Validation{mpfsGetFacts} (Change (Key k) _v) = do
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
