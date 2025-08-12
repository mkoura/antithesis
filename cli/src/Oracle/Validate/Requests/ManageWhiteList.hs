module Oracle.Validate.Requests.ManageWhiteList (validateAddWhiteListed, validateRemoveWhiteListed)
where

import Core.Types.Basic (Platform, Repository)
import Core.Types.Change (Change)
import Core.Types.Operation (Op (..))
import Oracle.Validate.Types (Validate, Validated (..))
import User.Agent.Types (WhiteListKey)
import Validation (Validation)

data UpdateWhiteListFailure
    = PlatformMissing Platform
    | RepositoryMissing Repository
    deriving (Show, Eq)

validateAddWhiteListed
    :: Monad m
    => Validation m
    -> Change WhiteListKey (OpI ())
    -> Validate UpdateWhiteListFailure m Validated
validateAddWhiteListed _ _ = pure Validated

validateRemoveWhiteListed
    :: Monad m
    => Validation m
    -> Change WhiteListKey (OpD ())
    -> Validate UpdateWhiteListFailure m Validated
validateRemoveWhiteListed _ _ = pure Validated
