module Oracle.Cli
    ( OracleCommand (..)
    , oracleCmd
    ) where

import Core.Types (TokenId, Wallet)
import Oracle.Token.Cli (TokenCommand, tokenCmdCore)
import Oracle.Validate.Cli (ValidateCommand, validateCmd)
import Oracle.Validate.Requests.TestRun.Config
    ( TestRunValidationConfig
    )
import Servant.Client (ClientM)
import Submitting (Submitting)
import Text.JSON.Canonical (JSValue)
import Validation (mkValidation)

data OracleCommand a where
    OracleTokenCommand :: TokenCommand a -> OracleCommand a
    OracleValidateCommand :: ValidateCommand -> OracleCommand JSValue

deriving instance Show (OracleCommand a)
deriving instance Eq (OracleCommand a)

oracleCmd
    :: Submitting
    -> Wallet
    -> TestRunValidationConfig
    -> Maybe TokenId
    -> OracleCommand a
    -> ClientM a
oracleCmd sbmt wallet testRunConfig (Just tk) = \case
    OracleTokenCommand tokenCommand ->
        tokenCmdCore
            sbmt
            wallet
            (Just tk)
            tokenCommand
    OracleValidateCommand validateCommand -> do
        let validation = mkValidation tk
        validateCmd
            testRunConfig
            validation
            tk
            validateCommand
oracleCmd sbmt wallet _testRunConfig Nothing = \case
    OracleTokenCommand tokenCommand ->
        tokenCmdCore
            sbmt
            wallet
            Nothing
            tokenCommand
    OracleValidateCommand _validateCommand ->
        error "TokenId is required for ValidateCommand"
