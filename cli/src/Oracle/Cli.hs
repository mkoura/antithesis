module Oracle.Cli
    ( OracleCommand (..)
    , oracleCmd
    ) where

import Core.Types.Basic (Owner, TokenId)
import Core.Types.Wallet (Wallet)
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
    -> Owner
    -> Maybe TokenId
    -> OracleCommand a
    -> ClientM a
oracleCmd sbmt wallet testRunConfig agentPkh mtk = \case
    OracleTokenCommand tokenCommand ->
        tokenCmdCore
            sbmt
            wallet
            mtk
            testRunConfig
            agentPkh
            tokenCommand
    OracleValidateCommand validateCommand -> do
        tk <- case mtk of
            Just tokenId -> pure tokenId
            Nothing -> error "TokenId is required for ValidateCommand"
        let validation = mkValidation tk
        validateCmd
            testRunConfig
            agentPkh
            validation
            tk
            validateCommand
