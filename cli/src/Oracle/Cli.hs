module Oracle.Cli
    ( OracleCommand (..)
    , oracleCmd
    ) where

import Core.Types.Basic (Owner, TokenId)
import MPFS.API (MPFS)
import Oracle.Token.Cli (TokenCommand, tokenCmdCore)
import Oracle.Validate.Cli (ValidateCommand, validateCmd)
import Oracle.Validate.Requests.TestRun.Config
    ( TestRunValidationConfig
    )
import Submitting (Submission)
import Text.JSON.Canonical (JSValue)
import Validation (Validation)

data OracleCommand a where
    OracleTokenCommand :: TokenCommand a -> OracleCommand a
    OracleValidateCommand :: ValidateCommand -> OracleCommand JSValue

deriving instance Show (OracleCommand a)
deriving instance Eq (OracleCommand a)

oracleCmd
    :: Monad m
    => MPFS m
    -> (TokenId -> Validation m)
    -> Submission m
    -> TestRunValidationConfig
    -> Owner
    -> Maybe TokenId
    -> OracleCommand a
    -> m a
oracleCmd mpfs mkValidation submit testRunConfig agentPkh mtk = \case
    OracleTokenCommand tokenCommand ->
        tokenCmdCore
            mpfs
            mkValidation
            submit
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
            mpfs
            testRunConfig
            agentPkh
            validation
            tk
            validateCommand
