module Oracle.Config.Cli
    ( configCmd
    , ConfigCmd (..)
    )
where

import Control.Monad.Trans.Class (lift)
import Core.Context (WithContext, askMpfs, askSubmit)
import Core.Types.Basic (TokenId)
import Core.Types.Tx (WithTxHash (..))
import Core.Types.Wallet (Wallet)
import Data.Functor (void)
import Facts (FactsSelection (..), factsCmd)
import MPFS.API
import Oracle.Config.Types
import Submitting (Submission (..))
import Text.JSON.Canonical

data ConfigCmd a where
    SetConfig :: TokenId -> Wallet -> Config -> ConfigCmd (WithTxHash ())

deriving instance Show (ConfigCmd a)
deriving instance Eq (ConfigCmd a)

configCmd
    :: Monad m => ConfigCmd a -> WithContext m a
configCmd (SetConfig tokenId wallet config) = do
    mpfs <- askMpfs
    Submission submit <- askSubmit wallet
    present <- lift $ factsCmd mpfs tokenId ConfigFact
    jkey <- toJSON ConfigKey
    jvalue <- toJSON config
    case present of
        [oldConfig] -> do
            oldValue <- toJSON oldConfig
            lift $ fmap void $ submit $ \address ->
                mpfsRequestUpdate mpfs address tokenId
                    $ RequestUpdateBody
                        { key = jkey
                        , newValue = jvalue
                        , oldValue
                        }
        _ -> do
            lift $ fmap void $ submit $ \address ->
                mpfsRequestInsert mpfs address tokenId
                    $ RequestInsertBody{key = jkey, value = jvalue}
