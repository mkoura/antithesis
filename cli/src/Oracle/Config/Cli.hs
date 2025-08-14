module Oracle.Config.Cli
    ( configCmd
    , ConfigCmd (..)
    )
where

import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Trans.Class (lift)
import Core.Context (WithContext, askMpfs, askSubmit)
import Core.Types.Basic (TokenId)
import Core.Types.Fact (Fact (..), parseFacts)
import Core.Types.Tx (WithTxHash (..))
import Data.Functor (void)
import MPFS.API
import Oracle.Config.Types
import Submitting (Submission (..))
import Text.JSON.Canonical

data ConfigCmd a where
    SetConfig :: TokenId -> Config -> ConfigCmd (WithTxHash ())
    GetConfig :: TokenId -> ConfigCmd (Maybe Config)

configCmd
    :: MonadIO m => ConfigCmd a -> WithContext m a
configCmd (SetConfig tokenId config) = do
    mpfs <- askMpfs
    Submission submit <- askSubmit
    present <- configCmd (GetConfig tokenId)
    jkey <- toJSON ConfigKey
    jvalue <- toJSON config
    case present of
        Nothing -> do
            lift $ fmap void $ submit $ \address ->
                mpfsRequestInsert mpfs address tokenId
                    $ RequestInsertBody{key = jkey, value = jvalue}
        Just oldConfig -> do
            oldValue <- toJSON oldConfig
            lift $ fmap void $ submit $ \address ->
                mpfsRequestUpdate mpfs address tokenId
                    $ RequestUpdateBody
                        { key = jkey
                        , newValue = jvalue
                        , oldValue
                        }
configCmd (GetConfig tokenId) = do
    mpfs <- askMpfs
    facts :: [Fact ConfigKey Config] <-
        fmap parseFacts
            $ lift
            $ mpfsGetTokenFacts mpfs tokenId
    pure $ case facts of
        [Fact _ c] -> Just c
        _ -> Nothing
