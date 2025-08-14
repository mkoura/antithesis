module Oracle.Validate.Requests.Config
    ( validateConfig
    , ConfigFailure (..)
    )
where

import Control.Monad (when)
import Core.Types.Basic (Owner)
import Core.Types.Change (Change (..))
import Core.Types.Operation (Op (..), Operation (..))
import Lib.JSON.Canonical.Extra (object, (.=))
import Oracle.Config.Types
    ( Config (Config, configTestRun)
    , ConfigKey
    )
import Oracle.Validate.Requests.TestRun.Config
    ( TestRunValidationConfig (..)
    )
import Oracle.Validate.Types
    ( Validate
    , Validated (..)
    , mapFailure
    , notValidated
    )
import Text.JSON.Canonical (Int54, ToJSON (..))
import Validation (KeyFailure, Validation, insertValidation)

data ConfigFailure
    = ConfigureKeyValidationFailure KeyFailure
    | ConfigureNotFromOracle Owner
    | ConfigureMinLessThanOne Int
    | ConfigureMaxLessThanMin Int Int
    deriving (Show, Eq)

instance Monad m => ToJSON m ConfigFailure where
    toJSON (ConfigureKeyValidationFailure keyFailure) =
        object ["configureKeyValidationFailure" .= keyFailure]
    toJSON (ConfigureNotFromOracle owner) =
        object ["configureNotFromOracle" .= show owner]
    toJSON (ConfigureMinLessThanOne minD) =
        object ["configureMinLessThanOne" .= fromIntegral @_ @Int54 minD]
    toJSON (ConfigureMaxLessThanMin maxD minD) =
        object
            [ (,) "configureMaxLessThanMin"
                $ object
                    [ "max" .= fromIntegral @_ @Int54 maxD
                    , "min" .= fromIntegral @_ @Int54 minD
                    ]
            ]

validateConfig
    :: Monad m
    => Validation m
    -> Owner
    -- ^ Oracle
    -> Owner
    -- ^ Submitter
    -> Change ConfigKey (OpI Config)
    -> Validate ConfigFailure m Validated
validateConfig
    v
    oracleOwner
    submitterOwner
    change@(Change _ (Insert Config{configTestRun})) = do
        mapFailure ConfigureKeyValidationFailure $ insertValidation v change
        when (submitterOwner /= oracleOwner)
            $ notValidated
            $ ConfigureNotFromOracle submitterOwner
        let minD = minDuration configTestRun
            maxD = maxDuration configTestRun
        when (minD < 1)
            $ notValidated
            $ ConfigureMinLessThanOne minD
        when (maxD < minD)
            $ notValidated
            $ ConfigureMaxLessThanMin maxD minD
        pure Validated
