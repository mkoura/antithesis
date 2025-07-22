{-# OPTIONS_GHC -Wno-orphans #-}

module Oracle.Validate.Types
    ( ValidationResult
    , Validate
    , AValidationResult
    , runValidate
    , notValidated
    , mapFailure
    , throwNotValid
    , throwJusts
    , withValidationResult
    ) where

import Control.Monad.Trans.Except
    ( ExceptT (..)
    , runExceptT
    , throwE
    , withExceptT
    )
import Lib.JSON
    ( stringJSON
    )
import Text.JSON.Canonical.Class (ToJSON (..))

type Validate e m a = ExceptT e m a

type AValidationResult e a = Either e a

withValidationResult
    :: (e -> e')
    -> AValidationResult e a
    -> AValidationResult e' a
withValidationResult f = either (Left . f) Right

type ValidationResult e = AValidationResult e ()

notValidated :: Monad m => e -> Validate e m a
notValidated = throwE

runValidate :: Validate e m a -> m (AValidationResult e a)
runValidate = runExceptT

throwNotValid :: Applicative m => AValidationResult String a -> m a
throwNotValid (Left reason) =
    error $ "Validation failed: " ++ reason
throwNotValid (Right result) = pure result

throwJusts :: Monad m => Maybe e -> Validate e m ()
throwJusts Nothing = pure ()
throwJusts (Just e) = notValidated e

mapFailure
    :: Functor m => (e -> e') -> Validate e m a -> Validate e' m a
mapFailure = withExceptT

instance (Monad m, Show e) => ToJSON m (ValidationResult e) where
    toJSON = \case
        Right () -> stringJSON "validated"
        Left reason ->
            stringJSON
                $ "not validated: "
                    <> show reason
