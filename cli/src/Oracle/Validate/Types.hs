{-# LANGUAGE DeriveFunctor #-}

module Oracle.Validate.Types
    ( ValidationResult
    , Validate
    , AValidationResult (..)
    , Validated (..)
    , runValidate
    , notValidated
    , mapFailure
    , throwJusts
    , withValidationResult
    , throwValidationResult
    , sequenceValidate
    ) where

import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Except
    ( ExceptT (..)
    , runExceptT
    , throwE
    , withExceptT
    )
import Lib.JSON
    ( object
    , (.=)
    )
import Text.JSON.Canonical.Class (ToJSON (..))

type Validate e m a = ExceptT e m a

data AValidationResult e a
    = ValidationSuccess a
    | ValidationFailure e
    deriving (Show, Eq, Functor)

withValidationResult
    :: (e -> e')
    -> AValidationResult e a
    -> AValidationResult e' a
withValidationResult _ (ValidationSuccess a) = ValidationSuccess a
withValidationResult f (ValidationFailure e) = ValidationFailure (f e)

data Validated = Validated
    deriving (Show, Eq)

instance Monad m => ToJSON m Validated where
    toJSON _ = toJSON ("validated" :: String)

type ValidationResult e = AValidationResult e Validated

notValidated :: Monad m => e -> Validate e m a
notValidated = throwE

runValidate
    :: Functor m => Validate e m a -> m (AValidationResult e a)
runValidate = fmap (either ValidationFailure ValidationSuccess) . runExceptT

throwJusts :: Monad m => Maybe e -> Validate e m Validated
throwJusts Nothing = pure Validated
throwJusts (Just e) = notValidated e

throwValidationResult
    :: Monad m => ValidationResult e -> Validate e m Validated
throwValidationResult (ValidationSuccess a) = pure a
throwValidationResult (ValidationFailure e) = notValidated e

mapFailure
    :: Functor m => (e -> e') -> Validate e m a -> Validate e' m a
mapFailure = withExceptT

instance (Monad m, ToJSON m a, ToJSON m e) => ToJSON m (AValidationResult e a) where
    toJSON = \case
        ValidationSuccess a -> toJSON a
        ValidationFailure e ->
            object ["validationFailed" .= e]

sequenceValidate :: Monad m => [Validate e m a] -> Validate [e] m [a]
sequenceValidate = go [] []
  where
    go xs es [] =
        if null es
            then pure xs
            else notValidated es
    go xs es (v : vs) = do
        result <- lift $ runValidate v
        case result of
            ValidationSuccess x -> go (x : xs) es vs
            ValidationFailure e -> go xs (e : es) vs
