{-# LANGUAGE DeriveFunctor #-}

module Oracle.Validate.Types
    ( ValidationResult
    , AValidationResult
    , Validate (..)
    , ValidationFailure (..)
    , runValidate
    , notValidated
    , cannotValidate
    , mapFailure
    , throwNotValid
    , throwJusts
    , withValidationResult
    ) where

import Control.Exception (Exception)
import Control.Monad.Trans.Class (MonadTrans)
import Control.Monad.Trans.Except
    ( ExceptT (..)
    , runExceptT
    , throwE
    , withExceptT
    )
import Data.Typeable (Typeable)
import Lib.JSON
    ( stringJSON
    )
import Text.JSON.Canonical.Class (ToJSON (..))

data ValidationFailure e
    = NotValidated e
    | CannotValidate String
    deriving (Eq, Show, Functor)

instance (Typeable e, Show e) => Exception (ValidationFailure e)

newtype Validate e m a = Validate
    { _runValidate :: ExceptT (ValidationFailure e) m a
    }
    deriving (Functor, Applicative, Monad)

deriving instance MonadTrans (Validate e)

type AValidationResult e a = Either (ValidationFailure e) a

withValidationResult
    :: (e -> e')
    -> AValidationResult e a
    -> AValidationResult e' a
withValidationResult f = either (Left . fmap f) Right

type ValidationResult e = AValidationResult e ()

notValidated :: Monad m => e -> Validate e m a
notValidated reason = Validate $ throwE $ NotValidated reason

cannotValidate :: Monad m => String -> Validate e m a
cannotValidate reason = Validate $ throwE $ CannotValidate reason

runValidate :: Validate e m a -> m (AValidationResult e a)
runValidate (Validate action) = runExceptT action

throwNotValid :: Applicative m => AValidationResult String a -> m a
throwNotValid (Left (NotValidated reason)) =
    error $ "Validation failed: " ++ reason
throwNotValid (Left (CannotValidate reason)) =
    error $ "Cannot validate: " ++ reason
throwNotValid (Right result) = pure result

throwJusts :: Monad m => Maybe e -> Validate e m ()
throwJusts Nothing = pure ()
throwJusts (Just e) = notValidated e

mapFailure
    :: Functor m => (e -> e') -> Validate e m a -> Validate e' m a
mapFailure f (Validate action) = Validate $ withExceptT (fmap f) action
instance (Monad m, Show e) => ToJSON m (ValidationResult e) where
    toJSON = \case
        Right () -> stringJSON "validated"
        Left (NotValidated reason) ->
            stringJSON
                $ "not validated: "
                    <> show reason
        Left (CannotValidate reason) ->
            stringJSON
                $ "cannot validate: "
                    <> reason
