module Validation
    ( Validation (..)
    ) where

import Core.Types (Fact)

-- | Abstract the side effects necessary for validation.
newtype Validation m = Validation
    { facts :: m [Fact]
    }
