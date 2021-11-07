-- Abstract Algebra

{- 
Small reminder regarding "natural transformation"

data NonEmpty f a = NonEmpty a (f a)

newtype NonEmptyList a = NonEmptyList (NonEmpty List a)

toList :: NonEmptyList ~> List

where the natural transformation is shorthand for:
toList :: ∀ a. NonEmptyList a -> List a
-}

module Ch8 where

import Effect (Effect)
import Effect.Console (log)
import Prelude

test :: Effect Unit
test = do
  log $ show $ "test"