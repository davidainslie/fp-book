-- JSON and Ajax

module Ch24 where

import Effect (Effect)
import Effect.Console (log)
import Prelude

{-
Type Alias, F, represents one or more errors when working with Javascript data. It's definition is:
type F = Except MultipleErrors
where
Except is built with ExceptT and the Identity Monad.

Type Alias definition for MultipleErrors:
type MultipleErrors = NonEmptyList ForeignError

And the definition of ForeignError:
data ForeignError =
  ForeignError String |
  TypeMismatch String String |
  ErrorAtIndex Int ForeignError |
  ErrorAtProperty String ForeignError
-}

-----------------------------------------------------

-- Run: npx spago run --main Ch24
main :: Effect Unit
main = do
  log "json"