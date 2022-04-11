module Ch222 where

import Control.Monad.Error.Class (try)
import Data.Either (Either(..))
import Data.Identity
import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)
import Data.Tuple (Tuple(..))
import Effect.Aff (Aff, launchAff_)
import Effect (Effect)
import Effect.Class.Console (log)
import Node.Encoding (Encoding(..))
import Node.FS.Aff (readTextFile)
import Prelude

{-
While Effect is the Monad that our program ultimately runs in, there’s another more powerful effects
Monad called Aff, which is a Monad for Asynchronous Effects.
-}

readAFile :: Aff Unit
readAFile = do
  -- "try" results in Either Error String, where
  -- try :: ∀ e m a. MonadError e m => m a -> m (Either e a)
  result <- try $ readTextFile ASCII "README.md"
  case result of
    Right fileData -> log fileData
    Left err -> log $ show err

-----------------------------------------------------

-- Run: npx spago run --main Ch222
main :: Effect Unit
main = launchAff_ readAFile