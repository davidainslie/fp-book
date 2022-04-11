module Ch2221b where

import Control.Monad.Error.Class (try)
import Data.Either (Either(..))
import Data.Identity
import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)
import Data.Time.Duration (Milliseconds(..))
import Data.Tuple (Tuple(..))
import Effect.Aff (Aff, Fiber, delay, forkAff, killFiber, launchAff_, runAff)
import Effect (Effect)
import Effect.Class.Console (log)
import Effect.Exception (error) -- error :: String -> Error
import Node.Encoding (Encoding(..))
import Node.FS.Aff (readTextFile)
import Prelude

{-
Fibers can return values after they’re finished running, but we have to use runAff to capture that value:

runAff
  :: ∀ a. (Either Error a -> Effect Unit)
  -> Aff a
  -> Effect (Fiber Unit)
-}

logEverySecond :: Aff Unit
logEverySecond = go 0 where
  go x = do
    log $ show x
    delay (Milliseconds 1000.0)
    go $ x + 1

-- We change readAFileAfterTwoSeconds to return the results of reading the file:
readAFileAfterTwoSeconds :: Aff String
readAFileAfterTwoSeconds = do
  delay (Milliseconds 2000.0)
  result <- try $ readTextFile ASCII "testing.txt"
  pure $ case result of
    Right text -> text
    Left err -> show err

-- We’d like to have both of these (above) Functions running simultaneously. We’ll let them run for 5 seconds and then terminate them.

kill :: ∀ a. Fiber a -> Aff Unit
kill = killFiber (error "Killing you softly...")

-----------------------------------------------------

-- Run: npx spago run --main Ch2221b
{- FIXME
main :: Effect Unit
main = do
  fileReader <- runAff case _ of
    Left err -> log $ show err
    Right result -> log $ "File contents: " <> show result $ readAFileAfterTwoSeconds
  launchAff_ do
    logger <- forkAff logEverySecond
    delay (Milliseconds 5000.0)
    kill logger
    kill fileReader
-}