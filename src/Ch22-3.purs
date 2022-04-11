module Ch223 where

import Control.Monad.Error.Class (try)
import Data.Either (Either(..))
import Data.Identity
import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)
import Data.Time.Duration (Milliseconds(..))
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Aff (Aff, delay, forkAff, launchAff_)
import Effect.Aff.AVar (AVar)
import Effect.Aff.AVar as AVar
import Effect.Class.Console (log)
import Node.Encoding (Encoding(..))
import Node.FS.Aff (readTextFile)
import Prelude

{-
AVars

Imagine you have a Fiber that reads a File and you want that data to be processed by another Fiber.
We can accomplish this by using a mechanism called an AVar, which stands for Asynchrounous Variable.

A Fiber can read and write to the AVar and even block until data becomes available.

Let’s write a program that runs 2 Fibers, one, which reads a File and sends it to the other via an AVar:
-}

-- This takes an AVar that it’ll put the File data to. Notice how the AVar Type takes a single Type Parameter. This is the Type of data that you can put to it. In this case, it’s a String.
readAFileAfterTwoSeconds :: AVar String -> Aff Unit
readAFileAfterTwoSeconds fileAVar = do
  delay (Milliseconds 2000.0)
  result <- try $ readTextFile ASCII "testing.txt"
  case result of
    Right text -> AVar.put text fileAVar
    Left err -> log $ show err

processFile :: AVar String -> Aff Unit
processFile fileAVar = do
  text <- AVar.take fileAVar
  log text

-----------------------------------------------------

-- Run: npx spago run --main Ch223
main :: Effect Unit
main = launchAff_ do
  fileAVar <- AVar.empty -- This is how to create a new AVar that’s initially empty. To create an AVar with initial data, use AVar.new and pass it an initial Value.
  void $ forkAff $ processFile fileAVar
  void $ forkAff $ readAFileAfterTwoSeconds fileAVar

{-
AVars can be used as a poor-man’s queue where multiple Fibers write to the same AVar.
In that scenario, trying to put to a non-empty AVar will block the Producer until the AVar is emptied by the Consumer using take.
And trying to take an empty AVar will block the Consumer until the AVar is written to by the Producer.
-}