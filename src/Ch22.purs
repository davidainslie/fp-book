module Ch22 where

import Data.Identity
import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Console (log)
import Effect.Random
import Prelude

{-
One way of thinking about Effect is that we can use it to interface with the real world.
Another way to think of Effect is that it’s a Monad for Synchronous Effects.
No matter how complex our program gets, it eventually becomes one massive Function that runs in the Effect Monad.
-}

-----------------------------------------------------

test :: Effect Unit
test = do
  n <- random -- Where random is declared as: random :: Effect Number
  log $ show n