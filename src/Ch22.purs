-- Code StateT

module Ch22 where

import Data.Identity
import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Console (log)
import Prelude

-----------------------------------------------------

test :: Effect Unit
test = do
  log "ch 22"