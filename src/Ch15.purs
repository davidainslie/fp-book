-- Coding Functors

module Ch15 where

import Effect
import Effect.Console (log)
import Prelude (Unit)

test :: Effect Unit
test = do
  log "test"