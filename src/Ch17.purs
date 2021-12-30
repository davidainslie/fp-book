module Ch17 where

import Effect (Effect)
import Effect.Console (log)
import Prelude (Unit, discard)

test :: Effect Unit
test = do
  log "test"