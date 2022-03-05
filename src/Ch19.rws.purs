module Ch19rws where

import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Console (log)
import Prelude

-- Remember:
newtype Reader r a = Reader (r -> a)

newtype Writer w a = Writer (Tuple a w)

newtype State s a = State (s -> Tuple a s)

-- Let's work on combining the above.

type RWSResult r w s = {
  r :: r,
  w :: w,
  s :: s
}

-- As usual, we put the a last so we can make Functor, Apply, Applicative and Monad Instances for the following Type.
newtype RWS r w s a = RWS (RWSResult r w s -> Tuple a (RWSResult r w s))

instance functorRWS :: Functor (RWS r w s) where
  map :: ∀ a b. (a -> b) -> RWS r w s a -> RWS r w s b
  map fab (RWS fn) = RWS \rws -> fn rws # \(Tuple a rr') -> Tuple (fab a) rr'

instance applyRWS :: Monoid w => Apply (RWS r w s) where
  apply :: ∀ a b. RWS r w s (a -> b) -> RWS r w s a -> RWS r w s b
  apply = ap

instance applicativeRWS :: Monoid w => Applicative (RWS r w s) where
  pure :: ∀ a. a -> RWS r w s a
  pure a = RWS \{ r, s } -> Tuple a { r, w: mempty, s }

runRWS :: ∀ r w s a. RWS r w s a -> (RWSResult r w s -> Tuple a (RWSResult r w s))  
runRWS (RWS fn) = fn

instance bindRWS :: Monoid w => Bind (RWS r w s) where
  bind :: ∀ a b. RWS r w s a -> (a -> RWS r w s b) -> RWS r w s b
  bind (RWS g) f = RWS \rws -> g rws # \(Tuple a rws' @ { w }) -> runRWS (f a) rws' # \(Tuple a' rws'' @ { w: w' }) -> Tuple a' rws'' { w = w <> w' }

instance monadRWS :: Monoid w => Monad (RWS r w s)

-- Helper functions that we can call the Monad’s API:

tell :: ∀ r w s. w -> RWS r w s Unit
tell w = RWS \{ r, s } -> Tuple unit { r, w, s }
-- "pushes" data from the Pure Computation to the Monadic one. This is why the a Parameter for RWS is Unit. We’re only calling this function for its Side-effect, which is to send the log.

ask :: ∀ r w s. Monoid w => RWS r w s r
ask = RWS \{ r, s } -> Tuple r { r, w: mempty, s }
-- "pulls" data from the Monadic Computation to the Pure one. This is why the a position of RWS is r. It’s the Value we want to retrieve.

get :: ∀ r w s. Monoid w => RWS r w s s
get = RWS \{ r, s } -> Tuple s { r, w: mempty, s }
-- "pulls" data from the Monadic Computation to the Pure one. This is why the a position of RWS is s. It’s the Value we want to retrieve.

put :: ∀ r w s. Monoid w => s -> RWS r w s Unit
put s = RWS \{ r } -> Tuple unit { r, w: mempty, s }
-- "pushes" data from the Pure Computation to the Monadic one. This is why the a Parameter for RWS is Unit. We’re only calling this function for its Side-effect, which is to send the State.

type Config = { debugModeOn :: Boolean } -- This Record will be our read-only information.

type Counter = Int -- This counter is our State.

-----------------------------------------------------

rwsTest :: RWS Config (Array String) Counter Unit   -- The r is Config, the read-only Value, i.e. our configuration. The w is Array String, the write-only Value, i.e. our log. The s is Counter, our read-write Value, i.e. our State. And finally, the last is Unit since we’re not doing any computation in rwsTest. We’re only interested in testing the Side-effects.
rwsTest = do
  tell ["test the log"]                             -- Append this message to the log.
  tell ["test the log2", "test the log3"]           -- Then append these 2 messages to the log.
  config <- ask                                     -- Retrieve the read-only configuration Record into config.
  tell ["the config is " <> show config]            -- Append a message with the configuration Record Value to the log.
  counter <- get                                    -- Retrieve the current counter Value into counter.
  tell ["old counter is " <> show counter]          -- Append a message with the current counter Value to the log.
  put $ counter + 1                                 -- Increment the counter Value by 1 and save it to the State.
  newCounter <- get                                 -- Retrieve the updated counter Value into newCounter.
  tell ["new counter is " <> show newCounter]       -- Append a message with the new counter Value to the log.
  pure unit                                         -- Set the final result of the Pure Computation.

test :: Effect Unit
test = do
  log $ show $ runRWS rwsTest { r: { debugModeOn: true }, w: mempty, s: 0 }
  -- (Tuple unit { r: { debugModeOn: true }, s: 1, w: ["test the log", "test the log2", "test the log3", "the config is { debugModeOn: true }", "old counter is 0", "new counter is 1"] })