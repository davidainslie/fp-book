-- Effects

module Ch23b where

import Control.Monad.Reader.Trans (ReaderT, runReaderT, ask)
import Control.Monad.Rec.Class (forever)
import Control.Monad.State.Trans (StateT, runStateT)
import Control.Monad.Trans.Class (lift)
import Data.Either (Either(..))
import Data.Tuple (Tuple)
import Effect.Aff (Aff, forkAff, launchAff_, makeAff, nonCanceler)
import Effect.Aff.Bus as Bus
import Effect.Aff.Bus (BusRW)
import Effect.Class (liftEffect)
import Effect (Effect)
import Effect.Class.Console (log)
import Effect.Random (random)
import Prelude

{-
Here we will run separate Fibers for each Publisher and Subscriber in a Monad Stack that contains ReaderT and StateT. Our Stack will have Aff at the base since we’re running in a Fiber.
Publishers will produce Random Numbers delaying 1 second before doing so. Then they will publish a String message onto a Bus when their randomly generated number passes some Predicate.
The Publisher will then countdown a count in the State and continue only if the count is positive.

There will be 3 Publishers.
One will check for values over 0.5.
Another will check for values less than 0.5.
The final one will check to see if the value is over 0.1.

Subscribers will listen to the Bus and log the data they get from the Bus to the console. There will be 1 Subscriber.

This (above) spec (like others) main raise questions which can be called `Technological Hurdles` e.g.
- How to create a Random Number
- How to make a Monad Stack (StateT and ReaderT)
    - A read-only structure will be passed to all Fibers that contains the read-write Bus.
    - Each Fiber will have a State that contains a count of how many Random Numbers to generate before exiting.
- How to create a Fiber
- How to run a Monad Stack in a Fiber
- How to create a Bus
- How to Publish to a Bus
- How to Subscribe to a Bus
-}

{-
There is a function to get a random number, but it would be within an `Effect` and the steps outlines above would be in `Aff`:
random :: Effect Number

We could somehow make use of:
makeAff :: ∀ a. ((Either Error a -> Effect Unit) -> Effect Canceler) -> Aff a

where, the first Parameter is a Function, and that Function’s first Parameter is another Function.

So keep in mind this type alias as a simplification:
type Callback a = Either Error a -> Effect Unit

For first Parameter’s return Type we can use `nonCanceler`.
-}

randomAff :: Aff Number
randomAff = makeAff \cb -> do -- We know that makeAff takes a single Parameter that’s a Function. And that Function takes a single Parameter, which we determined is a Callback Function.
  n <- random                 -- We can call random here since we coding in the Effect Monad in this do block
  cb $ Right n                -- `random` will not error, so we har code Right. If we were to handle an error we could use:
                              -- cb $ Left $ error "Something went horribly awry"
                              -- where `error` is defined as: error :: String -> Error
  pure nonCanceler            -- We know that our Lambda is going to return an Effect Canceler so that means that our code is going to run in the Effect Monad.

{-
The above can be simplified to:

randomAff :: Aff Number
randomAff = liftEffect random
-}

{-
So, looking back over this code, we can see that we provide makeAff with a computation in the Effect Monad that it’ll run in Aff for us.
That computation calls random and returns the successful result, via the Callback Function that makeAff passed to us.
-}

type Config = { bus :: BusRW String }
{-
BusRW is a bidirectional bus, i.e. we can read and write to it.
String is the Type of the Values that will be written to the Bus.
-}

-- A Type Alias for the State called State that contains our count.
type State = { count :: Int }

{-
A Type Alias called FiberM, that represents the Monad Stack that our Fibers will run in.
Put ReaderT on the top of the Stack and Aff on the bottom of the Stack.
-}
type FiberM a = ReaderT Config (StateT State Aff) a
{-
A nested type that represents a Monad Stack is read (stacked from top to bottom) by reading left to right.
So the leftmost Monad is ReaderT (on top of the stack),
then it is StateT,
and finally (at the bottom) at the bottom of the stack is Aff.

And in order to run, we again progress top -> bottom i.e. left -> right
i.e. we would run ReaderT first.
-}

{-
To create a fiber, there is:
launchAff :: ∀ a. Aff a -> Effect (Fiber a)
and
forkAff :: ∀ a. Aff a -> Aff (Fiber a)

i.e. we know how to create a Fiber in either Effect or Aff.
-}

runFiberM :: BusRW String -> FiberM Unit -> Aff Unit
runFiberM bus =
  void <<< forkAff
  <<< flip runStateT { count: 10 }  -- Pass in our read-write State
  <<< flip runReaderT { bus }       -- We pass a Config that contains our bus

{-
When running the above, remember that runReaderT returns it’s underlying Monad i.e.
runReaderT :: ∀ r m a. ReaderT r m a -> r -> m a

What is `m`?
                ReaderT   r            m          a
type FiberM a = ReaderT Config (StateT State Aff) a

We see that `m` is:
StateT State Aff

As runReaderT’s return Type, i.e. m a, is StateT State Aff, we next had to run StateT
-}

liftAffToFiberM :: Aff ~> FiberM -- Equivalent to:
-- liftAffToFiberM :: ∀ a. Aff a -> FiberM a
liftAffToFiberM = lift <<< lift
-- Lift twice - With our Stack, we’re first lifting the base Monad, Aff, to StateT and then ReaderT

logger :: FiberM Unit
logger = forever do
  { bus } <- ask
  s <- liftAffToFiberM $ Bus.read bus
  log $ "Logger: " <> s

randomGenerator :: (Number -> Boolean) -> FiberM Unit
randomGenerator pred = pure unit
-- TODO - Did not finish this implementation on p.1044, and so did not finish chapter 23.3.

-----------------------------------------------------

-- Run: npx spago run --main Ch23b
main :: Effect Unit
main = launchAff_ do
  bus <- Bus.make
  let forkFiberM = runFiberM bus
  forkFiberM logger
  forkFiberM $ randomGenerator (_ > 0.5)
  forkFiberM $ randomGenerator (_ < 0.5)
  forkFiberM $ randomGenerator (_ > 0.1)