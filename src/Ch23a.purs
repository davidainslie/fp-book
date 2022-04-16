-- Effects

module Ch23a where

import Data.Time.Duration (Milliseconds(..))
import Effect.Aff (Aff, delay, launchAff_, forkAff, killFiber, joinFiber)
import Effect.Aff.AVar (AVar)
import Effect.Aff.AVar as AVar
import Effect (Effect)
import Effect.Class.Console (log)
import Effect.Exception (error)
import Prelude

{-
We are going to have 3 Fibers.
One that’s half a clock, i.e. the Tick part of the clock.
Another that is the second half of the clock, i.e the Tock part. They will communicate via an AVar.
And the third is a bomb that waits for a specified number of Tick-Tock combinations before detinating with a BOOM!! message to the console.

There will be a single `AVar` named `ttAVar`.

The Tick Fiber will look at ttAVar once a second and if it’s Value is tock, then it’ll change it to tick. The Tock Fiber will do the opposite.

THe Bomb Fiber will check ttAVar every 500 milliseconds to see if the time has changed from tick to tock.
When it does, it’ll consider that a count down and once it reaches the specified number, it’ll detinate and our program will exit.
-}

data TickTock = Tick | Tock

{-
- Read the ttAVar
- Delay 1 second
- If it’s Tock then change it to Tick otherwise leave it alone
- Recurse
Watch out - this function is endless, so we would need to `killFiber`.
-}
tick :: AVar TickTock -> Aff Unit
tick ttAVar = do
  -- tt <- AVar.take ttAVar                                             -- Originally we needed the current value because we thought we needed it, but the changes below show we don't need it, so we change this line to:
  void $ AVar.take ttAVar
  delay (Milliseconds 1000.0)
  -- if tt == Tock then AVar.put Tick ttAVar else AVar.put tt ttAVar    -- We change this to the following by flipping the parameters to AVar.put
  -- flip AVar.put ttAVar if tt == Tock then Tick else tt               -- However, this is also rubbish because if ttAVar is Tick we just put it back "as is". So we'll improve again:
  AVar.put Tick ttAVar                                                  -- We actually don't care about the current value, since this function will always end up putting in a Tick
  tick ttAVar                                                           -- And recurse...

tock :: AVar TickTock -> Aff Unit
tock ttAVar = do
  void $ AVar.take ttAVar
  delay (Milliseconds 1000.0)
  AVar.put Tock ttAVar
  tock ttAVar

-- We can combine the above 2 fibers into 1 e.g.
clock :: AVar TickTock -> Aff Unit
clock ttAVar = do
  void $ AVar.take ttAVar
  delay (Milliseconds 1000.0)
  AVar.put Tock ttAVar
  void $ AVar.take ttAVar
  delay (Milliseconds 1000.0)
  AVar.put Tick ttAVar
  clock ttAVar

data BombState = WaitingTick | WaitingTock

derive instance eqTickTock :: Eq TickTock -- Required as we will be performing a TickTock equality check in the `bomb` function.

{-
- If count = detination count then log "BOOM!!" and exit Fiber
- Delay 500 milliseconds
- If waiting for Tick and ttAVar is Tick then wait for Tock.
- If waiting for Tock and ttAVar is Tock then increment count
- Recurse
Note we're going to have to keep some state i.e. `current count` and `BombState`, but we won't pass this in, so we'll need a nested/inner/local `go` function.
-}
bomb :: AVar TickTock -> Int -> Aff Unit
bomb ttAVar detinationCount = go 0 WaitingTick where
  go :: Int -> BombState -> Aff Unit
  go count state = do
    if count == detinationCount then
      log "BOOM!!"
    else do
      delay (Milliseconds 500.0)
      tt <- AVar.read ttAVar
      case state of
        WaitingTick ->
          if tt == Tick then
            log "Tick" *> go count WaitingTock
          else
            go count state
        WaitingTock ->
          if tt == Tock then
            log "Tock" *> go (count + 1) WaitingTick
          else
            go count state

-----------------------------------------------------

-- Run: npx spago run --main Ch23a
main :: Effect Unit
main = launchAff_ do
  ttAVar <- AVar.empty
  clockFiber <- forkAff $ clock ttAVar
  bombFiber <- forkAff $ bomb ttAVar 3
  AVar.put Tick ttAVar
  joinFiber bombFiber
  killFiber (error "Exploded") clockFiber