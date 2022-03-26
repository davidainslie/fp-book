-- Code StateT

module Ch21 where

import Control.Monad.Error.Class (class MonadError, class MonadThrow, throwError, catchError)
import Control.Monad.Reader.Class (class MonadAsk, ask)
import Control.Monad.State.Class (class MonadState)
import Control.Monad.Trans.Class (class MonadTrans, lift)
import Control.Monad.Writer.Class (class MonadTell, tell)
import Data.Identity
import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Console (log)
import Prelude

newtype State s a = State (a -> Tuple a s)

newtype StateT s m a = StateT (s -> m (Tuple a s))

runStateT :: ∀ s m a. StateT s m a -> (s -> m (Tuple a s)) -- The redundant Parentheses can help communicate that we’re returning a Function
runStateT (StateT f) = f

instance functorStateT :: Functor m => Functor (StateT s m) where
  map :: ∀ a b. (a -> b) -> StateT s m a -> StateT s m b
  -- The name mg is to remind us that we have a Monadic Function, i.e. it will return a Pure Value in a Monadic Context.
  map f (StateT mg) = StateT \s -> (mg s) <#> \(Tuple x s') -> Tuple (f x) s' -- <#> is the flipped version of <$> aka "map"

-- To help understand the above - here are the types of the variables in scope of "map"
-- s :: s
-- f :: a -> b
-- mg :: s -> m (Tuple a s)

instance applyStateT :: Monad m => Apply (StateT s m) where
  apply :: ∀ a b. StateT s m (a -> b) -> StateT s m a -> StateT s m b
  apply (StateT fmf) (StateT fmx) = -- fmf: Function that returns Function in a Monad, fmx: Function that returns Value in a Monad
    StateT \s -> do
      Tuple f s'  <- fmf s
      Tuple x s'' <- fmx s'
      pure $ Tuple (f x) s''

-- To help understand the above - here are the types of variables in scope of "apply"
-- fmf :: s -> m (Tuple (a -> b) s)
-- fmx :: s -> m (Tuple a s)

instance applicativeStateT :: Monad m => Applicative (StateT s m) where
  pure x = StateT \s -> pure $ Tuple x s

instance bindStateT :: Monad m => Bind (StateT s m) where
  bind :: ∀ a b. StateT s m a -> (a -> StateT s m b) -> StateT s m b
  -- The name fmx reminds us that we have a Function that will return a Monadic value
  bind (StateT fmx) f = StateT \s -> fmx s >>= \(Tuple x s') -> runStateT (f x) s'

{-
To help understand the above, let's say we started our implemenation as:
bind (StateT fmx) f = StateT \s -> ???

So we HAVE:
fmx :: s -> m (Tuple a s)
f :: a -> StateT s m b
s :: s
\s -> ??? :: s -> m (Tuple b s)

We notice that COMPUTED:
fmx s :: m (Tuple a s)

And we WANT:
StateT \s -> ??? :: StateT s m b
-}

instance monadStateT :: Monad m => Monad (StateT s m)

{-
MonadState instance for StateT, where MonadState is defined as:

class Monad m <= MonadState s m | m -> s where
  state :: ∀ a. (s -> Tuple a s) -> m a
-}

instance monadStateStateT :: Monad m => MonadState s (StateT s m) where
  -- state f = StateT \s -> pure $ f s        -- 1
  -- state f = StateT \s -> (pure <<< f) s    -- 2
  -- Finally ETA reduce:
  state f = StateT $ pure <<< f

{-
To help understand the above, the types are:
f :: s -> Tuple a s
s :: s
f s :: Tuple a s
-}

{-
How did we get from 1 to 2?
Remember:
compose :: ∀ b c d. (c -> d) -> (b -> c) -> (b -> d)
compose f g x = f (g x)

Move the x Parameter to the right-hand side of the equal sign as a Lambda:
compose :: ∀ b c d. (c -> d) -> (b -> c) -> (b -> d)
compose f g = \x -> f $ g x

We can then imagine this becoming:
f <<< g = \x -> f $ g x

and moving the x back to the left-hand side:
(f <<< g) x = f $ g x

Replacing back f with pure,
g with f,
x with s

we have:
(pure <<< f) s = pure $ f s
-}

-- Below, both MonadAsk and MonadTell can use "liftStateT" if we didn't use the already provide "lift" from MonadTrans:
{-
liftStateT :: ∀ s m a. Functor m => m a -> StateT s m a
liftStateT mx = StateT \s -> mx <#> \x -> Tuple x s

Knowing that:
class MonadTrans t where
  lift :: ∀ m a. Monad m => m a -> t m a

Instead of the above, we'll provide a StateT instance of MonadTrans:
-}
instance monadTransStateT :: MonadTrans (StateT s) where
  lift mx = StateT \s -> mx <#> \x -> Tuple x s

{-
MonadAsk instance for StateT, where MonadAsk is defined as:

class Monad m <= MonadAsk r m | m -> r where
  ask :: m r
-}

-- Without liftStateT
{-
instance monadAskStateT :: MonadAsk r m => MonadAsk r (StateT s m) where
  ask :: StateT s m r
  ask = StateT \s -> ask <#> \r -> Tuple r s
-}

-- With liftStateT
instance monadAskStateT :: MonadAsk r m => MonadAsk r (StateT s m) where
  -- ask = liftStateT ask -- If we used our custom liftStateT instead of lift
  ask = lift ask

{-
MonadTell instance for StateT, where MonadTell is defined as:

class Monad m <= MonadTell w m | m -> w where
  tell :: w -> m Unit
-}

-- Without liftStateT
{-
instance monadTellStateT :: MonadTell r m => MonadTell r (StateT s m) where
  tell w = StateT \s -> tell w <#> \_ -> Tuple unit s
-}

-- With liftStateT
instance monadTellStateT :: MonadTell r m => MonadTell r (StateT s m) where
  -- tell w = liftStateT (tell w) -- If we used our custom liftStateT instead of lift
  -- tell = liftStateT <<< tell   -- changing above to point free
  tell = lift <<< tell

{-
MonadThrow instance for StateT, where MonadThrow is defined as:

class Monad m <= MonadThrow e m | m -> e where
  throwError :: ∀ a. e -> m a
-}

instance monadThrowStateT :: MonadThrow e m => MonadThrow e (StateT s m) where
  throwError = lift <<< throwError

{-
MonadError instance for StateT, where MonadError is defined as:

class MonadThrow e m <= MonadError e m | m -> e where
  catchError :: ∀ a. m a -> (e -> m a) -> m a
-}

instance monadErrorStateT :: MonadError e m => MonadError e (StateT s m) where
  catchError :: ∀ a. StateT s m a -> (e -> StateT s m a) -> StateT s m a
  -- The name fmx is to remind us that we have a Function that returns a Value in the underlying Monad.
  -- f is the Function that will be called to handle the error. It gets the error, e and returns a StateT which "replaces" the StateT that failed.
  catchError (StateT fmx) f = StateT \s -> catchError (fmx s) \e -> runStateT (f e) s

{-
To help understand the above, the types are:
fmx :: s -> m (Tuple a s)
f   :: e -> StateT s m a

f e :: StateT s m a
-}

-----------------------------------------------------

test :: Effect Unit
test = do
  log "ch 21"