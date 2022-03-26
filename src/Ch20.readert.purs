module Ch20ReaderT where

import Data.Identity
import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Console (log)
import Prelude

newtype ReaderT r m a = ReaderT (r -> m a)

{-
Notice ReaderT contains a Monadic Function that returns a Value in the Context of the underlying Monad, m.
Compare that with Reader:

newtype Reader r a = Reader (r -> a)
-}

runReaderT :: ∀ r m a. ReaderT r m a -> (r -> m a)
runReaderT (ReaderT mf) = mf -- The name mf reminds us that it’s a Monadic Function, which means it’s a Function of the form a -> m b (or specifically r -> m a).

instance functorReaderT :: Functor m => Functor (ReaderT r m) where
  map f (ReaderT mg) = ReaderT \r -> f <$> mg r

instance applyReaderT :: Apply m => Apply (ReaderT r m) where
  -- The name fmf reminds us that we have a Function that returns a Function in the underlying Monad.
  -- The name fmx reminds us that we have a Function that returns a Value in the underlying Monad.
  apply (ReaderT fmf) (ReaderT fmx) = ReaderT \r -> fmf r <*> fmx r

instance applicativeReaderT :: Applicative m => Applicative (ReaderT r m) where
  -- pure x = ReaderT \r -> pure x        -- As we don't need `r`
  -- pure x = ReaderT \_ -> pure x        -- The Lambda is now just `const`
  -- pure x = ReaderT $ const $ pure x    -- We can now ETA reduce `x`
  pure = ReaderT <<< const <<< pure
  -- This could actually be replaced by the following, when (below) we implement MonadTrans:
  -- pure = lift <<< pure

instance bindReaderT :: Bind m => Bind (ReaderT r m) where
  bind (ReaderT fmx) f = ReaderT \r -> fmx r >>= \x -> runReaderT (f x) r

instance monadReaderT :: Monad m => Monad (ReaderT r m)

class MonadTrans t where
  lift :: ∀ m a. Monad m => m a -> t m a

instance monadTransReaderT :: MonadTrans (ReaderT r) where
  -- lift mx = ReaderT \r -> mx -- The name mx reminds us that we’re dealing with a Monadic Value, i.e. a Value in a Context
  -- Since we’re not using r, we can eliminate it:
  -- lift mx = ReaderT \_ -> mx
  -- Now we can replace \_ -> mx with const mx:
  -- lift mx = ReaderT $ const mx
  -- Now we can compose the Data Constructor, ReaderT, with const:
  -- lift mx = (ReaderT <<< const) mx
  -- Finally, ETA-reduce
  lift = ReaderT <<< const

class Monad m <= MonadAsk r m | m -> r where -- The | m -> r is the Functional Dependency that states if we know the Monad, m, we can determine the read-only Value, r
  ask :: m r -- `ask` returns the read-only Value in the Context of this Monad. In our case, m is going to be ReaderT r m

instance monadAskReaderT :: Monad m => MonadAsk r (ReaderT r m) where
  ask :: (ReaderT r m) r -- The brackets are actually redundant here, but we keep for clarity
  -- ask = ReaderT \r -> pure r
  -- ETA-reduce \r -> pure r to simply pure:
  ask = ReaderT pure

class Monad m <= MonadTell w m | m -> w where -- Again the functionoal dependency definition is usually put in to help the compiler when there’s ambiguity doing Type Inference - Without this, we’d find ourselves having to be explicit about our Types when we use tell.
  tell :: w -> m Unit -- `tell` takes a Monoid, w, and appends it to its internal log. Notice how it returns a Value of Type Unit. That’s because we’re calling tell for its Side-effect not to have it compute something.

-----------------------------------------------------

test :: Effect Unit
test = do
  log "hi yo"