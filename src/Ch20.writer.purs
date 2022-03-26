module Ch20Writer where

import Data.Identity
import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Console (log)
import Prelude

newtype WriterT w m a = WriterT (m (Tuple a w))

{-
Notice the extra Type Parameter m when compared with our Writer implementation:

newtype Writer w a = Writer (Tuple a w)

That’s because WriterT’s results are now within the Context of its underlying Monad.

How to "run" the Transformer?
With Writer we simply return a `Tuple a w` i.e. the `pure computation` and `the log` - But now we have to consider the underlying Monad.
And remember, the `underlying Monad` will be run AFTER WriterT, so we'll need to return the results in the underlying Monad's context.
So, we would want:

runWriterT :: ∀ w m a. WriterT w m a -> m (Tuple a w)

where the typical Writer return result `Tuple a w` is in the underlying Monadic context `m`.
-}

runWriterT :: ∀ w m a. WriterT w m a -> m (Tuple a w)
runWriterT (WriterT mx) = mx -- `mx` reminds us that we have a Value in a Monadic Context - We know this by looking at WriterT’s Data Constructor.

instance functorWriterT :: Functor m => Functor (WriterT w m) where
  map f (WriterT mx) = WriterT $ mx <#> \(Tuple x w) -> Tuple (f x) w -- Using the flipped version of <$>, i.e. <#>. This reads better when the Lambda is on the right-hand side.

instance applyWriterT :: (Semigroup w, Monad m) => Apply (WriterT w m) where
  apply (WriterT mf) (WriterT mx) = WriterT do -- mf reminds us that a Function is inside the underlying Monad. And mx reminds us that a Value is inside the underlying Monad.
    Tuple f w1 <- mf
    Tuple x w2 <- mx
    pure $ Tuple (f x) (w1 <> w2)

instance applicativeWriterT :: (Monoid w, Monad m) => Applicative (WriterT w m) where
  pure x = WriterT $ pure $ Tuple x mempty

instance bindWriterT :: (Semigroup w, Monad m) => Bind (WriterT w m) where
  bind :: ∀ a b. WriterT w m a -> (a -> WriterT w m b) -> WriterT w m b
  bind (WriterT mx) f = WriterT do -- We destructure to get at the underlying Monadic Value, mx. Remember that f is a Monadic Function, i.e. it will return a WriterT.
    Tuple x w1 <- mx
    Tuple y w2 <- runWriterT $ f x
    pure $ Tuple y $ w1 <> w2

instance monadWriterT :: (Monoid w, Monad m) => Monad (WriterT w m)

-----------------------------------------------------

test :: Effect Unit
test = do
  log "hi there"