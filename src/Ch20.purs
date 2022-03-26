module Ch20 where

import Data.Identity
import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Console (log)
import Prelude

{-
Monads do not compose - we can see this by comparing "bind" for 2 different Monads:

bind :: ∀ a b. Reader r a -> (a -> Reader r b) -> Reader r b

bind :: ∀ a b. State s a -> (a -> State s b) -> State s b
-}

newtype ReaderT r m a = ReaderT (r -> m a)

newtype StateT s m a = StateT (s -> m (Tuple a s))

newtype WriterT w m a = WriterT (m (Tuple a w))

type Nested r s w a = ReaderT r (StateT s (WriterT w Identity)) a

-- Can now redeclare Monads from their Transformers.
-- We can declare Reader as:
-- type Reader r a = ReaderT r Identity a

-- But we can ETA reduce
type Reader r = ReaderT r Identity

type Writer w = WriterT w Identity

type State s = StateT s Identity

{-
Scala sidebar

type Reader[E, A] = ReaderT[Id, E, A]

type Writer[W, A] = WriterT[Id, W, A]

type State[S, A] = StateT[Id, S, A]
-}

{-
But how do we take a StateT e.g. and wrap it into a ReaderT?

If we have the following monad stack:
ReaderT
StateT
WriterT
Identity

let's say we wish to "lift" WriterT into StateT. We can imagine having "lift" functions such as:

liftIntoStateT :: ∀ s m a. Monad m => m a -> StateT s m a
This imaginary Function takes any Monad, m a, and lifts it into StateT

liftIntoReaderT :: ∀ r m a. Monad m => m a -> ReaderT r m a
This imaginary Function takes any Monad, m a, and lifts it into ReaderT

We should stop right here and make a Typeclass for this since we have the classic condition for a Typeclass,
i.e. same Function for multiple Types.
-}

-- We’ll call it MonadTrans to represent Monads that can transform other Monads:
class MonadTrans t where
  lift :: ∀ m a. Monad m => m a -> t m a

{-
where the above "t" could be:
- ReaderT r
- WriterT w
- StateT s
etc.

So Monad Transformers follow the general form of:
t m a
where
- t is the Monad Transformer
- m is the underlying Monad
- a is the Pure Value

giving e.g.
- ReaderT r m a
- WriterT w m a
- StateT s m a
- ExceptT e m a -- This one is for handling errors. For complicated reasons, this is essentially EitherT
-}

-- The following are the Typeclass definitions for many of the more common APIs:
class Monad m <= MonadAsk r m | m -> r where
  ask :: m r

-- Notice the Functional Dependency, | m -> w that says that for each Monad, m, there can be ONE AND ONLY ONE w. As is goes for the others
class Monad m <= MonadTell w m | m -> w where
  tell :: w -> m Unit

class Monad m <= MonadState s m | m -> s where
  state :: ∀ a. (s -> (Tuple a s)) -> m a

class Monad m <= MonadThrow e m | m -> e where
  throwError :: ∀ a. e -> m a

class MonadThrow e m <= MonadError e m | m -> e where
  catchError :: ∀ a. m a -> (e -> m a) -> m a

{-
The following are many of the more common helper API Functions:

asks :: ∀ r m a. MonadAsk r m => (r -> a) -> m a

get :: ∀ m s. MonadState s m => m s
put :: ∀ m s. MonadState s m => s -> m Unit

modify :: ∀ s m. MonadState s m => (s -> s) -> m s
modify_ :: ∀ s m. MonadState s m => (s -> s) -> m Unit
-}

-----------------------------------------------------

test :: Effect Unit
test = do
  log "hi"