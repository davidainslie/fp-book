-- Monads

module Ch18 where

import Data.Generic.Rep (class Generic)
import Data.Int.Bits ((.&.))
import Data.Monoid.Additive
import Data.Tuple (Tuple(..))
import Data.Show (class Show, show)
import Data.Show.Generic (genericShow)
import Effect (Effect)
import Effect.Console (log)
import Prelude (class Monad, class Bind, bind, (>>=), class Applicative, pure, class Apply, class Functor, class Monoid, mempty, class Semigroup, class Semiring, Unit, unit, discard, const, zero, identity, (#), ($), (+), (<>), (<$>), (<*>), (<), (>), (==))
  
{-
When you interface with the outside world, you’re dealing in the realm of Side-effects.
For example, when you read from a file or call a Web API, the value that’s returned from that Function will NOT be deterministic, i.e. it could be different for 2 different consecutive calls with the same inputs.
-}

{-
Type class Semigroupoid:
(Remember a Category consists of Objects and Morphisms)

class Semigroupoid a where
  compose :: ∀ b c d. a c d -> a b c -> a b d

infixr 9 compose as <<<

WTF?

Compose is really just:
compose :: ∀ a b c. (b -> c) -> (a -> b) -> (a -> c)

-- However, "->" is just sugar syntax for Function:
compose :: ∀ a b c. Function b c -> Function a b -> Function a c

-- Notice that Function appears to be just another data structure.
-- If it works for that data structure, why not generalize it for any data structure?
compose :: ∀ f a b c. f b c -> f a b -> f a c

-- Let's rename our generics, so that it starts with `a` rather than `f`:
compose :: ∀ a b c d. a c d -> a b c -> a b d

Low and behold... Semigroupoid

Let's just look at the Function instance of Semigroupoid:

instance semigroupoidFn :: Semigroupoid (->) where
  compose :: ∀ b c d. (c -> d) -> (b -> c) -> (b -> d)
  compose f g x = f (g x)

- Remember, (->), is the Type Operator for the Type Constructor, Function.
- Everywhere you see -> think of a but prefixed to see how this Type Signature relates to the Semigroupoid one.

-}

composeDebuggable :: ∀ b c d. (c -> Tuple String d) -> (b -> Tuple String c) -> (b -> Tuple String d)
composeDebuggable g f x =
  let Tuple s r = f x
      Tuple s' t = g r in
  Tuple (s <> s') t

f :: Int -> Tuple String Int
f x = Tuple "added 10\n" (x + 10)

g :: Int -> Tuple String Int
g y = Tuple "multiplied by 100\n" (y + 100)

h :: Int -> Tuple String Int
h = g `composeDebuggable` f

{-
The above is fine when all functions are side-effecting as expected by composeDebuggable.

How about composing Side-effect Function with Pure Ones?

Let’s write a Function that converts a Pure Function into a Side-effect one:
-}
makeFuncDebuggable :: ∀ a b. (a -> b) -> a -> Tuple String b
makeFuncDebuggable f x = Tuple "" (f x)

{-
makeFuncDebuggable will lift our Function into one that has a Side-effect,
i.e. it takes a Function that normally returns a Pure Value, b, and makes it return a Side-effect Value, Tuple String b.
-}

nse :: Int -> Int
nse x = x + 42

c :: Int -> Tuple String Int
c = makeFuncDebuggable nse `composeDebuggable` f

-- Function application?
f' :: Int -> Int
f' = (+) 10

g' :: Int -> Int
g' = (+) 100

y :: Int
y = 12345 # f' # g'
-- Rember # is like "apply from" whereas $ is "apply to"

-- We can solve this problem, like we did for composing, by writing a Side-effect version of #. Let’s call it applyDebuggable.

{-
Convert # i.e. applyFlipped to a version for side effects which we'll call applyDebuggable:

applyFlipped :: ∀ a b .a -> (a -> b) -> b
-}

applyDebuggable :: ∀ a b. Tuple String a -> (a -> Tuple String b) -> Tuple String b
applyDebuggable (Tuple s x) f =
  let Tuple s' r = f x in
    Tuple (s <> s') r

-- And so
y' :: Tuple String Int
y' = (12345 # f) `applyDebuggable` g

-- But we can improve this further using:
makeDebuggable :: ∀ a. a -> Tuple String a
makeDebuggable x = Tuple "" x

-- giving:
y'' :: Tuple String Int
y'' = makeDebuggable 12345 `applyDebuggable` f `applyDebuggable` g

{-
One step to being more generic:
-}
type Debuggable a = Tuple String a

composeDebuggable' :: ∀ b c d. (c -> Debuggable d) -> (b -> Debuggable c) -> (b -> Debuggable d)
composeDebuggable' g f x =
  let Tuple s r = f x
      Tuple s' t = g r in
  Tuple (s <> s') t

applyDebuggable' :: ∀ a b. Debuggable a -> (a -> Debuggable b) -> Debuggable b
applyDebuggable' (Tuple s x) f =
  let Tuple s' r = f x in
  Tuple (s <> s') r

makeFuncDebuggable' :: ∀ a b. (a -> b) -> a -> Debuggable b
makeFuncDebuggable' f x = Tuple "" (f x)

makeDebuggable' :: ∀ a. a -> Debuggable a
makeDebuggable' x = Tuple "" x

-- We can rewrite composeDebuggable to reuse applyDebuggable:
composeDebuggable'' :: ∀ b c d. (c -> Debuggable d) -> (b -> Debuggable c) -> (b -> Debuggable d)
composeDebuggable'' g f x =
  f x `applyDebuggable'` g

-- And the same for makeFuncDebuggable:
makeFuncDebuggable'' :: ∀ a b. (a -> b) -> a -> Debuggable b
makeFuncDebuggable'' f x = makeDebuggable' (f x)

------------------------------------------------

{-
Just like Debuggable, we could come up with Countable, where this time the first element of Tuple "counts" (accumulates) and the second polymorphic parameter "a" is the computational result:
type Countable a = Tuple Int a

We would end up implementing the same set of behaviour, where only the details are different - A perfect case to extract a Type Class, which we'll call SideEffect.

A first attempt (which we'll enhance):
class SideEffect s where
  makeSideEffect :: ∀ a. a -> s a -- Which is the same as "pure" from Applicative
  applySideEffect :: ∀ a b. s a -> (a -> s b) -> s b

we'll end up with:  
-}
class Applicative f <= SideEffect f where
  applySideEffect :: ∀ a b. f a -> (a -> f b) -> f b

-- We'll have to make Debuggable and Countable real Types (not just aliases) so that we can make them instances of SideEffect - let's do just Countable:
newtype Count = Count Int

derive newtype instance semiringCount :: Semiring Count -- This will allow us to use (+) next

instance semigroupCount :: Semigroup Count where
  append = (+)

newtype Countable a = Countable (Tuple Count a)
-- We use Count (essentially wrapping Int) instead of just an Int, because Int has multiple semigroups
-- Below, when we derive Apply (for Countable) it implicitly derives from the Apply instance of Tuple (as that is what is being wrapped),
-- and that instance has a Semigroup constraint on the first Tuple entry which would be Int in our case, but which Semigroup instance should be used for Int? Hence we have a newtype around Int to derive our own.

-- We'll also need a Monoid for Count, as the below Applicative for Countable which delegates to Tuple's instance, and the Tuple instance has a constrait of Monoid:
instance monoidCount :: Monoid Count where
  mempty = zero -- Where zero is from Semiring

-- To implement a SideEffect for Countable, we'll need an Applicative, which in turn needs an Apply, which in turn needs a Functor:
derive newtype instance functorCountable :: Functor Countable

derive newtype instance applyCountable :: Apply Countable

derive newtype instance applicativeCountable :: Applicative Countable

instance sideEffectCountable :: SideEffect Countable where
  applySideEffect (Countable (Tuple c x)) f =
    let Countable (Tuple c' r) = f x in
      Countable $ Tuple (c <> c') r -- Or we could have done c + c'

------------------------------------------------

{-
The above SideEffect Type Class is in fact nearly a Monad, which is declared in PureScript using two Type Classes:

class Apply m <= Bind m where
  bind :: ∀ a b. m a -> (a -> m b) -> m b
  
infixl 1 bind as >>=

class (Applicative m, Bind m) <= Monad m

Monad basically allows us to compose functions of the form:
a -> m b

Functions of this type are commonly referred to as Monadic functions or Effectful Functions - Basically functions with side-effects.
-}

------------------------------------------------

{-
A general version of composeDebuggable:
-}
composeKleisli :: ∀ a b c m. Monad m => (b -> m c) -> (a -> m b) -> (a -> m c)
composeKleisli g f x = f x >>= g

infixr 5 composeKleisli as >=>

{-
So bind, Monadic Function Application, lets us write composeKleisli for Monads, just like $, Pure Function Application, lets us write compose for Functions:
-}

------------------------------------------------

-- Can we now show that Debuggable is a Monad? We'll create another version of Debuggable that is a newtype instead of simply a type alias:
newtype Debuggable'' a = Debuggable'' (Tuple String a)

derive newtype instance functorDebuggable'' :: Functor Debuggable''

derive newtype instance applyDebuggable'' :: Apply Debuggable''

derive newtype instance applicativeDebuggable'' :: Applicative Debuggable''

instance bindDebuggable'' :: Bind Debuggable'' where
  bind :: ∀ a b. Debuggable'' a -> (a -> Debuggable'' b) -> Debuggable'' b
  bind (Debuggable'' (Tuple s x)) fab =
    let (Debuggable'' (Tuple s' r)) = fab x in
    Debuggable'' $ Tuple (s <> s') r

------------------------------------------------

-- Maybe Monad

data Maybe a = Nothing | Just a

instance functorMaybe :: Functor Maybe where
  map _ Nothing = Nothing
  map fn (Just x) = Just $ fn x

instance applyMaybe :: Apply Maybe where
  apply Nothing _ = Nothing
  apply (Just fn) x = fn <$> x

instance applicativeMaybe :: Applicative Maybe where
  pure = Just

instance bindMaybe :: Bind Maybe where
  bind :: ∀ a b. Maybe a -> (a -> Maybe b) -> Maybe b
  bind Nothing _ = Nothing
  bind (Just x) fn = fn x

instance monadMaybe :: Monad Maybe
 
-- To show the following tests:
derive instance genericMaybe :: Generic (Maybe a) _

instance showMaybe :: Show a => Show (Maybe a) where
  show :: Maybe a -> String
  show = genericShow

oddTest :: Int -> Maybe Int
oddTest x = if x .&. 1 == 1 then Just x else Nothing

greaterThanTest :: Int -> Int -> Maybe Int
greaterThanTest min x = if x > min then Just x else Nothing

lessThanTest :: Int -> Int -> Maybe Int
lessThanTest max x = if x < max then Just x else Nothing

gauntletUsingKleisliComposition :: Int -> Maybe Int
gauntletUsingKleisliComposition = oddTest >=> pure <<< (_ + 1) >=> greaterThanTest 10 >=> lessThanTest 20

gauntletUsingBind :: Int -> Maybe Int
gauntletUsingBind x =
  pure x >>= oddTest
    >>= \o -> pure (o + 1)
      >>= greaterThanTest 10 -- SAME AS >>= \y -> greaterThanTest 10 y
        >>= \z -> lessThanTest 20 z

-- LOOK OUT - With Bind we could end up going down "indentation hell" - That is when you reach for "Do Notation".

{-
A word on "discard" which can be implicitly applied to a "do" block.

gauntlet :: Int -> Maybe Int
gauntlet x = do
  o <- oddTest x
  let y = o + 1
  z <- greaterThanTest 10 y
  lessThanTest 20 z

gauntlet :: Int -> Maybe Int
gauntlet x = do
  o <- oddTest x
  let y = o + 1
  greaterThanTest 10 y -- COMPILER ERROR!!
  lessThanTest 20 y

We can avoid the compiler error in two ways:

Explicitly discard:
  _ <- greaterThanTest 10 y 

Implicitly discard :
  greaterThanTest 10 y
where we import Prelude discard and also have an instance of Discard for the Value we've discarding.

Actually, there is a third way where we can use the "void" function:
  void $ greaterThanTest 10 y

To achieve the above when "binding", we can ignore a (previous) result by using "*>" instead of ">>=" e.g.

gauntlet :: Int -> Maybe Int
gauntlet x =
  oddTest x
    >>= \o -> pure (o + 1)
      >>= \y -> greaterThanTest 10 y
        *> lessThanTest 20 y -- Where *> is an alias for "applySecond"
-}

------------------------------------------------

-- Either Monad

data Either a b = Left a | Right b

instance functorEither :: Functor (Either a) where
  map _ (Left a)    = Left a
  map fbc (Right b) = Right $ fbc b

instance applyEither :: Apply (Either a) where
  apply (Left a) _ = Left a
  apply (Right fbc) fb = fbc <$> fb

instance applicativeEither :: Applicative (Either a) where
  pure = Right

instance bindEither :: Bind (Either a) where
  bind (Left a) _ = Left a
  bind (Right b) fbmc = fbmc b -- Where fbmc is b -> m c

instance monadEither :: Monad (Either a)   

------------------------------------------------

-- Writer Monad

newtype Writer w a = Writer (Tuple a w)
{-
It would be nicer to have "a" on the right, but this is the historical way
- a: computational
- w: log
-}

instance functorWriter :: Functor (Writer w) where -- We hold the Type "w" constant, i.e. we can only "map" over the computation Type "a"
  map :: ∀ a b. (a -> b) -> Writer w a -> Writer w b
  map fab (Writer (Tuple a w)) = Writer (Tuple (fab a) w)

instance applyWriter :: Monoid w => Apply (Writer w) where
  apply :: ∀ a b. Writer w (a -> b) -> Writer w a -> Writer w b
  apply (Writer (Tuple fab l1)) (Writer (Tuple a l2)) = Writer (Tuple (fab a) (l1 <> l2))

instance applicativeWriter :: Monoid w => Applicative (Writer w) where
  pure :: ∀ a. a -> Writer w a
  pure a = Writer (Tuple a mempty)

instance bindWriter :: Monoid w => Bind (Writer w) where
  bind :: ∀ a b. Writer w a -> (a -> Writer w b) -> Writer w b
  bind (Writer (Tuple a l1)) faw =
    faw a # \(Writer (Tuple b l2)) -> Writer (Tuple b (l1 <> l2))

instance monadWriter :: Monoid w => Monad (Writer w)

{-
Monad API functions acting as Writer helper functions:

tell :: ∀ w. w -> Writer w Unit

listen :: ∀ a w. Writer w a -> Writer w (Tuple a w)

pass :: ∀ a w. Writer w (Tuple a (w -> w)) -> Writer w a
-}
tell :: ∀ w. w -> Writer w Unit
tell l = Writer (Tuple unit l)

doNothingWithLog :: Writer (Array String) Int
doNothingWithLog = do
  tell ["We did nothing"]
  -- Bind performs the magic; the way it threads through the processing is dependent on the how the Monad is implemented. In this case what does Writer do? Well.....
  -- append the preceeding log, i.e. ["We did nothing"]
  -- to the following log, i.e. mempty
  pure 0

-- In do notation there’s an implied >>= between each line.
-- And since >>= is the bind implementation, it’s behavior is hidden from us and appears to be magic. But it’s not.
-- Think of it as a Parallel Computation.  

------------------------------------------------

{-
Parallel Computations

There are 2 Computations that are present in every Monad.
First is the Pure Computation, i.e. the code that you’ve written in the computation, for example:

gauntlet :: Int -> Either String Int
gauntlet x = do
  o <- oddTest x
  let y = o + 1
  void $ greaterThanTest 10 y
  lessThanTest 20 y

The second computation is the Monadic Computation, i.e what the bind implementation is doing. In the above example, it’s checking for errors.
Since the Monad’s bind implementation does this work for us, we’re freed to code without thinking too much about it.

Some common Monads and their Monadic Computations:

Identity  -> No Computation
Maybe     -> Error Checking
Either    -> Error Checking
Writer    -> Log Appending
Reader    -> Threading Read Only Value
State     -> Threading State

Notice what we’ve been calling a Side-effect is really a Parallel Computation, i.e. the Monadic Computation that’s being performed every time we use bind.
-}

------------------------------------------------

-- Reader

newtype Reader r a = Reader (r -> a)

{-
Type Constructor:
r is the Type that the Reader will maintain and pass to every Function for us and a is the result of the Pure Computation.

Data Constructor:
Takes 1 Parameter and that’s a Function with the Type Signature r -> a. This Function takes an r, i.e. some read-only Value and produces an a, i.e. its Pure Computation.
-}

instance functorReader :: Functor (Reader r) where
  map :: ∀ a b. (a -> b) -> Reader r a -> Reader r b
  map fab (Reader fra) = Reader \r -> fab $ fra r -- OR the following alternatives
  -- map fab (Reader fra) = Reader (fra >>> fab)
  -- map fab (Reader fra) = Reader (fab <<< fra)

instance applyReader :: Apply (Reader r) where
  apply :: ∀ a b. Reader r (a -> b) -> Reader r a -> Reader r b
  apply (Reader frf) (Reader fra) = Reader \r -> frf r $ fra r

instance applicativeReader :: Applicative (Reader r) where
  pure :: ∀ a. a -> Reader r a
  -- pure x = Reader \r -> x BUT as r is not used, we can replace with _
  -- pure x = Reader \_ -> x BUT shows that \_ -> x is equivalent to const x, therefore
  -- pure x = Reader $ const x BUT with eta-reduction
  pure = Reader <<< const

{-
The above implementation is a common pattern for Functors that contain Functions i.e. F \a -> b
We also know that Functors map OUTPUTs, so the above Reader implementation first applies the contained function and then the function from "map".
-}

{-
Now for Bind, remembering:

class Apply m <= Bind m where
  bind :: ∀ a b. m a -> (a -> m b) -> m b 
-}
runReader :: ∀ a r. Reader r a -> (r -> a)
runReader (Reader f) = f

instance bindReader :: Bind (Reader r) where
  bind :: ∀ a b. Reader r a -> (a -> Reader r b) -> Reader r b
  bind (Reader fra) far = Reader \r -> runReader (far $ fra r) r

-- Finally, the Monad
instance monadReader :: Monad (Reader r)

-- And now the Reader (helper) API:
ask :: ∀ r. Reader r r -- When the Reader returned by "ask" is run, it'll return the read-only value
ask = Reader identity

asks :: ∀ a r. (r -> a) -> Reader r a
asks fra = Reader \r -> fra r

------------------------------------------------

-- We've done "read", and we've done "write" - But what if we want to both "read" and "write"? Enter State Monad

{-
State Monad
-}

newtype State s a = State (s -> Tuple a s)

{-
Looks a bit like Reader, but Reader does not give back the state it was given, as that state is "read-only" - State monad allows state to be changed while threaded through.
A Function will change the State and return it along with its computation. We’ll take that new State and pass it to the next Function and so on.
So when State is run, it’ll return both the computation of Type a and the new State of Type s.
-}

instance functorState :: Functor (State s) where
  map :: ∀ a b. (a -> b) -> State s a -> State s b
  map f (State fx) = State \s -> fx s # \(Tuple x s') -> Tuple (f x) s'

instance applyState :: Apply (State s) where
  apply :: ∀ a b. State s (a -> b) -> State s a -> State s b
  apply (State ff) (State fx) = State \s -> ff s # \(Tuple g s') -> fx s' # \(Tuple x s'') -> Tuple (g x) s''

{-
Note that implementing "apply" with "ap" would be safer, to make sure ordering of the applied functions where

ap :: ∀ a b m. Monad m => m (a -> b) -> m a -> m b
ap mf mx = do
  f <- mf
  x <- mx
  pure $ f x
-}

instance applicativeState :: Applicative (State s) where
  pure :: ∀ a. a -> State s a
  pure x = State \s -> Tuple x s

runState :: ∀ a s. State s a -> (s -> Tuple a s)
runState (State f) = f

instance bindState :: Bind (State s) where
  bind :: ∀ a b. State s a -> (a -> State s b) -> State s b
  bind (State fx) f = State \s -> fx s # \(Tuple x s') -> runState (f x) s'

instance monadState :: Monad (State s)

-- State API:

get :: ∀ s. State s s
get = State \s -> Tuple s s

put :: ∀ s. s -> State s Unit
put s = State \_ -> Tuple unit s

modify :: ∀ s. (s -> s) -> State s s
modify f = State \s -> let ns = f s in Tuple ns ns

modify_ :: ∀ s. (s -> s) -> State s Unit
modify_ f = State \s -> Tuple unit (f s)

------------------------------------------------

-- Use State as a Monadic Validation

-- The original version (solution) using Either, will short-circuit i.e. stop the first time we get a Left
-- First function errIfMissing is named errIfMissing' because we are going to rewrite it

errIfMissing' :: Maybe String -> String -> Either String String
errIfMissing' Nothing err = Left err
errIfMissing' (Just s) _ = Right s

fullName :: String -> String -> String -> String
fullName first middle last = first <> " " <> middle <> " " <> last

fullNameEither :: Maybe String -> Maybe String -> Maybe String -> Either String String
fullNameEither first middle last =
  fullName <$> (first `errIfMissing'` "First name must exist")
           <*> (middle `errIfMissing'` "Middle name must exist")
           <*> (last `errIfMissing'` "Last name must exist")

-- OR using "do" notation:
fullNameEither' :: Maybe String -> Maybe String -> Maybe String -> Either String String
fullNameEither' first middle last = do
  f <- errIfMissing' first "First name must exist"
  m <- errIfMissing' middle "Middle name must exist"
  l <- errIfMissing' last "Last name must exist"
  pure $ fullName f m l

-- Rewrite using State - Start by converting errIfMissing to use State instead of Either

errIfMissing :: Maybe String -> String -> State (Array String) String
errIfMissing Nothing err = do
  modify_ (_ <> [err])
  pure mempty
errIfMissing (Just s) _ =
  pure s

fullNameValid :: Maybe String -> Maybe String -> Maybe String -> State (Array String) String
fullNameValid first middle last = do
  f <- errIfMissing first "First name must exist"
  m <- errIfMissing middle "Middle name must exist"
  l <- errIfMissing last "Last name must exist"
  pure $ fullName f m l

-- BUT we didn't actually need to use State in errIfMissing - It uses "append" which comes from Semigroup.
-- We could replace State with Writer to have:
errIfMissing'' :: Maybe String -> String -> Writer (Array String) String
errIfMissing'' Nothing err = do
  tell [err] -- The only required change moving from State to Writer
  pure mempty
errIfMissing'' (Just s) _ =
  pure s

-- Now, all we have to do is change the Type Signature on fullNameValid. Since both State and Writer are Monads, all of our code inside the do block doesn’t change.
-- That’s because the compiler will use Writer’s Methods, i.e. bind and pure, instead of State’s Methods:
fullNameValid' :: Maybe String -> Maybe String -> Maybe String -> Writer (Array String) String
fullNameValid' first middle last = do
  fn <- errIfMissing'' first "First name must exist"
  mn <- errIfMissing'' middle "Middle name must exist"
  ln <- errIfMissing'' last "Last name must exist"
  pure $ fullName fn mn ln

-- Just like State has a "runState" we'll need an equivalent for Writer, which takes 1 less parameter (see the test usage below)
runWriter :: ∀ a w. Writer w a -> Tuple a w
runWriter (Writer x) = x  

------------------------------------------------

-- Kleisli Category
-- Remember, Kleisli composition composes Monadic functions of form a -> m b
-- See above definition of:
-- composeKleisli

-- along with composeKleisli we have:

class Semigroupoid :: ∀ k. (k -> k -> Type) -> Constraint
class Semigroupoid a where
  compose :: ∀ b c d. a c d -> a b c -> a b d

infixr 9 compose as <<<

instance semigroupoidFn :: Semigroupoid (->) where
  compose :: ∀ b c d. (c -> d) -> (b -> c) -> (b -> d)
  compose cd bc x = cd (bc x)

------------------------------------------------

test :: Effect Unit
test = do
  log $ show y  
  log $ show y''
  log "-----------------------------"
  log $ show $ gauntletUsingKleisliComposition 14
  log $ show $ gauntletUsingKleisliComposition 1
  log $ show $ gauntletUsingKleisliComposition 93
  log $ show $ gauntletUsingKleisliComposition 17
  log "-----------------------------"
  log $ show $ gauntletUsingBind 14
  log "-----------------------------"
  log $ show $ runState (fullNameValid Nothing (Just "") Nothing) []
  log "-----------------------------"
  log $ show $ runWriter (fullNameValid' Nothing (Just "") Nothing)