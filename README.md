# FP Made Easier with PureScript

```shell
npm install -g purescript

npm install -g spago
```

```shell
# Build e.g. after changes to spago.dhall
npx spago build

# Run
npx spago run

# REPL
npx spago repl
```

## REPL Example

```shell
npx spago repl

> import Ch7aa

> :type Left "left"
forall (t2 :: Type). Either String t2

> :t Left "left"
forall (t2 :: Type). Either String t2

> :t Right (Just 42)
forall (t1 :: Type). Either t1 (Maybe Int)

-- Load a module and run a function:
> import Ch11FoldableTree
> test
(5 : -1 : 14 : 99 : Nil)

> ^D # i.e. control-D to exit REPL
See ya!
()
```

## Math

#### Commutative

```purescript
a * b = b * a
```

#### Church Numerals in Lambda Calculus

A way to represent Numbers using Lambda Notation:

```purescript
0 = λf.λx.x
1 = λf.λx.f x
2 = λf.λx.f (f x)
3 = λf.λx.f (f (f x))
```

Basically, the number is represented by how many times a function f is applied to x. In the zero case, it’s applied 0 times.
In the one case, it’s applied 1 time and so on.
This may be easier to see using PureScript Notation:

```purescript
0 = \f x -> x
1 = \f x -> f x
2 = \f x -> f (f x)
3 = \f x -> f (f (f x))
```

We can also represent this same idea with a Data Type in PureScript:

```purescript
data Nat = Zero | Succ Nat

-- Nat stands for Natural Number
```

Here’s how we can represent numbers:

```purescript
0 = Zero
1 = Succ Zero
2 = Succ (Succ Zero)
3 = Succ (Succ (Succ Zero))
```

Notice that the function f from the Lambda Calculus’s encoding is our Succ Data Constructor and the x in Church Numerals is our Zero.

## Category Theory

Definition of a Category:
> A Category consists of a Set of Objects and a Set of Morphisms between the Objects.

We get to pick what our Objects and Morphisms are, which depends on what we’re trying to model. Since our interest is Programming, we’ll pick our Objects as Types and our Morphisms as Functions.
This Category is traditionally called Hask after Haskell. But works just fine for PureScript.

And as always in Math, there are laws:
- Each Morphism f has a Source Object a and Target Object b and is written like f: a → b and we say "f is a morphism from a to b"
- Morphisms compose
- There is an Identity Morphism for every Object, i.e. 1<sub>x</sub>: x → x
- Composition is Associative: if f: a -> b, g: b -> c, and h: c -> d then h ∘ (g ∘ f) = (h ∘ g) ∘ f

## Functor

```purescript
f :: ∀ a b. a -> b
```

While `Morphisms` are mappings between `Objects`, `Functors` are mappings between `Categories`.

Functors that map back to the SAME Category are known as `EndoFunctors`.
So all Functors in PureScript are really `EndoFunctors` but everyone just says `Functors`.

> Functors preserve Categorical Structure.
> In Abstract Algebra, functions that preserves the Algebraic Structure of say a Group are called Homomorphism (Homo means same and morph means shape).
> Here Functors are like Homomorphisms since they too preserve the Categorical Structure.

```purescript
class Functor f where
  map :: ∀ a b. (a -> b) -> (f a -> f b)

infixl 4 map as <$>

-- e.g.
instance functorMaybe :: Functor Maybe where
  map _ Nothing = Nothing
  map f (Just x) = Just $ f x

instance functorEither :: Functor (Either a) where
  map _ (Left x) = Left x
  map f (Right y) = Right $ f y  
```

NOTE about Either instance. One would initially think that the instance would be defined as:
`instance functorEither :: Functor Either where`

but that does not compile because the `Kind`s wont's match.
Comparing the Functor definition and Either the kinds don't align:
```purescript
f :: Type -> Type

Either :: Type -> Type -> Type
```

We get around this by `partially applying` types to either.
Just like functions, we can partially apply Type Constructors which are function like:
```purescript
f :: Type -> Type

Either a :: Type -> Type
```

Left has to be constant for the Functor instance of Either as the following would not work:
```purescript
instance functorEither :: Functor (Either a) where
  map f (Left x) = Left $ f x -- COMPILER ERROR!!
  map f (Right y) = Right $ f y
```

Above we have fixed the Left and it will actually be `a` coming into the Right according to the Functor declaration i.e. `f a`.
It might be easier to think of the following code (for illustration purposes):

```purescript
class Functor f where
  map :: ∀ right result. (right -> result) -> (f right -> f result) -- i.e. fn is a right -> result

instance functorEither :: Functor (Either left) where
  map fn (Left left) = Left $ fn left -- COMPILER ERROR!! fn expects a right 
  map fn (Right right) = Right $ fn right  
```

#### Functor Laws

```purescript
-- Identity
map identity = identity
-- The Identity law says that if we map the identity function, then it’s equivalent to just calling identity on the Functor.
-- Notice that this law means that map can NEVER change the structure of the Functor. 

-- Composition
map (g <<< f) = map g <<< map f
-- The Composition law says that mapping the Composition of 2 functions is equivalent to mapping each and Composing the results.
```

#### Proof

Use `Maybe` to prove:
```purescript
data Maybe a = Nothing | Just a

instance functorMaybe :: Functor Maybe where
  map _ Nothing = Nothing
  map f (Just x) = Just $ f x

-- Identity proof for Nothing:
map identity Nothing = identity Nothing
-- Substitute left side
Nothing = identity Nothing
-- Substitute right side
Nothing = Nothing 

-- Identity proof for Just:
 map identity (Just x) = identity (Just x)
 -- Substitute left side
 Just (identity x) = identity (Just x)
 -- Function application
 Just x = identity (Just x)
 -- Function application
 Just x = Just x

 -- Composition for Nothing:
 map (g <<< f) Nothing = (map g <<< map f) Nothing
 -- Substitute left side
 Nothing = (map g <<< map f) Nothing
 -- Function composition
 Nothing = map g (map f Nothing)
 -- Substitute right side
 Nothing = map g Nothing
 -- Substitute right side
 Nothing = Nothing

 -- Composition for Just:
 map (g <<< f) (Just x) = (map g <<< map f) (Just x)
 -- Substitute left side
 Just ((g <<< f) x) = (map g <<< map f) (Just x)
 -- Function composition
 Just (g (f x)) = (map g <<< map f) (Just x)
 -- Function composition
 Just (g (f x)) = map g (map f (Just x))
 -- Substitute right side
 Just (g (f x)) = map g (Just (f x))
 -- Substitute right side
 Just (g (f x)) = Just (g (f x))
```

## Bifunctor

The Functor instance of Either `short-circuits`. We are forced to short circuit by Functor definition and partially applying our types.

But what if we wanted a `non short-circuit` version of Either e.g.

```purescript
data Choice a b = PickA a | PickB b
```

As mentioned, Functor forces our hand on all but the last Type parameter. We again have to short-circuit when we don't want to e.g.:
```purescript
instance functorChoice :: Functor (Choice a) where
  map _ (PickA x) = PickA x
  map f (PickB y) = PickB $ f y
```

In steps `Bifunctor`.
```purescript
class Bifunctor f where
  bimap :: ∀ a b c d. (a -> c) -> (b -> d) -> f a b -> f c d  
```

Functor only maps 1 of the Type parameters - to map 2 Type parameters we have Bifunctor.
We can now have the following Bifunctor instance for Choice:
```purescript
instance bifunctorChoice :: Bifunctor Choice where
  bimap f _ (PickA x) = PickA $ f x
  bimap _ g (PickB y) = PickB $ g y
```

We no longer have to use Choice a like we did with Functor that’s because Bifunctor’s f takes 2 Parameters and so does Choice - In other words, their Kinds match.

Choice is a `Sum Type` (`Coproduct`), but what about a `Product Type`? Well, we could implement the following for a Tuple:
```purescript
data Tuple a b = Tuple a b

instance bifunctorTuple :: Bifunctor Tuple where
  bimap f g (Tuple x y) = Tuple (f x) (g y)
```

and if we only wanted to map the left side of the Tuple, we could:
```purescript
lmap :: ∀ a b c f. Bifunctor f => (a -> b) -> f a c -> f b c
lmap f = bimap f identity
```

For a Tuple of 3 types we again have to fix a type parameter to match kinds e.g.
```purescript
data Threeple a b c = Threeple a b c

instance bifunctorTuple :: Bifunctor (Threeple a) where
  bimap f g (Threeple x y z) = Threeple x (f y) (g z)
```

Here we fix `a` and so the `x` (of Type `a`) is unaffected by the `bimap` function.