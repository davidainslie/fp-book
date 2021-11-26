# FP Made Easier with PureScript

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

## Category Theory (Scratching the Surface)

Definition of a Category:
> A Category consists of a Set of Objects and a Set of Morphisms between the Objects.

We get to pick what our Objects and Morphisms are, which depends on what we’re trying to model. Since our interest is Programming, we’ll pick our Objects as Types and our Morphisms as Functions.
This Category is traditionally called Hask after Haskell. But works just fine for PureScript.

And as always in Math, there are laws:
> • Each Morphism f has a Source Object a and Target Object b and is written like f: a → b and we say "f is a morphism from a to b"
> • Morphisms compose
> • There is an Identity Morphism for every Object, i.e. 1<sub>x</sub>: x → x
> • Composition is Associative: if f: a -> b, g: b -> c, and h: c -> d then h ∘ (g ∘ f) = (h ∘ g) ∘ f