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

> ^D # i.e. control-D to exit REPL
See ya!
()
```