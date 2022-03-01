{-
Welcome to a Spago project! You can edit this file as you like.
Upon changing run:
npx spago build
-}
{
  name = "my-project",
  packages = ./packages.dhall,
  sources = [ "src/**/*.purs", "test/**/*.purs" ],
  dependencies = [
    "console",
    "effect",
    "lists",
    "prelude",
    "psci-support",
    "maybe",
    "either",
    "tuples",
    "arrays",
    "ordered-collections",
    "newtype",
    "strings",
    "integers",
    "nonempty",
    "foldable-traversable",
    "bifunctors",
    "contravariant",
    "profunctor",
    "identity",
    "control",
    "unfoldable",
    "unicode",
    "control"
  ]
}
