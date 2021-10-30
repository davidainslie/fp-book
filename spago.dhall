{-
Welcome to a Spago project! You can edit this file as you like.
Upon changing run: npx spago build
-}
{
  name = "my-project",
  dependencies = [ "console", "effect", "lists", "prelude", "psci-support", "maybe", "tuples", "arrays", "ordered-collections", "newtype", "strings", "integers" ],
  packages = ./packages.dhall,
  sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
