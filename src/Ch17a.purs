-- Applicative Parser
module Ch17a where

import Effect (Effect)
import Effect.Console (log)
import Prelude

{-
"10/2/1962"
 
Regex to parse above date:
\d{1,2}\/\d{1,2}\/\d{4}

We have 3 unique "mini-parsers":
\d{1,2}   -- digits 1 or 2
\/        -- forward slash (escaped since it's a special character)
\d{4}     -- digits 4

In typical Functional fashion, it would be nice to build Parsers for each of these and then combine them somehow to finish the whole job.
In Functional Programming, we combine Functions by composing them where the output of one Function is the input to another.

So if we’re going to combine Parsers to build larger Parsers, each Parser will need to whittle away at our String, doing their part, until we fail or success to parse.

That means that the Parsing State is going to need to be passed from Parser to Parser,
i.e. when the current Parser is done, it passes what’s left of the String to the next Parser who takes a stab at parsing what’s left.

Also, if a single Parser in the chain were to fail, we want to short-circuit the parsing and return the error, hopefully with some useful information as to what went wrong.

In this scenario, we’ll want each Parser to return an Int. But not every Parser is going to parse numbers into an Int.
So, we’ll want our Parser to return the Polymorphic Parameter, a. And there are no constraints on a. a can be any Type.

The other thing we’ll need is the String to parse. We will consume what we need from that String and return what’s left over.

See Parser.purs
-}

------------------------------------

test :: Effect Unit
test = do
  log "test"