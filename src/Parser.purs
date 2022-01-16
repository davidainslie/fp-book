-- Applicative Parser
module Parser where

import Control.Plus (class Plus, empty)
import Data.Either (Either(..))
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(..))
import Data.Show.Generic (genericShow)
import Data.String.CodeUnits (fromCharArray, uncons)
import Data.Traversable (class Traversable, sequence)
import Data.Tuple (Tuple(..))
import Data.Unfoldable (class Unfoldable, replicate, none)
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
-}

type ParserState a = Tuple String a -- Our polymorphic parameter "a" is second in case we wish to use Functor for Tuple and Functor only Kind * -> *

class ParserError (e :: Type) where
  eof :: e

type ParseFunction e a = ParserError e => String -> Either e (ParserState a) -- We constrain the ParseFunction to return errors of Type ParserError
{-
When we cannot parse the given String,
we'll return Left e, otherwise
we'll return Right (ParserState a)

e.g. a Parser that parses one of more Numbers given the String:
"123abc"
after calling ParseFunction, we'll have the following return value:
Right (Tuple "abc" 123)
-}

newtype Parser e a = Parser (ParseFunction e a)

{-
A reminder of Functor, Apply and Applicative:

class Functor f where
  map :: ∀ a b. (a -> b) -> f a -> f b

class Functor f <= Apply f where
  apply :: ∀ a b. f (a -> b) -> f a -> f b

infixl 4 apply as <*>

class Apply f <= Applicative f where
  pure :: ∀ a. a -> f a
-}

instance functorParser :: Functor (Parser e) where
  map :: ∀ a b. (a -> b) -> Parser e a -> Parser e b
  -- map fab (Parser fsp) = Parser \s -> map (map fab) (fsp s)      Which equates to
  -- map fab (Parser fsp) = Parser \s -> (<$>) (map fab) (fsp s)    Which equates to
  map fab (Parser fsp) = Parser \s -> map fab <$> (fsp s)

instance applyParser :: Apply (Parser e) where
  apply :: ∀ a b. Parser e (a -> b) -> Parser e a -> Parser e b
  apply (Parser fab) (Parser faa) =
    Parser \s -> case fab s of
        Left err              -> Left err
        Right (Tuple s1 hsp)  -> case faa s1 of
            Left err            -> Left err
            Right (Tuple s2 x)  -> Right $ Tuple s2 (hsp x) -- Bit ugly dealing with 2 Lefts the same, but this will be improved after learning about Monads.

instance applicativeParser :: Applicative (Parser e) where
  pure :: ∀ a. a -> Parser e a
  pure a = Parser \s -> pure $ (Tuple s a)

{-
where
Parser fab :: Parser e (a -> b)
Parser faa :: Parser e a

faa :: String -> Either e (ParserState a)

fab :: String -> Either e (ParserState (a -> b))
-}

{-
Parser e a = Parser (String -> Either e (Tuple String a))
-}
parse :: ∀ e a. ParserError e => Parser e a -> String -> Either e (ParserState a)
parse (Parser fn) = fn

{-
OR
parse :: ∀ e a. ParserError e => Parser e a -> ParseFunction e a
parse (Parser fn) = fn

but since ParseFunction already has a ParserError contraint, we can omit it here:
parse :: ∀ e a. Parser e a -> ParseFunction e a
parse (Parser fn) = fn
-}

{-
Next we need is a way to parse a single character from the String.
This Parser will be the base Parser that all other Parsers will be built upon.
-}
data MyError = MyEOF | InvalidThing

instance parserErrorMyError :: ParserError MyError where
  eof = MyEOF

data ParsingError = NoMoreData | MissingStuff String | IntOutOfRange Int

instance parserErrorParsingError :: ParserError ParsingError where
  eof = NoMoreData

{-
The approach we chose allows the user of our Parser the flexibility to name their errors whatever they want as we’ve done in these 2 preceding examples.
-}

data PError = EOF

derive instance genericPError :: Generic PError _

instance showPError :: Show PError where
  show = genericShow

instance parserErrorPError :: ParserError PError where
  eof = EOF  

char :: ∀ e. Parser e Char
char = Parser \s -> case uncons s of
  Nothing             -> Left eof
  Just { head, tail } -> Right $ Tuple tail head

twoChars :: ∀ e. Parser e (Tuple Char Char)
twoChars = Tuple <$> char <*> char

{-
As a side note, twoChars explicitly would be:
twoChars :: ∀ e. Parser e (ParseFunction e (Tuple Char Char))

which equates to:
twoChars :: ∀ e. Parser e (ParserError e => String -> Either e (ParserState (Tuple Char Char)))

which equates to:
twoChars :: ∀ e. Parser e (ParserError e => String -> Either e (Tuple String (Tuple Char Char)))
-}

{-
An explanation of twoChars:

1st step:
twoChars :: ∀ e. Parser e (Tuple Char Char)
twoChars = char

Results in a Parser with a single Char (and the remainder of a given String).
We want to return this Char as the first part of a Tuple - so let's map Tuple over the Parser:

2nd step:
twoChars :: ∀ e. Parser e (Tuple Char Char)
twoChars = Tuple <$> char

Now we have our Tuple Data Constructor, which is a Function, Partially Applied in a Context, i.e. inside of Parser.
Luckily for us, Parser is an Applicative, which means it’s also an Apply. So we can use <*> to apply the Function in one Parser to result of another Parser:

3rd step:
twoChars :: ∀ e. Parser e (Tuple Char Char)
twoChars = Tuple <$> char <*> char
-}

{-
<*> does all the heavy lifting for us regarding twoChars (and other Parsers):

apply :: ∀ a b. Parser e (a -> b) -> Parser e a -> Parser e b
apply p1 p2 = Parser \s -> case parse p1 s of
  Left err -> Left err
  Right (Tuple s1 h) -> case parse p2 s1 of
    Left err -> Left err
    Right (Tuple s2 x) -> Right $ Tuple s2 (h x)
-}

threeChars :: ∀ e. Parser e (Tuple Char (Tuple Char Char))
threeChars = Tuple <$> char <*> twoChars

-- OR being a tad more explicit:
data Threeple a b c = Threeple a b c

threeChars' :: ∀ e. Parser e (Threeple Char Char Char)
threeChars' = Threeple <$> char <*> char <*> char

-- Another more convenient version of threeChars that produces a String:
threeChars'' :: ∀ e. Parser e String
threeChars'' = fn <$> char <*> char <*> char where
  fn :: Char -> Char -> Char -> String
  fn c1 c2 c3 = fromCharArray [c1, c2, c3]

-- parse' :: ∀ a. Parser PError a -> ParseFunction PError a
parse' :: ∀ a. Parser PError a -> String -> Either PError (ParserState a)
parse' = parse

{-
Remember the Traverable type class:

class (Functor t, Foldable t) <= Traversable t where
  traverse :: ∀ a b m. Applicative m => (a -> m b) -> t a -> m (t b)
  
  sequence :: ∀ a m. Applicative m => t (m a) -> m (t a)
-}

count :: ∀ e a. Int -> Parser e a -> Parser e (Array a)
count n p
  | n <= 0     = pure []
  | otherwise = sequence (replicate n p)

{-
Make "count" be more general i.e. work on Array, List or any Traversable.

replicate :: ∀ f a. Unfoldable f => Int -> a -> f a
-}

count' :: ∀ e a f. Traversable f => Unfoldable f => Int -> Parser e a -> Parser e (f a)
count' n p
  | n <= 0 = pure none
  | otherwise = sequence (replicate n p)

------------------------------------

test :: Effect Unit
test = do
  log $ show $ (parse char "ABC" :: Either PError _)
  log $ show $ (parse twoChars "ABC" :: Either PError _)
  log $ show $ (parse threeChars "ABC" :: Either PError _)
  log "-----------------------------------"
  log $ show $ parse' char "ABC"
  log $ show $ parse' twoChars "ABC"
  log $ show $ parse' threeChars "ABC"
  log "-----------------------------------"
  log $ show $ parse' threeChars'' "A" -- Prints (Left EOF)
  log "-----------------------------------"
  log $ show $ parse' (fromCharArray <$> (count 3 char)) "xyz"