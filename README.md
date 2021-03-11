# purescript-sytc

[Scrap your (PureScript) typeclasses](https://www.haskellforall.com/2012/05/scrap-your-type-classes.html).

# Background

Type classes are ways to define the behavior of a function for certain types. Here's an example:

```purescript
class Show a where
  show :: a -> String

instance showString :: Show String where
  show = identity

instance showInt :: Show Int where
  show i
    | i == 0 = "0"
    | i > 0 = "1 + " <> show (i - 1)
    | i < 0 = "-1 + " <> show (i + 1)
```

Type classes live in a global scope. The compiler stores all the instances as "rows" of a table. So `Show` above would be a table and `showString` would be a row, as would `showInt`. Then, when it finds a value of type `x`, it looks in the table to see if there is an instance (row) of the type class that can handle `x`. If so, it plugs in that instance. If not, it raises a compiler error.

```purescript
prog = do
  log $ show 1 -- ok
  log $ show "hello" -- ok
  log $ show unit -- compiler error because we haven't defined Show Unit
```

# Scrap your type classes

Type classes make a lot of sense when you're working on a library and you don't know who will be using it. In that case, they provide a global namespace so that, if you make an instance of `Show` and that instance gets imported (directly or transitively) into a project, the compiler will use it.

On the other hand, when building applications, we often want composable segments that represent business logic. For example, if we have a complicated record with many fields, we'll usually split it up into smaller records. For example:

```purescript
type FirstName r = (firstName :: String + r)
type LastName r = (lastName :: String + r)
type Person r = (FirstName + LastName + r)
--- and later, on the application layer, we close the record
type SimplePerson = Record (Person + ())
```

This library provides a set of tools for using typeclasses as one would use extensible records. In the example below, `intShow` is composed with `boolShow` much like, in the example above, `FirstName` is composed with `LastName`.

```purescript
module Main where

import Prelude
import Data.Newtype (class Newtype)
import Data.Typeclass (using, Typeclass, type (@@), (<@@>), type (@>), TNil, (@>), tnil)
import Effect (Effect)
import Effect.Class.Console (log)

newtype ShowMe a
  = ShowMe (a -> String)

derive instance newtypeShowMe :: Newtype (ShowMe a) _

type Showable t
  = Typeclass (ShowMe @@ (t @> TNil))

showInt :: Int -> String
showInt i
  | i > 0 = "1 + " <> showInt (i-1)
  | i < 0 = "-1 + " <> showInt (i+1)
  | otherwise = "0"

intShow :: Showable Int
intShow = (ShowMe $ showInt) @> tnil

boolShow :: Showable Boolean
boolShow = (ShowMe $ (if _ then "true" else "false")) @> tnil

main :: Effect Unit
main = do
  log $ using (intShow <@@> boolShow) true
  log $ using (intShow <@@> boolShow) 5
  log $ using intShow (-1)
```

It is similar in some ways to the [scrap your typeclass article from 2012](https://www.haskellforall.com/2012/05/scrap-your-type-classes.html) with the major caveat that it still allows for parametric polymorphism. I think this is a very useful feature that I'm not willing to scrap!

# API

Here's an example showing how type classes can be treated as data that can be composed together.

```purescript
module Basic where

import Prelude
import Data.Newtype (class Newtype)
import Data.Tuple.Nested ((/\))
import Data.Typeclass (type (@>), type (@@), TNil, Typeclass, tnil, using, (<@@>), (@-), (@>))
import Effect (Effect)
import Effect.Class.Console (log)
import Type.Proxy (Proxy(..))

newtype ShowMe a
  = ShowMe (a -> String)

derive instance newtypeShowMe :: Newtype (ShowMe a) _

type Show'
  = ShowMe @@ Int @> Boolean @> TNil

myShow :: Typeclass Show'
myShow =
  ShowMe (show :: Int -> String)
    @> ShowMe (\(_ :: Boolean) -> "Fooled you with a fake boolean!")
    @> tnil

yourShow :: Typeclass Show'
yourShow =
  (ShowMe $ \(_ :: Int) -> "Fooled you with a fake integer!")
    @> (ShowMe $ (show :: Boolean -> String))
    @> tnil

meanShow :: Typeclass Show'
meanShow =
  let
    _ /\ _ /\ t = (Proxy :: Proxy Int) @- myShow

    _ /\ h /\ _ = (Proxy :: Proxy Boolean) @- yourShow
  in
    h <@@> t

niceShow :: Typeclass Show'
niceShow =
  let
    _ /\ _ /\ t = (Proxy :: Proxy Int) @- yourShow

    _ /\ h /\ _ = (Proxy :: Proxy Boolean) @- myShow
  in
    h <@@> t

basic :: Effect Unit
basic = do
  log $ using myShow true
  log $ using myShow 1
  log $ using yourShow true
  log $ using yourShow 1
  log $ using niceShow true
  log $ using niceShow 1
  log $ using meanShow true
  log $ using meanShow 1
```

Produces:

```bash
**** basic
Fooled you with a fake boolean!
1
true
Fooled you with a fake integer!
true
1
Fooled you with a fake boolean!
Fooled you with a fake integer!
```

# Induction

Recursively defined typeclasses can be created with the induction operator `@!>`.

At the typelevel, this takes a type `initial /@\ iterator`, where `/@\` is an infix operator taking the initial value and the iterator. At the term level, it takes a value of type `f (Proxy initial) /\ (forall x. (Proxy x) -> (f (Proxy x)) -> (f (Proxy (iterator x))))`. Check out the example below:

```purescript
module Recursive1 where

import Prelude
import Data.Newtype (class Newtype)
import Data.Tuple.Nested ((/\))
import Data.Typeclass
  ( type (/@\)
  , type (@!>)
  , type (@>)
  , type (@@)
  , TNil
  , Typeclass
  , tnil
  , using
  , (@!>)
  , (@>)
  )
import Effect (Effect)
import Effect.Class.Console (log)
import Type.Proxy (Proxy(..))

data Peano

foreign import data Z :: Peano

foreign import data Succ :: Peano -> Peano

newtype ShowMe a
  = ShowMe (a -> String)

derive instance newtypeShowMe :: Newtype (ShowMe a) _

type Show'
  = ShowMe @@ (Z /@\ Succ) @!> Boolean @> TNil

myShow :: Typeclass Show'
myShow =
  ShowMe (const "Z")
    /\ (\px (ShowMe f) -> ShowMe (const $ "Succ (" <> (f px) <> ")"))
    @!> (ShowMe (\(_ :: Boolean) -> "Fooled you with a fake boolean!"))
    @> tnil

recursive1 :: Effect Unit
recursive1 = do
  log $ using myShow true
  log $ using myShow (Proxy :: Proxy (Succ (Succ (Succ (Succ (Succ Z))))))
  log $ using myShow (Proxy :: Proxy Z)
```

This produces:

```bash
Fooled you with a fake boolean!
Succ (Succ (Succ (Succ (Succ (Z)))))
Z
```

# More examples

More examples can be found in the [tests](./test).
