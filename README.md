# purescript-extensible-typeclass

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

# Extensible type classes

Type class instances can't be modified once they're created. That means that, once `Show Unit` has been defined, we need to define a new type class if we want to override `Show Unit`. In most cases this is fine, but in some cases it won't work:

1. We can't create [orphaned instances](https://github.com/purescript/documentation/blob/master/errors/OrphanInstance.md).
1. We can't use information about type classes to determine the flow of a program. For example, we can't count how many instances they have, determine if they have instances of the same type, etc.
1. We can't pass type classes as arguments to other type classes (also called constraint polymorphism). Meaning you can't do `class k a <= Foo k a` where `k` is a `Constraint` passed to `Foo`.

This library provides a set of tools for working around those limitations.

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

# Constraint polymorphism

This technique makes constraint polymorphism possible. In the example below, `MyShow` acts like a polymorphic constraint that is then extended by `extension`.

```purescript
module ConstraintPolymorphism where

import Prelude
import Data.Newtype (class Newtype)
import Data.Typeclass (using, Typeclass, type (@@), type (@>), TNil, (@>), tnil)
import Effect (Effect)
import Effect.Class.Console (log)

newtype ShowMe a
  = ShowMe (a -> String)

derive instance newtypeShowMe :: Newtype (ShowMe a) _

type BaseShow a
  = ShowMe @@ (Int @> a)

type MyShows a
  = Typeclass (BaseShow a)

myShow :: forall a. Typeclass (ShowMe @@ a) -> MyShows a
myShow a = (ShowMe $ (show :: Int -> String)) @> a

extension :: Typeclass (ShowMe @@ (Boolean @> TNil))
extension = (ShowMe $ (show :: Boolean -> String)) @> tnil

constraintPolymorphism :: Effect Unit
constraintPolymorphism = do
  log $ using (myShow extension) true
  log $ using (myShow tnil) 1
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
