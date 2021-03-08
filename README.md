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

Type classes can't be modified once they're created. That means that, once `Show Unit` has been defined, we need to define a new type class if we want to override `Show Unit`. In most cases this is fine, but in some cases it won't work:

1. We can't create [orphaned instances](https://github.com/purescript/documentation/blob/master/errors/OrphanInstance.md).
1. We can't use information about type classes to determine the flow of a program. For example, we can't count how many instances they have, determine if they have instances of the same type, etc.
1. As of now in PureScript (though this may change), we can't pass type classes as arguments to other type classes (also called constraint polymorphism). Meaning you can't do `class k a <= Foo k a` where `k` is a `Constraint` passed to `Foo`.

This library provides a set of tools for working around those limitations.

# API

Here's an example showing how type classes can be treated as data that can be composed together.

```purescript
module Basic where

import Prelude
import Data.Newtype (class Newtype)
import Data.Tuple.Nested ((/\))
import Data.Typeclass (class Cons, Typeclass, TypeclassCons', TypeclassNil', cons, empty, uncons, union, get)
import Effect (Effect)
import Effect.Class.Console (log)
import Type.Proxy (Proxy(..))

newtype ShowMe a
  = ShowMe (a -> String)

derive instance newtypeShowMe :: Newtype (ShowMe a) _

type MyShows
  = TypeclassCons' Int ShowMe (TypeclassCons' Boolean ShowMe TypeclassNil')

myShows :: Typeclass MyShows
myShows = cons (Proxy :: Proxy Int) (ShowMe $ show) empty (cons (Proxy :: Proxy Boolean) (ShowMe $ const "Fooled you with a fake boolean!") empty empty)

myShow :: forall x head tail. Cons x ShowMe head tail MyShows => x -> String
myShow = get (Proxy :: Proxy ShowMe) myShows

yourShows :: Typeclass MyShows
yourShows = cons (Proxy :: Proxy Int) (ShowMe $ const "Fooled you with a fake integer!") empty (cons (Proxy :: Proxy Boolean) (ShowMe $ show) empty empty)

yourShow :: forall x head tail. Cons x ShowMe head tail MyShows => x -> String
yourShow = get (Proxy :: Proxy ShowMe) yourShows

meanShows :: Typeclass MyShows
meanShows =
  let
    _ /\ _ /\ t = uncons (Proxy :: Proxy Int) myShows

    _ /\ h /\ _ = uncons (Proxy :: Proxy Boolean) yourShows
  in
    union h t

meanShow :: forall x head tail. Cons x ShowMe head tail MyShows => x -> String
meanShow = get (Proxy :: Proxy ShowMe) meanShows

niceShows :: Typeclass MyShows
niceShows =
  let
    _ /\ _ /\ t = uncons (Proxy :: Proxy Int) yourShows

    _ /\ h /\ _ = uncons (Proxy :: Proxy Boolean) myShows
  in
    union h t

niceShow :: forall x head tail. Cons x ShowMe head tail MyShows => x -> String
niceShow = get (Proxy :: Proxy ShowMe) niceShows

basic :: Effect Unit
basic = do
  log $ myShow true
  log $ myShow 1
  log $ yourShow true
  log $ yourShow 1
  log $ niceShow true
  log $ niceShow 1
  log $ meanShow true
  log $ meanShow 1
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

```purescript
module ConstraintPolymorphism where

import Prelude
import Data.Newtype (class Newtype)
import Data.Typeclass (class Cons, class HomogeneousOp', Typeclass, TypeclassCons', TypeclassNil', cons, empty, get)
import Effect (Effect)
import Effect.Class.Console (log)
import Type.Proxy (Proxy(..))

newtype ShowMe a
  = ShowMe (a -> String)

derive instance newtypeShowMe :: Newtype (ShowMe a) _

type BaseShow a
  = TypeclassCons' Int ShowMe a

type MyShows a
  = Typeclass (BaseShow a)

myShow :: forall a x head tail. HomogeneousOp' ShowMe a => Cons x ShowMe head tail (BaseShow a) => Typeclass a -> x -> String
myShow a x = get (Proxy :: Proxy ShowMe) (myShows a) x

myShows :: forall a. HomogeneousOp' ShowMe a => Typeclass a -> MyShows a
myShows a = cons (Proxy :: Proxy Int) (ShowMe $ show) empty a

extension :: Typeclass (TypeclassCons' Boolean ShowMe TypeclassNil')
extension = (cons (Proxy :: Proxy Boolean) (ShowMe $ show) empty empty)

constraintPolymorphism :: Effect Unit
constraintPolymorphism = do
  log $ myShow extension true
  log $ myShow empty 1
```