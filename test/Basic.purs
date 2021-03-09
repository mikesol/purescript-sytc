module Basic where

import Prelude
import Data.Newtype (class Newtype)
import Data.Tuple.Nested ((/\))
import Data.Typeclass (class Cons, Typeclass, TypeclassC', TypeclassCons', TypeclassNil', conz, empty, get, uncons, union)
import Effect (Effect)
import Effect.Class.Console (log)
import Type.Proxy (Proxy(..))

newtype ShowMe a
  = ShowMe (a -> String)

derive instance newtypeShowMe :: Newtype (ShowMe a) _

type MyShows
  = TypeclassC' ShowMe (TypeclassCons' Int (TypeclassCons' Boolean TypeclassNil'))

myShows :: Typeclass MyShows
myShows =
  conz
    (ShowMe $ (show :: Int -> String))
    (conz (ShowMe $ \(_ :: Boolean) -> "Fooled you with a fake boolean!") empty)

myShow ::
  forall x head tail.
  Cons x ShowMe head tail MyShows => x -> String
myShow = get (Proxy :: Proxy ShowMe) myShows

yourShows :: Typeclass MyShows
yourShows =
  conz
    (ShowMe $ \(_ :: Int) -> "Fooled you with a fake integer!")
    (conz (ShowMe $ (show :: Boolean -> String)) empty)

yourShow ::
  forall x head tail.
  Cons x ShowMe head tail MyShows => x -> String
yourShow = get (Proxy :: Proxy ShowMe) yourShows

meanShows :: Typeclass MyShows
meanShows =
  let
    _ /\ _ /\ t = uncons (Proxy :: Proxy Int) myShows

    _ /\ h /\ _ = uncons (Proxy :: Proxy Boolean) yourShows
  in
    union h t

meanShow ::
  forall x head tail.
  Cons x ShowMe head tail MyShows => x -> String
meanShow = get (Proxy :: Proxy ShowMe) meanShows

niceShows :: Typeclass MyShows
niceShows =
  let
    _ /\ _ /\ t = uncons (Proxy :: Proxy Int) yourShows

    _ /\ h /\ _ = uncons (Proxy :: Proxy Boolean) myShows
  in
    union h t

niceShow ::
  forall x head tail.
  Cons x ShowMe head tail MyShows => x -> String
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
