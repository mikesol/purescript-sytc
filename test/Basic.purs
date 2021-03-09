module Basic where

import Prelude
import Data.Newtype (class Newtype)
import Data.Tuple.Nested ((/\))
import Data.Typeclass
  ( class Cons
  , Typeclass
  , TNil
  , type (@@)
  , type (@>)
  , (@>)
  , (@-)
  , empty
  , get
  , union
  )
import Effect (Effect)
import Effect.Class.Console (log)
import Type.Proxy (Proxy(..))

newtype ShowMe a
  = ShowMe (a -> String)

derive instance newtypeShowMe :: Newtype (ShowMe a) _

type MyShows
  = ShowMe @@ Int @> Boolean @> TNil

myShows :: Typeclass MyShows
myShows =
  ShowMe (show :: Int -> String)
    @> ShowMe (\(_ :: Boolean) -> "Fooled you with a fake boolean!")
    @> empty

myShow ::
  forall x head tail.
  Cons x ShowMe head tail MyShows => x -> String
myShow = get (Proxy :: Proxy ShowMe) myShows

yourShows :: Typeclass MyShows
yourShows =
  (ShowMe $ \(_ :: Int) -> "Fooled you with a fake integer!")
    @> (ShowMe $ (show :: Boolean -> String))
    @> empty

yourShow ::
  forall x head tail.
  Cons x ShowMe head tail MyShows => x -> String
yourShow = get (Proxy :: Proxy ShowMe) yourShows

meanShows :: Typeclass MyShows
meanShows =
  let
    _ /\ _ /\ t = (Proxy :: Proxy Int) @- myShows

    _ /\ h /\ _ = (Proxy :: Proxy Boolean) @- yourShows
  in
    union h t

meanShow ::
  forall x head tail.
  Cons x ShowMe head tail MyShows => x -> String
meanShow = get (Proxy :: Proxy ShowMe) meanShows

niceShows :: Typeclass MyShows
niceShows =
  let
    _ /\ _ /\ t = (Proxy :: Proxy Int) @- yourShows

    _ /\ h /\ _ = (Proxy :: Proxy Boolean) @- myShows
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
