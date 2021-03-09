module ConstraintPolymorphism where

import Prelude
import Data.Newtype (class Newtype)
import Data.Typeclass (class Cons, Typeclass, type (@@), type (@>), TNil, (@>), tnil, get)
import Effect (Effect)
import Effect.Class.Console (log)
import Type.Proxy (Proxy(..))

newtype ShowMe a
  = ShowMe (a -> String)

derive instance newtypeShowMe :: Newtype (ShowMe a) _

type BaseShow a
  = ShowMe @@ (Int @> a)

type MyShows a
  = Typeclass (BaseShow a)

myShow ::
  forall a x head tail.
  Cons x ShowMe head tail (BaseShow a) =>
  Typeclass (ShowMe @@ a) -> x -> String
myShow a x = get (Proxy :: Proxy ShowMe) (myShows a) x

myShows :: forall a. Typeclass (ShowMe @@ a) -> MyShows a
myShows a = (ShowMe $ (show :: Int -> String)) @> a

extension :: Typeclass (ShowMe @@ (Boolean @> TNil))
extension = (ShowMe $ (show :: Boolean -> String)) @> tnil

constraintPolymorphism :: Effect Unit
constraintPolymorphism = do
  log $ myShow extension true
  log $ myShow tnil 1
