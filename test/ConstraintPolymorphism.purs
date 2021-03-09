module ConstraintPolymorphism where

import Prelude
import Data.Newtype (class Newtype)
import Data.Typeclass (class Cons, Typeclass, TypeclassC', TypeclassCons', TypeclassNil', conz, empty, get)
import Effect (Effect)
import Effect.Class.Console (log)
import Type.Proxy (Proxy(..))

newtype ShowMe a
  = ShowMe (a -> String)

derive instance newtypeShowMe :: Newtype (ShowMe a) _

type BaseShow a
  = TypeclassC' ShowMe (TypeclassCons' Int a)

type MyShows a
  = Typeclass (BaseShow a)

myShow ::
  forall a x head tail.
  Cons x ShowMe head tail (BaseShow a) =>
  Typeclass (TypeclassC' ShowMe a) -> x -> String
myShow a x = get (Proxy :: Proxy ShowMe) (myShows a) x

myShows :: forall a. Typeclass (TypeclassC' ShowMe a) -> MyShows a
myShows a = conz (ShowMe $ (show :: Int -> String)) a

extension :: Typeclass (TypeclassC' ShowMe (TypeclassCons' Boolean TypeclassNil'))
extension = (conz (ShowMe $ (show :: Boolean -> String)) empty)

constraintPolymorphism :: Effect Unit
constraintPolymorphism = do
  log $ myShow extension true
  log $ myShow empty 1
