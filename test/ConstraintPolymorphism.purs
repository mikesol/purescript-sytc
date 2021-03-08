module ConstraintPolymorphism where

import Prelude
import Data.Newtype (class Newtype)
import Data.Typeclass
  ( class Cons
  , class HomogeneousOp'
  , Typeclass
  , TypeclassCons'
  , TypeclassNil'
  , cons
  , empty
  , get
  )
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

myShow ::
  forall a x head tail.
  HomogeneousOp' ShowMe a =>
  Cons x ShowMe head tail (BaseShow a) =>
  Typeclass a -> x -> String
myShow a x = get (Proxy :: Proxy ShowMe) (myShows a) x

myShows ::
  forall a.
  HomogeneousOp' ShowMe a =>
  Typeclass a -> MyShows a
myShows a = cons (ShowMe $ (show :: Int -> String)) empty a

extension :: Typeclass (TypeclassCons' Boolean ShowMe TypeclassNil')
extension = (cons (ShowMe $ (show :: Boolean -> String)) empty empty)

constraintPolymorphism :: Effect Unit
constraintPolymorphism = do
  log $ myShow extension true
  log $ myShow empty 1
