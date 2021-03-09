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
