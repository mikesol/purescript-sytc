module Polymorphism where

import Prelude
import Data.Newtype (class Newtype)
import Data.Typeclass (type (@>), type (@@), TNil, Typeclass, tnil, using, (@>))
import Effect (Effect)
import Effect.Class.Console (log)

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

doShow :: forall x. Typeclass (ShowMe @@ x @> Boolean @> TNil) -> x -> String
doShow tc x = using tc x

polymorphism :: Effect Unit
polymorphism = do
  log $ doShow myShow 1
  log $ using myShow true
