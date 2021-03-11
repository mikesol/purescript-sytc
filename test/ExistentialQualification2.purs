module ExistentialQualification2 where

import Prelude
import Data.Newtype (class Newtype)
import Data.Typeclass (tnil, using, (@>))
import Effect (Effect)
import Effect.Class.Console (log)

newtype ShowMe a
  = ShowMe (a -> String)

derive instance newtypeShowMe :: Newtype (ShowMe a) _

existentialQualification2 :: forall x. Effect Unit
existentialQualification2 = do
  let
    myShow =
      ShowMe (\(i :: x -> x) ->  "Yo! Neda!")
        @> ShowMe (\i -> "Not " <> (show :: Boolean -> String) (not i))
        @> tnil
  log $ using myShow (\(x :: x) -> x)
  log $ using myShow false
