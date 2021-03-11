module ExistentialQualification2 where

import Prelude
import Data.Newtype (class Newtype)
import Data.Typeclass (tnil, using, (@>))
import Effect (Effect)
import Effect.Class.Console (log)

newtype ShowMe a
  = ShowMe (a -> String)

derive instance newtypeShowMe :: Newtype (ShowMe a) _

type Identity x
  = x -> x

internal :: forall x. Identity x -> Effect Unit
internal y = do
  let
    myShow =
      ShowMe (\(i :: Identity x) -> "Hi identity!")
        @> ShowMe (\i -> "Not " <> (show :: Boolean -> String) (not i))
        @> tnil
  log $ using myShow y
  log $ using myShow false

existentialQualification2 :: Effect Unit
existentialQualification2 = do
  internal identity
