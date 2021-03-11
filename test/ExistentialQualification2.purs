module ExistentialQualification2 where

import Prelude
import Data.Newtype (class Newtype)
import Data.Typeclass (tnil, using, (@>))
import Effect (Effect)
import Effect.Class.Console (log)

newtype ShowMe a
  = ShowMe (a -> String)

derive instance newtypeShowMe :: Newtype (ShowMe a) _

internal :: forall x. (x -> x) -> Effect Unit
internal y = do
  let
    myShow =
      ShowMe (\(i :: x -> x) -> "Hi identity!")
        @> ShowMe (\i -> "Not " <> (show :: Boolean -> String) (not i))
        @> tnil
  log $ using myShow y
  log $ using myShow false

existentialQualification2 :: Effect Unit
existentialQualification2 = do
  internal identity
