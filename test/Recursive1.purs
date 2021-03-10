module Recursive1 where

import Prelude
import Data.Newtype (class Newtype)
import Data.Tuple.Nested ((/\))
import Data.Typeclass (type (/@\), type (@!>), type (@>), type (@@), TNil, Typeclass, tnil, using, (@!>), (@>))
import Effect (Effect)
import Effect.Class.Console (log)
import Type.Proxy (Proxy(..))

data Peano

foreign import data Z :: Peano

foreign import data Succ :: Peano -> Peano

newtype ShowMe a
  = ShowMe (a -> String)

derive instance newtypeShowMe :: Newtype (ShowMe a) _

type Show'
  = ShowMe @@ (Z /@\ Succ) @!> Boolean @> TNil

myShow :: Typeclass Show'
myShow =
  ShowMe
    (const "Z")
    /\ (\px (ShowMe f) -> ShowMe (const $ "Succ (" <> (f px) <> ")"))
    @!> (ShowMe (\(_ :: Boolean) -> "Fooled you with a fake boolean!"))
    @> tnil

recursive1 :: Effect Unit
recursive1 = do
  log $ using myShow true
  log $ using myShow (Proxy :: Proxy (Succ (Succ (Succ (Succ (Succ Z))))))
  log $ using myShow (Proxy :: Proxy Z)
