module Basic where

import Prelude
import Data.Newtype (class Newtype)
import Data.Tuple.Nested ((/\))
import Data.Typeclass (type (@>), type (@@), TNil, Typeclass, tnil, using, (<@@>), (-@-), (@>))
import Effect (Effect)
import Effect.Class.Console (log)
import Type.Proxy (Proxy(..))

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

yourShow :: Typeclass Show'
yourShow =
  (ShowMe $ \(_ :: Int) -> "Fooled you with a fake integer!")
    @> (ShowMe $ (show :: Boolean -> String))
    @> tnil

meanShow :: Typeclass Show'
meanShow =
  let
    _ /\ _ /\ t = (Proxy :: Proxy Int) -@- myShow

    _ /\ h /\ _ = (Proxy :: Proxy Boolean) -@- yourShow
  in
    h <@@> t

niceShow :: Typeclass Show'
niceShow =
  let
    _ /\ _ /\ t = (Proxy :: Proxy Int) -@- yourShow

    _ /\ h /\ _ = (Proxy :: Proxy Boolean) -@- myShow
  in
    h <@@> t

basic :: Effect Unit
basic = do
  log $ using myShow true
  log $ using myShow 1
  log $ using yourShow true
  log $ using yourShow 1
  log $ using niceShow true
  log $ using niceShow 1
  log $ using meanShow true
  log $ using meanShow 1
