module ShowOverride where

import Prelude

import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype)
import Data.Typeclass (class Cons, Typeclass, Typeclass', TypeclassC', TypeclassCons', TypeclassNil', cons, tnil, using)
import Effect (Effect)
import Effect.Class.Console (log)
import Type.Proxy (Proxy(..))

newtype ShowMe a
  = ShowMe (a -> String)

derive instance newtypeShowMe :: Newtype (ShowMe a) _

type BaseShow :: forall k. k -> Typeclass'
type BaseShow a
  = TypeclassC' ShowMe (TypeclassCons' (Maybe Int) (TypeclassCons' a (TypeclassCons' Boolean TypeclassNil')))

type MyShows a
  = Show a => Proxy a -> Typeclass (BaseShow a)

myShows :: forall a. MyShows a
myShows _ =
  cons
    (ShowMe $ \(_ :: Maybe Int) -> "Everyone loves a Maybe Int!")
    tnil
    ( cons
        (ShowMe $ (show :: a -> String))
        tnil
        ( cons
            (ShowMe $ (show :: Boolean -> String))
            tnil
            tnil
        )
    )

myShow :: forall x head tail. Show x => Cons x ShowMe head tail (BaseShow x) => x -> String
myShow = using ((myShows :: MyShows x) (Proxy :: Proxy x))

showOverride :: Effect Unit
showOverride = do
  log $ myShow true
  log $ myShow (Just unit)
  log $ myShow (Just 1)
