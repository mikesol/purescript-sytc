module Recursive2 where

import Prelude
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype)
import Effect (Effect)
import Effect.Class.Console (log)
import Data.Typeclass (class Cons, Typeclass, TypeclassCons', TypeclassNil', cons, empty, get)
import Type.Proxy (Proxy(..))

data Peano

foreign import data Z :: Peano

foreign import data Succ :: Peano -> Peano

newtype ShowMe a
  = ShowMe (a -> String)

derive instance newtypeShowMe :: Newtype (ShowMe a) _

type BaseShow a
  = TypeclassCons' (Maybe Int) ShowMe (TypeclassCons' a ShowMe (TypeclassCons' Boolean ShowMe TypeclassNil'))

type MyShows a
  = Show a => Proxy a -> Typeclass (BaseShow a)

myShows :: forall a. MyShows a
myShows _ =
  cons
    (Proxy :: Proxy (Maybe Int))
    (ShowMe $ const "Everyone loves a Maybe Int!")
    empty
    ( cons (Proxy :: Proxy a)
        (ShowMe $ show)
        empty
        ( cons (Proxy :: Proxy Boolean)
            (ShowMe $ show)
            empty
            empty
        )
    )

myShow :: forall x head tail. Show x => Cons x ShowMe head tail (BaseShow x) => x -> String
myShow = get (Proxy :: Proxy ShowMe) ((myShows :: MyShows x) (Proxy :: Proxy x))

recursive2 :: Effect Unit
recursive2 = do
  log $ myShow true
  log $ myShow (Just unit)
  log $ myShow (Just 1)
