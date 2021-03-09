module Recursive2 where

import Prelude
import Data.Maybe (Maybe(..), maybe)
import Data.Newtype (class Newtype)
import Data.Typeclass (class Cons, Typeclass, TypeclassC', TypeclassCons', TypeclassNil', cons, tnil, get)
import Effect (Effect)
import Effect.Class.Console (log)
import Type.Proxy (Proxy(..))

newtype ShowMe a
  = ShowMe (a -> String)

derive instance newtypeShowMe :: Newtype (ShowMe a) _

type BaseShow
  = TypeclassC' ShowMe (TypeclassCons' (Maybe Int) (TypeclassCons' Int (TypeclassCons' Boolean TypeclassNil')))

type MyShows
  = Typeclass BaseShow

myShow :: forall x head tail. Cons x ShowMe head tail BaseShow => x -> String
myShow x = get (Proxy :: Proxy ShowMe) (myShows unit) x

myShows :: Unit -> MyShows
myShows _ =
  cons
    ( ShowMe
        ( maybe "Nothing"
            (append "One less than Maybe " <<< myShow <<< (_ + 1))
        )
    )
    tnil
    ( cons (ShowMe $ (show :: Int -> String))
        tnil
        ( cons
            (ShowMe $ (show :: Boolean -> String))
            tnil
            tnil
        )
    )

recursive2 :: Effect Unit
recursive2 = do
  log $ myShow true
  log $ myShow (Just 42)
  log $ myShow 1
