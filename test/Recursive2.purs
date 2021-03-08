module Recursive2 where

import Prelude
import Data.Maybe (Maybe(..), maybe)
import Data.Newtype (class Newtype)
import Data.Typeclass (class Cons, Typeclass, TypeclassCons', TypeclassNil', cons, empty, get)
import Effect (Effect)
import Effect.Class.Console (log)
import Type.Proxy (Proxy(..))

newtype ShowMe a
  = ShowMe (a -> String)

derive instance newtypeShowMe :: Newtype (ShowMe a) _

type BaseShow
  = TypeclassCons' (Maybe Int) ShowMe (TypeclassCons' Int ShowMe (TypeclassCons' Boolean ShowMe TypeclassNil'))

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
    empty
    ( cons (ShowMe $ (show :: Int -> String))
        empty
        ( cons
            (ShowMe $ (show :: Boolean -> String))
            empty
            empty
        )
    )

recursive2 :: Effect Unit
recursive2 = do
  log $ myShow true
  log $ myShow (Just 42)
  log $ myShow 1
