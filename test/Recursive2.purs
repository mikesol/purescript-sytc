module Recursive2 where

import Prelude
import Data.Maybe (Maybe(..), maybe)
import Data.Newtype (class Newtype)
import Data.Typeclass (Typeclass, TypeclassC', TypeclassCons', TypeclassNil', TypeclassSingleton', cons, conz, tnil, using_)
import Effect (Effect)
import Effect.Class.Console (log)

newtype ShowMe a
  = ShowMe (a -> String)

derive instance newtypeShowMe :: Newtype (ShowMe a) _

type BaseShow
  = TypeclassC' ShowMe (TypeclassCons' (TypeclassSingleton' (Maybe Int)) (TypeclassCons' (TypeclassSingleton' Int) (TypeclassCons' (TypeclassSingleton' Boolean) TypeclassNil')))

type MyShows
  = Typeclass BaseShow

myShow :: Unit -> MyShows
myShow _ =
  conz
    ( ShowMe
        ( maybe "Nothing"
            -- for recursion
            (\x -> append "One less than Maybe " $ using_ myShow $ (_ + 1) x)
        )
    )
    ( conz (ShowMe $ (show :: Int -> String))
        ( conz
            (ShowMe $ (show :: Boolean -> String))
            tnil
        )
    )

recursive2 :: Effect Unit
recursive2 = do
  log $ using_ myShow true

--log $ using_ myShow (Just 42)
--log $ using_ myShow 1
