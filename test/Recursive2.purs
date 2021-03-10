module Recursive2 where

import Prelude
import Data.Maybe (Maybe(..), maybe)
import Data.Newtype (class Newtype)
import Data.Typeclass (Typeclass, type (@>), type (@@), TNil, (@>), tnil, using_)
import Effect (Effect)
import Effect.Class.Console (log)

newtype ShowMe a
  = ShowMe (a -> String)

derive instance newtypeShowMe :: Newtype (ShowMe a) _

type BaseShow
  = ShowMe @@ Maybe Int @> Int @> Boolean @> TNil

type MyShows
  = Typeclass BaseShow

myShow :: Unit -> MyShows
myShow _ =
  ShowMe
    ( maybe "Nothing"
        -- function needed to avoid stack overflow in 0.14
        (\x -> append "One less than Maybe " $ using_ myShow $ (_ + 1) x)
    )
    @> (ShowMe $ (show :: Int -> String))
    @> (ShowMe $ (show :: Boolean -> String))
    @> tnil

recursive2 :: Effect Unit
recursive2 = do
  log $ using_ myShow true
  log $ using_ myShow (Just 42)
  log $ using_ myShow 1
