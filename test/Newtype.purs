module Test.Functor where

import Prelude
import Data.Maybe (Maybe(..), maybe)
import Data.Newtype (class Newtype)
import Data.Typeclass (type (@>), type (@@), TNil, Typeclass, tnil, using, using'', using_, (@>))
import Effect (Effect)
import Effect.Class.Console (log)

newtype ShowMe a
  = ShowMe (a -> String)

newtype Newertype f
  = Newertype (forall y a. f (y a) -> y a)

type Newertype'
  = Newertype @@ ShowMe @> Newertype @> TNil

myUnwrap :: Typeclass Newertype'
myUnwrap =
  using'' (\(Newertype f) -> f)
    ( Newertype
        (\(ShowMe f) -> f)
        (\(Newertype f) -> f)
        @> tnil
    )

functor :: Effect Unit
functor = do
  log $ myUnwrap (ShowMe (show :: Int -> String)) 5
