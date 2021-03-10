module Test.Functor where

import Prelude
import Data.Maybe (Maybe(..), maybe)
import Data.Newtype (class Newtype)
import Data.Typeclass (type (@>), type (@@), TNil, Typeclass, tnil, using'', using_, (@>))
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
        (\x -> using_ myShow $ (_ + 1) x)
    )
    @> (ShowMe $ (show :: Int -> String))
    @> (ShowMe $ (show :: Boolean -> String))
    @> tnil

newtype Funktor f
  = Funktor (forall a b. (a -> b) -> f a -> f b)

type Funktor'
  = Funktor @@ Maybe @> TNil

myMap :: Typeclass Funktor'
myMap =
  Funktor
    ( \f -> case _ of
        Nothing -> Nothing
        Just x -> Just $ f x
    )
    @> tnil

functor :: Effect Unit
functor = do
  let
    usingf = using'' \(Funktor x) -> x
  log $ (using_ myShow (usingf myMap ((+) 1) (Just 3)))
  log $ (using_ myShow (usingf myMap ((+) 1) (Nothing :: Maybe Int)))
