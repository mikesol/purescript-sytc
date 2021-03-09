module Recursive1 where

import Prelude

import Data.Newtype (class Newtype)
import Data.Tuple.Nested ((/\))
import Data.Typeclass (class Cons, Typeclass, TypeclassC', TypeclassCons', TypeclassNil', TypeclassSingleton', cons, tnil, uncons, using)
import Effect (Effect)
import Effect.Class.Console (log)
import Type.Proxy (Proxy(..))

data Peano

foreign import data Z :: Peano

foreign import data Succ :: Peano -> Peano

newtype ShowMe a
  = ShowMe (a -> String)

derive instance newtypeShowMe :: Newtype (ShowMe a) _

class ShowPeano (p :: Peano) where
  showPeano :: Proxy p -> String

instance showPeanoZ :: ShowPeano Z where
  showPeano _ = "Z"

instance showPeanoSucc :: ShowPeano x => ShowPeano (Succ x) where
  showPeano _ = "Succ ( " <> (showPeano (Proxy :: Proxy x)) <> ")"

class ShowPeanoAlt (p :: Peano) where
  showPeanoAlt :: Proxy p -> String

instance showPeanoAltZ :: ShowPeanoAlt Z where
  showPeanoAlt _ = "z"

instance showPeanoAltSucc :: ShowPeanoAlt x => ShowPeanoAlt (Succ x) where
  showPeanoAlt _ = "succ ( " <> (showPeanoAlt (Proxy :: Proxy x)) <> ")"

class AsPeano :: forall k. k -> Peano -> Constraint
class AsPeano x (y :: Peano) | x -> y where
  asPeano :: Proxy x -> Proxy y

instance asPeanoSucc :: AsPeano (Proxy (Succ x)) (Succ x) where
  asPeano _ = Proxy :: Proxy (Succ x)
else instance asPeanoZ :: AsPeano (Proxy Z) Z where
  asPeano _ = Proxy :: Proxy Z
else instance asPeanoX :: AsPeano x Z where
  asPeano _ = Proxy :: Proxy Z

type BaseShow (p :: Peano) = TypeclassC' ShowMe (TypeclassCons' (TypeclassSingleton' (Proxy p)) (TypeclassCons' (TypeclassSingleton' Boolean) TypeclassNil'))

type MyShows (p :: Peano) = ShowPeano p => Proxy p -> Typeclass (BaseShow p)

myShows :: forall (p :: Peano). MyShows p
myShows _ =
  cons
    (ShowMe (showPeano :: Proxy p -> String))
    tnil
    (cons (ShowMe $ \(_ :: Boolean) -> "Fooled you with a fake boolean!") tnil tnil)

type YourShows (p :: Peano) = ShowPeano p => ShowPeanoAlt p => Proxy p -> Typeclass (TypeclassC' ShowMe (TypeclassCons' (TypeclassSingleton' (Proxy p) )(TypeclassCons' (TypeclassSingleton' Boolean) TypeclassNil')))

yourShows :: forall (p :: Peano). YourShows p
yourShows _ =
  let
    _ /\ _ /\ t = uncons (Proxy :: Proxy (Proxy p)) (myShows (Proxy :: Proxy p))
  in
    cons (ShowMe (showPeanoAlt :: Proxy p -> String)) tnil t

myShow :: forall (p :: Peano) x head tail. AsPeano x p => ShowPeano p => Cons x ShowMe head tail (BaseShow p) => x -> String
myShow = using ((myShows :: MyShows p) (asPeano (Proxy :: Proxy x)))

yourShow :: forall (p :: Peano) x head tail. AsPeano x p => ShowPeano p => ShowPeanoAlt p => Cons x ShowMe head tail (BaseShow p) => x -> String
yourShow = using ((yourShows :: YourShows p) (asPeano (Proxy :: Proxy x)))

recursive1 :: Effect Unit
recursive1 = do
  log $ myShow true
  log $ myShow (Proxy :: Proxy Z)
  log $ myShow (Proxy :: Proxy (Succ Z))
  log $ yourShow true
  log $ yourShow (Proxy :: Proxy Z)
  log $ yourShow (Proxy :: Proxy (Succ Z))
  pure unit
